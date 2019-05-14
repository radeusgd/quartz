{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Runtime where

import Prelude hiding (mod, exp)

import Passes.TypeCheck
import AST.Desugared
import Builtins
import Interpreter

import System.Directory
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Quartz.Syntax.ErrM
import AppCommon

import Debug.Trace

orElse :: MonadError e1 m => (Either e2 a) -> (e2 -> e1) -> m a
orElse (Left e) f = throwError $ f e
orElse (Right a) _ = return a

moduleSearchDirectories :: IO [FilePath]
moduleSearchDirectories = return [".", "./stdlib/"] -- TODO add env, detect stdlib better

findModule :: MonadError RuntimeError m => MonadIO m => String -> m String
findModule mod = do
  dirs <- liftIO $ moduleSearchDirectories
  f <- liftIO $ findFile dirs (mod ++ ".quartz")
  case f of
    Nothing -> throwError $ RuntimeError $ ("Cannot find module " ++ mod)
    Just path -> return path

readModule :: MonadError RuntimeError m => MonadIO m => String -> m ([Ident], [Declaration])
readModule path = do
  parsed <- liftIO $ parseFile path
  case parsed of
    Bad err -> throwError $ RuntimeError $ path ++ " Syntax error: " ++ err
    Ok res -> return res

data RState = RState { rsTypeEnv :: TypeEnv, rsVarEnv :: Env, rsMem :: Memory, rsInitialTypeEnv :: TypeEnv, rsInitialEnv :: Env }
-- class MonadRuntime m
-- instance (MonadError RuntimeError m, MonadState RState m, MonadIO m) => MonadRuntime m
data RuntimeError = RuntimeError String
instance Show RuntimeError where
  show (RuntimeError s) = "Error: " ++ s

makeInitialState :: MonadError RuntimeError m => MonadIO m => m RState
makeInitialState = do
  builtins <- liftIO $ loadBuiltinDecls
  tenv <- case evalInfer $ extendEnvironment (Just builtinsModuleName) emptyTypeEnv builtins of
               Right env -> return env
               Left e -> throwError $ RuntimeError $ "Fatal error, cannot typecheck builtins: " ++ show e
  res <- liftIO $ execInterpreter emptyEnv emptyMemory builtinsEnv
  case res of
    Right (env, mem) ->
      return $ RState tenv env mem tenv env
    Left e -> throwError $ RuntimeError $ "Fatal error, cannot initialize builtins: " ++ e

execInterpreterInRuntime :: MonadError RuntimeError m => MonadState RState m => MonadIO m => Interpreter a -> m a
execInterpreterInRuntime i = do
  RState _ env mem _ _ <- get
  res <- liftIO $ execInterpreter env mem i
  case res of
    Left err -> throwError $ RuntimeError $ "Runtime error: " ++ err
    Right (e, mem') -> do
        modify (\rs -> rs { rsMem = mem' })
        return e

importModule :: (MonadError RuntimeError m, MonadState RState m, MonadIO m) => String -> (TypeEnv, Env) -> m (TypeEnv, Env)
importModule path (tenv, env) = do
  tenv0 <- gets rsInitialTypeEnv
  env0 <- gets rsInitialEnv
  let mod = Just $ moduleName path
  (imports, decls) <- readModule path
  paths <- mapM findModule imports
  (tenv1, env1) <- foldM (flip importModule) (tenv0, env0) paths
  mem <- gets rsMem
  tenv' <- orElse
    (evalInfer $ withEnvironment tenv1 $ inContext (moduleName path) $ extendEnvironment mod tenv decls)
    (\err -> RuntimeError $ path ++ " Type error: " ++ show err)
  exec <- liftIO $ execInterpreter env1 mem (processDefinitions mod env decls)
  (env', mem') <- orElse exec (\err -> RuntimeError $ path ++ " Initialization runtime error: " ++ show err)
  modify $ \rs -> rs { rsMem = mem' }
  return (tenv', env')

introduceModule :: MonadError RuntimeError m => MonadState RState m => MonadIO m => String -> m ()
introduceModule path = do
  tenv <- gets rsTypeEnv
  env <- gets rsVarEnv
  (tenv', env') <- importModule path (tenv, env)
  modify $ \rs -> rs { rsTypeEnv = tenv', rsVarEnv = env' }

introduceDeclaration :: MonadError RuntimeError m => MonadState RState m => MonadIO m => Declaration -> m Ident
introduceDeclaration decl = do
  RState tenv env mem _ _ <- get
  case evalInfer $ withEnvironment tenv $ extendEnvironment Nothing tenv [decl] of
    Left err -> throwError $ RuntimeError $ "Type error: " ++ show err
    Right tenv' -> do
      res <- liftIO $ execInterpreter env mem (processDefinition Nothing env decl)
      case res of
        Left err -> throwError $ RuntimeError $ "Runtime error: " ++ err
        Right (env', mem') -> do
          modify (\rs -> rs { rsTypeEnv = tenv', rsVarEnv = env', rsMem = mem' })
          return $ declarationName decl

evalExp :: MonadError RuntimeError m => MonadState RState m => MonadIO m => Exp -> m Value
evalExp exp = do
  RState tenv env mem _ _ <- get
  case evalInfer $ withEnvironment tenv $ inferExpType exp of
    Left err -> throwError $ RuntimeError $ "Type error: " ++ show err
    Right _ -> do
      execInterpreterInRuntime (interpret exp >>= force)

showExp :: MonadError RuntimeError m => MonadState RState m => MonadIO m => Exp -> m String
showExp exp = do
  v <- evalExp exp
  execInterpreterInRuntime (ishow RunIO v)
