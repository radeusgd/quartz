{-# LANGUAGE FlexibleContexts #-}

module Runtime where

import Passes.TypeCheck
import AST.Desugared
import Builtins
import Interpreter

import System.Console.Repline
import System.Directory
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List
import qualified ModuleMap as MM
import qualified Data.Map as Map
import Quartz.Syntax.ErrM
import AppCommon

moduleSearchDirectories :: IO [FilePath]
moduleSearchDirectories = return [".", "./stdlib/"] -- TODO add env, detect stdlib better

findModule :: String -> IO (Either String String)
findModule mod = do
  dirs <- moduleSearchDirectories
  maybeToRight ("Cannot find module " ++ mod) <$> findFile dirs (mod ++ ".quartz")
  where
    maybeToRight l Nothing = Left l
    maybeToRight _ (Just x) = Right x

data RState = RState { rsTypeEnv :: TypeEnv, rsVarEnv :: Env, rsMem :: Memory }

data RuntimeError = RuntimeError String

makeInitialState :: ExceptT RuntimeError IO RState
makeInitialState = do
  builtins <- liftIO $ loadBuiltinDecls
  tenv <- case evalInfer $ extendEnvironment (Just builtinsModuleName) emptyTypeEnv builtins of
               Right env -> return env
               Left e -> throwError $ RuntimeError $ "Fatal error, cannot typecheck builtins: " ++ show e
  res <- liftIO $ execInterpreter emptyEnv emptyMemory builtinsEnv
  case res of
    Right (env, mem) ->
      return $ RState tenv env mem
    Left e -> error $ "Fatal error, cannot initialize builtins: " ++ e

execInterpreterInRuntime :: MonadState RState m => MonadIO m => Interpreter a -> m (Either RuntimeError a)
execInterpreterInRuntime i = do
  RState tenv env mem <- get
  res <- liftIO $ execInterpreter env mem i
  case res of
    Left err -> return $ Left $ RuntimeError $ "Runtime error: " ++ err
    Right (e, mem') -> do
        modify (\_ -> RState tenv env mem')
        return $ Right e

introduceModule :: MonadState RState m => MonadIO m => String -> m (Either RuntimeError ())
introduceModule mod = do
  mpath <- liftIO $ findModule mod
  case mpath of
    Left e -> return $ Left $ RuntimeError e
    Right path -> do
      parsed <- liftIO $ parseFile path
      case parsed of
        Bad err -> return $ Left $ RuntimeError $ "Syntax error: " ++ err
        Ok (imports, decls) -> do
          -- TODO imports
          Right <$> mapM_ introduceDeclaration decls

introduceDeclaration :: MonadState RState m => MonadIO m => Declaration -> m (Either RuntimeError Ident)
introduceDeclaration decl = do
  RState tenv env mem <- get
  case evalInfer $ extendEnvironment Nothing tenv [decl] of
    Left err -> return $ Left $ RuntimeError $ "Type error: " ++ show err
    Right tenv' -> do
      res <- liftIO $ execInterpreter env mem (processDefinition env decl)
      case res of
        Left err -> return $ Left $ RuntimeError $ "Runtime error: " ++ err
        Right (env', mem') -> do
          modify (\_ -> RState tenv' env' mem')
          return $ Right $ declarationName decl

evalExp :: MonadState RState m => MonadIO m => Exp -> m (Either RuntimeError Value)
evalExp exp = do
  RState tenv env mem <- get
  case evalInfer $ withEnvironment tenv $ inferExpType exp of
    Left err -> return $ Left $ RuntimeError $ "Type error: " ++ show err
    Right _ -> do
      execInterpreterInRuntime (interpret exp >>= force)

showExp :: MonadState RState m => MonadIO m => Exp -> m (Either RuntimeError String)
showExp exp = do
  v <- evalExp exp
  case v of
    Left e -> return $ Left e
    Right v -> execInterpreterInRuntime (ishow RunIO v)
