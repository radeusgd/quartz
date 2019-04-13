{-# LANGUAGE FlexibleContexts #-}
module Repl where

import Quartz.Syntax.LexQuartz as Lex
import Quartz.Syntax.ParQuartz as Par
import Quartz.Syntax.ErrM as ParErr
import Passes.Desugar
import Passes.TypeCheck
import AST.Desugared
import Builtins
import Interpreter
import AppCommon

import System.Console.Repline
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List
import qualified Data.Map as Map


data RState = RState { rsTypeEnv :: TypeEnv, rsVarEnv :: Env, rsMem :: Memory }

makeInitialState :: IO RState
makeInitialState = do
  builtins <- loadBuiltinDecls
  let tenv = case evalInfer $ extendEnvironment emptyTypeEnv builtins of
               Right env -> env
               Left e -> error $ "Fatal error, cannot typecheck builtins: " ++ show e
  res <- execInterpreter emptyEnv emptyMemory builtinsEnv
  case res of
    Right (env, mem) ->
      return $ RState tenv env mem
    Left e -> error $ "Fatal error, cannot initialie builtins: " ++ e

type Repl = HaskelineT (StateT RState IO)

parse :: ([Lex.Token] -> ParErr.Err a) -> String -> Either String a
parse par str = let toks = Par.myLexer str in case par toks of
  Ok a -> return a
  Bad s -> throwError s

parseDecl :: String -> Either String Declaration
parseDecl str = desugarDeclaration <$> parse Par.pDeclaration str

parseExp :: String -> Either String Exp
parseExp str = desugarExpression <$> parse Par.pExp str

parseExpOrDecl :: String -> Either String (Either Declaration Exp)
parseExpOrDecl str = case (parse Par.pDeclaration str, parse Par.pExp str) of
  (Right d, Left _) -> return $ Left $ desugarDeclaration d
  (Left _, Right e) -> return $ Right $ desugarExpression e
  (Right _, Right _) -> throwError $ "Ambiguous parse: cannot discern expression from declaration"
  (Left e, Left d) -> throwError $ e ++ "\n/\n" ++ d

showingError :: Show e => Either e a -> Either String a
showingError (Right a) = Right a
showingError (Left e) = Left $ show e

runDecl :: Declaration -> Repl ()
runDecl decl = do
  RState tenv env mem <- get
  case evalInfer $ extendEnvironment tenv [decl] of
    Left err -> liftIO $ putStrLn $ "Type error: " ++ show err
    Right tenv' -> do
      res <- liftIO $ execInterpreter env mem (processDefinition env decl)
      case res of
        Left err -> liftIO $ putStrLn $ "Runtime error: " ++ err
        Right (env', mem') -> do
          modify (\_ -> RState tenv' env' mem')
          liftIO $ putStrLn $ (declarationName decl) ++ " defined"

runExp :: Exp -> Repl ()
runExp exp = do
  RState tenv env mem <- get
  case evalInfer $ withEnvironment tenv $ inferExpType exp of
    Left err -> liftIO $ putStrLn $ "Type error: " ++ show err
    Right _ -> do
      res <- liftIO $ execInterpreter env mem (interpret exp >>= ishow RunIO)
      case res of
        Left err -> liftIO $ putStrLn $ "Runtime error: " ++ err
        Right (res, mem') -> do
          modify (\s -> s { rsMem = mem' })
          liftIO $ putStrLn res

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = do
  case parseExpOrDecl input of
    Left err -> liftIO $ putStrLn $ "Parse error: " ++ err
    Right (Left decl) -> runDecl decl
    Right (Right exp) -> runExp exp
  return ()

completer = Prefix (wordCompleter complete) [(":load", fileCompleter)]

complete :: (Monad m, MonadState RState m) => WordCompleter m
complete n = let cmds = map ((':':) . fst) options in do
  tenv <- gets rsTypeEnv
  let names = Map.keys tenv
  return $ filter (isPrefixOf n) (names ++ cmds)

help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

typeof :: [String] -> Repl ()
typeof args = do
  tenv <- gets rsTypeEnv
  let exp = parseExp (unwords args)
  case exp of
    Left err -> liftIO $ putStrLn $ "Error: " ++ err
    Right exp ->
      let t = showingError $ inferType $ withEnvironment tenv $ inferE exp in
      case t of
        Left err -> liftIO $ putStrLn $ "Error: " ++ err
        Right t -> liftIO $ putStrLn $ "Type of " ++ show exp ++ " is " ++ show t

loadFile :: [String] -> Repl ()
loadFile args = do
  parsed <- liftIO $ parseFile (unwords args)
  case parsed of
    ParErr.Bad e -> liftIO $ putStrLn $ "Syntax error: " ++ e
    ParErr.Ok decls -> do
      mapM_ runDecl decls

options :: [(String, [String] -> Repl ())]
options = [
    ("t", typeof),
    ("q", quit),
    ("load", loadFile)
  ]

quit :: [String] -> Repl ()
quit _ = liftIO $ exitSuccess

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to the Quartz REPL!"

runRepl :: IO ()
runRepl = do
  initState <- makeInitialState
  evalStateT (evalRepl "$> " cmd options completer ini) initState
