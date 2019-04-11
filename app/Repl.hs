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


data RState = RState { rsTypeEnv :: [Declaration], rsVarEnv :: Env, rsMem :: Memory }

makeInitialState :: IO RState
makeInitialState = do
  builtins <- loadBuiltinDecls
  case execInterpreter emptyEnv emptyMemory builtinsEnv of
    Right (env, mem) ->
      return $ RState builtins env mem
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

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = do
  RState tenv env mem <- get
  case parseExpOrDecl input of
    Left err -> liftIO $ putStrLn $ "Parse error: " ++ err
    Right (Left decl@(Function name args _ body)) ->
      case evalInfer $ withTopLevelDecls tenv $ inferDeclType decl of
        Left err -> liftIO $ putStrLn $ "Type error: " ++ show err
        Right ttype ->
          case execInterpreter env mem (processDefinition env decl) of
            Left err -> liftIO $ putStrLn $ "Runtime error: " ++ err
            Right (env', mem') -> do
              let decl' = Function name args (Just ttype) body
              modify (\_ -> RState (decl' : tenv) env' mem')
              liftIO $ putStrLn $ (declarationName decl) ++ " defined"
    Right (Right exp) ->
      case evalInfer $ withTopLevelDecls tenv $ inferExpType exp of
        Left err -> liftIO $ putStrLn $ "Type error: " ++ show err
        Right _ -> case execInterpreter env mem (interpret exp) of
          Left err -> liftIO $ putStrLn $ "Runtime error: " ++ err
          Right (res, mem') -> do
            modify (\s -> s { rsMem = mem' })
            liftIO $ print res
  return ()

completer :: (Monad m, MonadState RState m) => WordCompleter m
completer n = do
  tenv <- gets rsTypeEnv
  let names = map getName tenv
  return $ filter (isPrefixOf n) names
  where
    getName (Function name _ _ _) = name

help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

typeof :: [String] -> Repl ()
typeof args = do
  tenv <- gets rsTypeEnv
  let exp = parseExp (unwords args)
  case exp of
    Left err -> liftIO $ putStrLn $ "Error: " ++ err
    Right exp ->
      let t = showingError $ inferType $ withTopLevelDecls tenv $ inferE exp in
      case t of
        Left err -> liftIO $ putStrLn $ "Error: " ++ err
        Right t -> liftIO $ putStrLn $ "Type of " ++ show exp ++ " is " ++ show t

options :: [(String, [String] -> Repl ())]
options = [
    ("t", typeof),
    ("q", quit)
  ]

quit :: [String] -> Repl ()
quit _ = liftIO $ exitSuccess

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to the Quartz REPL!"

runRepl :: IO ()
runRepl = do
  initState <- makeInitialState
  evalStateT (evalRepl "$> " cmd options (Word completer) ini) initState
