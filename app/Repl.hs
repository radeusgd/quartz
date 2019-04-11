{-# LANGUAGE FlexibleContexts #-}
module Repl where

import Quartz.Syntax.LexQuartz as Lex
import Quartz.Syntax.ParQuartz as Par
import Quartz.Syntax.ErrM as ParErr
import Passes.Desugar
import Passes.TypeCheck
import AST.Desugared
import Builtins

import System.Console.Repline
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List

data RState = RState { rsTypeEnv :: [Declaration] }

makeInitialState :: IO RState
makeInitialState = do
  builtins <- loadBuiltinDecls
  return $ RState builtins

type Repl = HaskelineT (StateT RState IO)

parse :: ([Lex.Token] -> ParErr.Err a) -> String -> Either String a
parse par str = let toks = Par.myLexer str in case par toks of
  Ok a -> return a
  Bad s -> throwError s

parseDecl :: String -> Either String Declaration
parseDecl str = desugarDeclaration <$> parse Par.pDeclaration str

parseExp :: String -> Either String Exp
parseExp str = desugarExpression <$> parse Par.pExp str

showingError :: Show e => Either e a -> Either String a
showingError (Right a) = Right a
showingError (Left e) = Left $ show e

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print input

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
    Left err -> liftIO $ putStrLn $ "Error" ++ err
    Right exp ->
      let t = showingError $ inferType $ withTopLevelDecls tenv $ inferE exp in
      case t of
        Left err -> liftIO $ putStrLn $ "Error" ++ err
        Right t -> liftIO $ putStrLn $ "Type of " ++ show exp ++ " is " ++ show t

options :: [(String, [String] -> Repl ())]
options = [
    ("t", typeof),
    ("q", quit)
  ]

quit :: [String] -> Repl ()
quit _ = liftIO $ exitSuccess

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to Quartz REPL!"

runRepl :: IO ()
runRepl = do
  initState <- makeInitialState
  evalStateT (evalRepl "$> " cmd options (Word completer) ini) initState
