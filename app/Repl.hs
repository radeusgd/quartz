{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Repl where

import Quartz.Syntax.LexQuartz as Lex
import Quartz.Syntax.ParQuartz as Par
import qualified Quartz.Syntax.AbsQuartz as Abs
import Quartz.Syntax.ErrM as ParErr
import Passes.Desugar
import Passes.TypeCheck
import AST.Desugared
import Builtins
import Interpreter
import AppCommon
import Runtime

import System.Console.Repline
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List
import qualified ModuleMap as MM
import qualified Data.Map as Map

type Repl = HaskelineT (StateT RState IO)
type FailableRepl = ExceptT RuntimeError Repl

handleError :: FailableRepl () -> Repl ()
handleError m = do
  e <- runExceptT m
  case e of
    Left e -> liftIO $ print e
    Right () -> return ()

parse :: ([Lex.Token] -> ParErr.Err a) -> String -> Either String a
parse par str = let toks = Par.myLexer str in case par toks of
  Ok a -> return a
  Bad s -> throwError s

parseDecl :: String -> Either String Declaration
parseDecl str = desugarDeclaration <$> parse Par.pDeclaration str

parseExp :: String -> Either String Exp
parseExp str = desugarExpression <$> parse Par.pExp str

data ParsedLine = PExp Exp | PDecl Declaration | PImport Ident | PError String

parseLine :: String -> ParsedLine
parseLine str = case (parseDecl str, parseExp str, parse Par.pImport str) of
  (Right d, Left _, Left _) -> PDecl d
  (Left _, Right e, Left _) -> PExp e
  (Left _, Left _, Right (Abs.NormalImport (Abs.QIdent (_, i)))) -> PImport i
  (Left e, Left d, Left i) -> PError $ e ++ "\n/\n" ++ d ++ "\n/\n" ++ i
  _ -> PError $ "Ambiguous parse: cannot discern between expression, declaration and import"

showingError :: Show e => Either e a -> Either String a
showingError (Right a) = Right a
showingError (Left e) = Left $ show e

runDecl :: Declaration -> FailableRepl ()
runDecl decl = do
  name <- introduceDeclaration decl
  liftIO $ putStrLn $ name ++ " defined"

runExp :: Exp -> FailableRepl ()
runExp e = do
  str <- showExp e
  liftIO $ putStrLn str

runImport :: String -> FailableRepl ()
runImport mod = do
  path <- findModule mod
  _ <- introduceModule path
  liftIO $ putStrLn $ "Imported " ++ mod

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = handleError $ do
  case parseLine input of
    PError err -> liftIO $ putStrLn $ "Parse error: " ++ err
    PDecl decl -> runDecl decl
    PExp exp -> runExp exp
    PImport mod -> runImport mod

completer = Prefix (wordCompleter complete) [(":load", fileCompleter)]

varsFromEnv :: TypeEnv -> [String]
varsFromEnv (MM.ModuleMap modules local) = (Map.toList modules >>= toQualifiedList) ++ (Map.keys local) ++ (Map.elems modules >>= Map.keys) where
  toQualifiedList :: (Ident, Map.Map Ident a) -> [String]
  toQualifiedList (mod, m) = let pref = mod ++ "." in map (pref ++) (Map.keys m)

complete :: (Monad m, MonadState RState m) => WordCompleter m
complete n = let cmds = map ((':':) . fst) options in do
  tenv <- gets rsTypeEnv
  let names = varsFromEnv tenv
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

loadFile :: [String] -> FailableRepl ()
loadFile args = do
  parsed <- liftIO $ parseFile (unwords args)
  case parsed of
    ParErr.Bad e -> liftIO $ putStrLn $ "Syntax error: " ++ e
    ParErr.Ok (imports, decls) -> do
      mapM_ runImport imports
      mapM_ runDecl decls

options :: [(String, [String] -> Repl ())]
options = [
    ("t", typeof),
    ("q", quit),
    ("load", handleError . loadFile)
  ]

quit :: [String] -> Repl ()
quit _ = liftIO $ exitSuccess

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to the Quartz REPL!"

runRepl :: IO ()
runRepl = do
  init <- runExceptT makeInitialState
  case init of
    Left (RuntimeError err) -> error $ "Error initializing REPL: " ++ err
    Right initState -> do
      evalStateT (evalRepl "$> " cmd options completer ini) initState
