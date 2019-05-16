{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Repl where

import Prelude hiding (mod, exp)
import Quartz.Syntax.LexQuartz as Lex
import Quartz.Syntax.ParQuartz as Par
import qualified Quartz.Syntax.AbsQuartz as Abs
import Quartz.Syntax.ErrM as ParErr
import Passes.Desugar
import Passes.TypeCheck
import AST.Desugared
import Interpreter
import AppCommon
import Runtime
import Linker
import Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.Text as PrettyText

import System.Console.Repline
import System.Exit ( exitSuccess, exitFailure )
import System.IO
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List
import qualified ModuleMap as MM
import qualified Data.Map as Map

type Repl = HaskelineT (StateT RState IO)
type FailableRepl = ExceptT String Repl

handleError :: FailableRepl () -> Repl ()
handleError m = do
  r <- runExceptT m
  case r of
    Left e -> liftIO $ hPutStrLn stderr e
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
    PError err -> liftIO $ hPutStrLn stderr $ "Parse error: " ++ err
    PDecl decl -> runDecl decl
    PExp exp -> runExp exp
    PImport mod -> runImport mod

completer :: CompleterStyle (StateT RState IO)
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
  case parseExp (unwords args) of
    Left err -> liftIO $ hPutStrLn stderr $ "Error: " ++ err
    Right exp ->
      case showingError $ inferType $ withEnvironment tenv $ inferE exp of
        Left err -> liftIO $ hPutStrLn stderr $ "Error: " ++ err
        Right t -> liftIO $ hPutStrLn stderr $ "Type of " ++ show exp ++ " is " ++ show t

inspectMemory :: [String] -> Repl ()
inspectMemory [arg] = do
  let loc = read arg :: Loc
  mem <- gets rsMem
  let locations = thunks mem
  case Map.lookup loc locations of
    Nothing -> liftIO $ putStrLn "No data under this address"
    Just (ThunkComputed v) -> liftIO $ print v
    Just (ThunkLazy (Lazy env _)) -> liftIO $ putStrLn $ "Lazy value, not computed, closure = " ++ show (vars env)
  return ()
inspectMemory _ = liftIO $ putStrLn "Wrong number of arguments"

loadFile :: [String] -> FailableRepl ()
loadFile args = do
  parsed <- liftIO $ parseFile (unwords args)
  case parsed of
    ParErr.Bad e -> liftIO $ hPutStrLn stderr $ "Syntax error: " ++ e
    ParErr.Ok (imports, decls) -> do
      mapM_ runImport imports
      mapM_ runDecl decls

options :: [(String, [String] -> Repl ())]
options = [
    ("t", typeof),
    ("q", quit),
    ("load", handleError . loadFile),
    ("inspectMemory", inspectMemory),
    ("parseExp", \args -> let s = unwords args in case parseExp s of
        Left err -> liftIO $ hPutStrLn stderr $ "Parse error: " ++ err
        Right tree -> liftIO $ (PrettyText.putDoc $ Pretty.pretty tree) >> putStrLn ""
    )
  ]

quit :: [String] -> Repl ()
quit _ = liftIO $ exitSuccess

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to the Quartz REPL!"

runRepl :: IO ()
runRepl = do
  initialState <- runExceptT makeInitialState
  case initialState of
    Left err -> (hPutStrLn stderr $ "Error initializing REPL: " ++ err) >> exitFailure
    Right initState -> do
      evalStateT (evalRepl "$> " cmd options completer ini) initState
