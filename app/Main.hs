module Main where

import System.IO
import System.Environment
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Exception

import Quartz.Syntax.LexQuartz
import Quartz.Syntax.ParQuartz
import Quartz.Syntax.SkelQuartz
import Quartz.Syntax.PrintQuartz
import qualified Quartz.Syntax.AbsQuartz as Abs

import Quartz.Syntax.ErrM

import Passes.Desugar
import Passes.EmbedTypes
import AST.Desugared
import Interpreter
import Runtime

import Builtins
import Passes.TypeCheck
import Linker
import Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.Text as PrettyText

import Repl
import AppCommon

handleSyntaxError :: Err a -> IO a
handleSyntaxError (Bad err) = putStrLn ("Syntax error: " ++ err) >> exitFailure
handleSyntaxError (Ok a) = return a

groupEithers :: [Either e a] -> ([e], [a])
groupEithers [] = ([], [])
groupEithers (Left e : t) =
  let (es, as) = groupEithers t in (e : es, as)
groupEithers (Right a : t) =
  let (es, as) = groupEithers t in (es, a : as)

collectSuccessOrPrintErrors :: (e -> IO ()) -> [Either e d] -> IO [d]
collectSuccessOrPrintErrors printer lst =
  let (es, as) = groupEithers lst in
  if null es then return as
  else mapM_ printer es >> exitFailure

typeCheck :: String -> [Declaration] -> IO [Declaration]
typeCheck mod decls = do
  builtinTypes <- loadBuiltinDecls
  let tcres = evalInfer $ withTopLevelDecls (Just mod) builtinTypes $ (typeCheckTopLevel (Just mod) decls)
  case tcres of
    Left err -> printTypeError err >> exitFailure
    Right () -> return decls
  where
    printTypeError e = putStrLn $ "Type error: " ++ show e

runCheck :: String -> IO ()
runCheck fname = do
  parsed <- parseFile' fname
  (imports, decls) <- handleSyntaxError parsed
  -- TODO imports
  let desugared = map desugarDeclaration decls
  _ <- typeCheck (moduleName fname) desugared
  exitSuccess

runExtract :: String -> IO ()
runExtract fname = do
  (imports, decls) <- parseFile fname >>= handleSyntaxError
  mapM_ printImport imports
  mapM_ printSignature decls
  exitSuccess
  where
    printImport :: String -> IO ()
    printImport i = putStrLn $ "import " ++ i
    printSignature :: Declaration -> IO ()
    printSignature (Function name args ttype _) = putStrLn sig where
      sig = name ++ "(" ++ printArgs args ++ ")" ++ ": " ++ maybe "???" show ttype
      printArgs [] = ""
      printArgs [arg] = arg
      printArgs (arg : t) = arg ++ ", " ++ printArgs t
    printSignature (DataType name typeargs cases) = mapM_ (printCase (buildConstructor name $ reverse typeargs)) cases
    buildConstructor :: Ident -> [Ident] -> Type
    buildConstructor name [] = Atom $ IQualified (moduleName fname) name
    buildConstructor name (h:t) = Construction (buildConstructor name t) (Atom $ IDefault h)
    buildFunction :: Type -> [Type] -> Type
    buildFunction ret [] = ret
    buildFunction ret (h:t) = Abstraction h (buildFunction ret t)
    printCase rettype (DataTypeCase name fields) = putStrLn $ name ++ ": " ++ show (buildFunction rettype fields)

runMain :: [Declaration] -> IO (Either Interpreter.ErrorType String)
runMain decls = error "TODO " -- runInterpreter $ withBuiltins $ withDeclared decls $ interpret (EVar $ IDefault "main") >>= ishow RunIO

handleError :: Show e => MonadIO m => Either e a -> m a
handleError (Left e) = (liftIO $ putStrLn $ "Error: " ++ show e) >> (liftIO $ exitFailure)
handleError (Right e) = return e

runImportAndMain :: FilePath -> StateT RState IO ()
runImportAndMain path = do
  introduceModule path >>= handleError
  _ <- showExp (EVar $ IDefault "main") -- TODO typecheck that main exists and has proper signature
  return ()
  -- TODO doesn't seem to evaluate

runRun :: String -> IO ()
runRun fname = do
  init <- runExceptT makeInitialState
  case init of
    Left (RuntimeError err) -> (putStrLn $ "Error initializing the runtime: " ++ err) >> exitFailure
    Right initState -> evalStateT (runImportAndMain fname) initState >> exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["repl"] -> runRepl
    ["check", fname] -> runCheck fname
    ["extract", fname] -> runExtract fname
    ["run", fname] -> runRun fname
    _ -> putStrLn "TODO usage"
