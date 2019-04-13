module Main where

import System.IO
import System.Environment
import System.Exit ( exitFailure, exitSuccess )

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

import Control.Monad
import Control.Exception

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

typeCheck :: [Declaration] -> IO [Declaration]
typeCheck decls = do
  builtinTypes <- loadBuiltinDecls
  let tcres = evalInfer $ withTopLevelDecls builtinTypes $ (typeCheckTopLevel decls)
  case tcres of
    Left err -> printTypeError err >> exitFailure
    Right () -> return decls
  where
    printTypeError e = putStrLn $ "Type error: " ++ show e

runCheck :: String -> IO ()
runCheck fname = do
  parsed <- parseFile' fname
  decls <- handleSyntaxError parsed
  let desugared = map desugarDeclaration decls
  _ <- typeCheck desugared
  exitSuccess

runExtract :: String -> IO ()
runExtract fname = do
  decls <- parseFile fname >>= handleSyntaxError
  mapM_ printSignature decls
  exitSuccess
  where
    printSignature :: Declaration -> IO ()
    printSignature (Function name args ttype _) = putStrLn sig where
      sig = name ++ "(" ++ printArgs args ++ ")" ++ ": " ++ maybe "???" show ttype
      printArgs [] = ""
      printArgs [arg] = arg
      printArgs (arg : t) = arg ++ ", " ++ printArgs t

runMain :: [Declaration] -> Either Interpreter.ErrorType String
runMain decls = runInterpreter $ withBuiltins $ withDeclared decls $ interpret (EVar "main") >>= ishow RunIO

runRun :: String -> IO ()
runRun fname = do
  parsed <- parseFile' fname
  decls <- handleSyntaxError parsed
  let desugared = map desugarDeclaration decls
  typed <- typeCheck desugared
  case runMain typed of
    Left err -> putStrLn ("Runtime error: " ++ show err) >> exitFailure
    Right res -> putStrLn res
  exitSuccess

runDebug :: String -> IO ()
runDebug fname = do
  parsed <- parseFile' fname
  decls <- handleSyntaxError parsed
  putStrLn "[Linearized AST]"
  mapM_ (putStrLn . printTree) decls
  let desugared = map desugarDeclaration decls
  putStrLn "\n[Desugared]"
  mapM_ prettyLine desugared
  putStrLn "Typechecking"
  builtinTypes <- loadBuiltinDecls
  let tcres = evalInfer $ withTopLevelDecls builtinTypes $ (typeCheckTopLevel desugared) -- TODO typecheck should also look for existence of main
  case tcres of
    Left err -> putStrLn ("Typechecker error: " ++ show err) >> exitFailure
    Right () -> putStrLn "Types ok."
  case runMain desugared of
    Left err -> putStrLn ("Runtime error: " ++ show err) >> exitFailure
    Right res -> putStrLn res
  exitSuccess
  where
    prettyLine x = (PrettyText.putDoc $ Pretty.pretty x) >> putStrLn ""

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["repl"] -> runRepl
    ["check", fname] -> runCheck fname
    ["extract", fname] -> runExtract fname
    ["run", fname] -> runRun fname
    ["debug", fname] -> runDebug fname
    _ -> putStrLn "TODO usage"
