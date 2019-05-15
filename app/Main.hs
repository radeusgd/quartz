{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude hiding(mod)
import System.Environment
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad.State
import Control.Monad.Except

import Quartz.Syntax.ErrM

import Passes.Desugar
import AST.Desugared
import Runtime

import Builtins
import Passes.TypeCheck

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

handleErrorByFailing :: MonadIO m => ExceptT String m a -> m a
handleErrorByFailing m = do
  r <- runExceptT m
  case r of
    Left err -> liftIO $ print err >> exitFailure
    Right a -> return a

runImportAndMain :: FilePath -> StateT RState IO ()
runImportAndMain path = do
  handleErrorByFailing $ introduceModule path
  te <- gets rsTypeEnv
  _ <- handleErrorByFailing $ showExp (EVar $ IDefault "main") -- TODO typecheck that main exists and has proper signature
  return ()

runRun :: String -> IO ()
runRun fname = do
  i <- runExceptT makeInitialState
  case i of
    Left err -> (putStrLn $ "Error initializing the runtime: " ++ err) >> exitFailure
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
