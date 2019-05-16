{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude hiding(mod)
import System.Environment
import System.IO
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad.State
import Control.Monad.Except

import Quartz.Syntax.ErrM

import AST.Desugared
import Runtime
import Linker
import Builtins
import Passes.TypeCheck

import Repl
import AppCommon

putStdErr :: String -> IO ()
putStdErr = hPutStrLn stderr

handleSyntaxError :: String -> Err a -> IO a
handleSyntaxError filename (Bad err) = putStdErr (filename ++ " Syntax error: " ++ err) >> exitFailure
handleSyntaxError _ (Ok a) = return a

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

runCheck :: String -> IO ()
runCheck fname = do
  builtins <- loadBuiltinDecls
  case evalInfer $ extendEnvironment (Just builtinsModuleName) emptyTypeEnv builtins of
    Left err -> printTypeError "[Builtins]" err >> exitFailure
    Right initialEnv -> do
      parsed <- parseFile fname
      (imports, decls) <- handleSyntaxError fname parsed
      paths <- mapM (handleErrorByFailing . findModule) imports
      env <- foldM loadSigsFromModule initialEnv paths
      -- _ <- typeCheck (moduleName fname) desugared
      case evalInfer $ withEnvironment env $ typeCheckTopLevel (Just $ moduleName fname) decls of
        Left err -> printTypeError fname err >> exitFailure
        Right () -> exitSuccess
  where
    printTypeError path e = putStdErr $ path ++ " Type error: " ++ show e
    loadSigsFromModule :: TypeEnv -> String -> IO TypeEnv
    loadSigsFromModule env path = do
      parsed <- parseFile path
      (_, decls) <- handleSyntaxError path parsed
      case evalInfer $ withEnvironment env $ withTopLevelDecls (Just $ moduleName path) decls $ fetchCurrentEnvironment of
        Left err -> putStdErr "Unexpected error:" >> printTypeError path err >> exitFailure
        Right env' -> return env'

runExtract :: String -> IO ()
runExtract fname = do
  (imports, decls) <- parseFile fname >>= handleSyntaxError fname
  mapM_ printImport imports
  mapM_ printSignature decls
  exitSuccess
  where
    printImport :: String -> IO ()
    printImport i = putStdErr $ "import " ++ i
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
    Left err -> liftIO $ putStdErr err >> exitFailure
    Right a -> return a

runImportAndMain :: FilePath -> StateT RState IO ()
runImportAndMain path = do
  handleErrorByFailing $ introduceModule path
  -- we create a block enforcing main's type to unify with IO ()
  _ <- handleErrorByFailing $ showExp (EBlock [Function "entryPoint" [] (Just $ ForAll [] (Construction (Atom $ IDefault "IO") (Atom $ IDefault "()"))) $ EVar $ IDefault "main"] (EVar $ IDefault "entryPoint"))
  return ()

runRun :: String -> IO ()
runRun fname = do
  i <- runExceptT makeInitialState
  case i of
    Left err -> (putStdErr $ "Error initializing the runtime: " ++ err) >> exitFailure
    Right initState -> evalStateT (runImportAndMain fname) initState >> exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--repl"] -> runRepl
    ["--check", fname] -> runCheck fname
    ["--extract", fname] -> runExtract fname
    [fname] -> runRun fname
    _ -> putStrLn "Possbile argument combinations: --repl | --check fname | --extract fname | fname\n Providing just a filename executes this file, check runs a typecheck of the file (but not its dependencies), extract prints all symbols and imports defined in the file, repl launches an interactive session."
