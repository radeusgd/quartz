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
import qualified AST.Typed as Typed
import qualified AST.Desugared as Desugared
import Interpreter

import Control.Monad
import Control.Exception

import Builtins
import Passes.TypeCheck
import Linker
import Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.Text as PrettyText


parseFile' :: String -> IO (Err [Abs.Declaration])
parseFile' fname = do
  fd <- openFile fname ReadMode
  contents <- hGetContents fd
  let toks = myLexer contents
  return $ (\(Abs.Prog decls) -> decls) <$> pProgram toks

parseFile :: String -> IO (Err [Desugared.Declaration])
parseFile fname = do
  raw <- parseFile' fname
  return $ (map desugarDeclaration) <$> raw

-- runRepl :: IO ()
-- runRepl = do
--   putStrLn "Welcome to Quartz REPL."
--   let tenv = makeTypeEnv builtins
--   (env, mem) <-
--         case execInterpreter emptyEnv emptyMemory (foldM register emptyEnv builtins) of
--           Left err -> (putStrLn $ show err) >> exitFailure
--           Right r -> return r
--   repl' tenv env mem
--   where
--     repl' :: Passes.TypeCheck.Env -> Interpreter.Env -> Interpreter.Memory -> IO ()
--     repl' tenv env mem = do
--       putStr "$> " >> hFlush stdout
--       done <- isEOF
--       if done then putStrLn "Quitting REPL..."
--       else do
--         line <- getLine
--         let toks = myLexer line
--         let decl = pDeclaration toks
--         let exp = pExp toks
--         case (decl, exp) of
--            (Bad de, Bad ee) -> putStrLn "Parse error:" >> print de >> print ee >> repl' tenv env mem
--            (Ok _, Ok _) -> putStrLn "Parsed both a declaration and an expression (how is this even possible?), don't know how to handle this." >> repl' tenv env mem
--            (Bad _, Ok e) -> runExp e tenv env mem
--            (Ok d, Bad _) -> runDecl d tenv env mem
--         exitSuccess
--     runDecl :: Abs.Declaration -> Passes.TypeCheck.Env -> Interpreter.Env -> Interpreter.Memory -> IO ()
--     runDecl decl tenv env mem = do
--       let desugared = desugarDeclaration decl
--       let embedded = embedDeclaration desugared
--       case introduceTopLevelTypes [embedded] tenv of
--          Left err -> (putStrLn $ "Type error: " ++ show err) >> repl' tenv env mem
--          Right tenv' -> do
--             let typed = checkDeclaration tenv' embedded
--             case typed of
--               Left err -> (putStrLn $ "Type error: " ++ show err) >> repl' tenv env mem
--               Right def ->
--                 case execInterpreter env mem (processDefinition env def) of
--                   Left err -> (putStrLn $ "Runtime error: " ++ show err) >> repl' tenv env mem
--                   Right (env', mem') -> putStrLn ("Defined: " ++ Typed.declarationName def) >> repl' tenv' env' mem'
--     runExp :: Abs.Exp -> Passes.TypeCheck.Env -> Interpreter.Env -> Interpreter.Memory -> IO ()
--     runExp exp tenv env mem = do
--       let desugared = desugarExpression exp
--       let embedded = embedExp desugared
--       let typed = checkExpression tenv embedded
--       -- Debugging
--       -- _ <- evaluate $ execInterpreter env mem (traceEnv env)
--       -- End of debugging
--       case typed of
--          Left err -> (putStrLn $ "Type error: " ++ show err) >> repl' tenv env mem
--          Right e ->
--           case execInterpreter env mem (interpret e) of
--             Left err -> (putStrLn $ "Runtime error: " ++ show err) >> repl' tenv env mem
--             Right (res, mem') -> putStrLn (show res) >> repl' tenv env mem'

handleSyntaxError :: Err a -> IO a
handleSyntaxError (Bad err) = putStrLn ("Syntax error: " ++ err) >> exitFailure
handleSyntaxError (Ok a) = return a

-- initialTypingEnv :: Passes.TypeCheck.Env
-- initialTypingEnv = makeTypeEnv builtins

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

-- preprocess :: [Abs.Declaration] -> [Typed.Declaration]
-- preprocess decls =
--   let desugared = map desugarDeclaration decls in
--   let embedded = map embedDeclaration desugared in
--     embedded

-- typeCheck :: [Typed.Declaration] -> IO [Typed.Declaration]
-- typeCheck decls = do
--   let env = introduceTopLevelTypes decls initialTypingEnv
--   case env of
--     Left err -> printTypeError err >> exitFailure
--     Right env -> do
--       let checked = map (checkDeclaration env) decls
--       collectSuccessOrPrintErrors printTypeError checked
--   where
--     printTypeError e = putStrLn $ "Type error: " ++ show e

-- runCheck :: String -> IO ()
-- runCheck fname = do
--   parsed <- parseFile' fname
--   decls <- handleSyntaxError parsed
--   let embedded = preprocess decls
--   _ <- typeCheck embedded
--   exitSuccess

-- runExtract :: String -> IO ()
-- runExtract fname = do
--   decls <- parseFile fname >>= handleSyntaxError
--   let embedded = map embedDeclaration decls
--   mapM_ printSignature embedded
--   exitSuccess
--   where
--     printSignature :: Typed.Declaration -> IO ()
--     printSignature (Typed.Function ttype name args _) = putStrLn sig where
--       sig = name ++ "(" ++ printArgs args ++ ")" ++ ": " ++ show ttype
--       printArgs [] = ""
--       printArgs [arg] = arg
--       printArgs (arg : t) = arg ++ ", " ++ printArgs t

-- runMain :: [Typed.Declaration] -> Either Interpreter.ErrorType Interpreter.Value
-- runMain decls = runWithBuiltins builtins $ withDeclared decls $ interpret (Typed.EVar undefined "main")

-- runRun :: String -> IO ()
-- runRun fname = do
--   parsed <- parseFile' fname
--   decls <- handleSyntaxError parsed
--   let embedded = preprocess decls
--   typed <- typeCheck embedded
--   case runMain typed of
--     Left err -> putStrLn ("Runtime error: " ++ show err) >> exitFailure
--     Right res -> print res
--   exitSuccess

runDebug :: String -> IO ()
runDebug fname = do
  parsed <- parseFile' fname
  decls <- handleSyntaxError parsed
  putStrLn "[Linearized AST]"
  mapM_ (putStrLn . printTree) decls
  let desugared = map desugarDeclaration decls
  putStrLn "\n[Desugared]"
  mapM_ prettyLine desugared
  -- let embedded = map embedDeclaration desugared
  -- putStrLn "\n[Embedded]"
  -- mapM_ prettyLine embedded
  -- typed <- typeCheck embedded
  -- putStrLn "\n[Typed]"
  -- mapM_ prettyLine typed
  -- case runMain typed of
  --   Left err -> putStrLn ("Runtime error: " ++ show err) >> exitFailure
  --   Right res -> print res
  -- exitSuccess
  where
    prettyLine x = (PrettyText.putDoc $ Pretty.pretty x) >> putStrLn ""

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- ["repl"] -> runRepl
    -- ["check", fname] -> runCheck fname
    -- ["extract", fname] -> runExtract fname
    -- ["run", fname] -> runRun fname
    ["debug", fname] -> runDebug fname
    _ -> putStrLn "TODO usage"
