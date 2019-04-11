module Builtins(
  loadBuiltinDecls,
  withBuiltins,
  builtinsEnv
               ) where

import qualified Quartz.Syntax.AbsQuartz as Abs
import qualified Quartz.Syntax.ParQuartz as Par
import qualified Quartz.Syntax.ErrM as Err
import Control.Monad.Reader
import Control.Monad.Except
import AST.Desugared
import Passes.Desugar
import System.IO
import Interpreter
import Linker

loadBuiltinDecls :: IO [Declaration]
loadBuiltinDecls = do
  stdlib <- findStdLib
  fd <- openFile (stdlib ++ "/Builtins.quartz") ReadMode
  contents <- hGetContents fd
  let toks = Par.myLexer contents
  let ed = (\(Abs.Prog decls) -> decls) <$> Par.pProgram toks
  case ed of
    Err.Bad s -> error $ "Fatal error: syntax error in Builtins.quartz: " ++ s
    Err.Ok decls -> return $ map desugarDeclaration decls

register :: Env -> (String, Value) -> Interpreter Env
register env (name, v) = do
  loc <- alloc
  setValueEager loc v
  return $ bind name loc env

-- TODO make sure all builtins have respective declarations and vice versa
builtinsEnv :: Interpreter Env
builtinsEnv = do
  env <- ask
  foldM register env builtins

withBuiltins :: Interpreter a -> Interpreter a
withBuiltins m = do
  env <- builtinsEnv
  local (\_ -> env) m

-- import qualified Data.Map as M
-- import AST.Typed
-- import Passes.TypeCheck(Env)
-- import Interpreter(Value(..), emptyEnv, readVar)
-- import qualified Interpreter as I
-- import Control.Monad.Reader

make1ArgFunLazy :: (LazyValue -> Interpreter LazyValue) -> Value
make1ArgFunLazy f = VFunction "x" emptyEnv $ do
  x <- readVarLazy "x"
  f x

make2ArgFunLazy :: (LazyValue -> LazyValue -> Interpreter LazyValue) -> Value
make2ArgFunLazy f = VFunction "x" emptyEnv $ do
  x <- readVarLazy "x"
  makeLazy $ return $ VFunction "y" emptyEnv $ do
    y <- readVarLazy "y"
    f x y

make3ArgFunLazy :: (LazyValue -> LazyValue -> LazyValue -> Interpreter LazyValue) -> Value
make3ArgFunLazy f = VFunction "z" emptyEnv $ do
  z <- readVarLazy "z"
  makeLazy $ return $ make2ArgFunLazy (f z)

make1ArgFun :: (Value -> Value) -> Value
make1ArgFun f = VFunction "x" emptyEnv $ do
  x <- readVar "x"
  makeLazy $ return $ f x

make2ArgFun :: (Value -> Value -> Value) -> Value
make2ArgFun f = VFunction "x" emptyEnv $ do
  x <- readVar "x"
  makeLazy $ return $ VFunction "y" emptyEnv $ do
    y <- readVar "y"
    makeLazy $ return $ f x y

make3ArgFun :: (Value -> Value -> Value -> Value) -> Value
make3ArgFun f = VFunction "z" emptyEnv $ do
  z <- readVar "z"
  makeLazy $ return $ make2ArgFun (f z)

builtins :: [(String, Value)]
builtins = [
  -- TODO actually plus should be polymorphic... but for now let's skip this
  ("+", (make2ArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x + y)),
  ("*", (make2ArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x * y)),
  ("-", (make2ArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x - y)),
  ("/", (make2ArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x `div` y)),
  ("<+>", (make2ArgFun $ \(VStr a) -> \(VStr b) -> VStr $ a ++ b)),
  ("==", make2ArgFun $ \x -> \y -> VBool $ x == y),
  ("<=", make2ArgFun $ \(VInt x) -> \(VInt y) -> VBool $ x <= y),
  (">=", make2ArgFun $ \(VInt x) -> \(VInt y) -> VBool $ x >= y),
  ("<", make2ArgFun $ \(VInt x) -> \(VInt y) -> VBool $ x < y),
  (">", make2ArgFun $ \(VInt x) -> \(VInt y) -> VBool $ x > y),
  ("if_then_else", (make3ArgFunLazy $ \cond -> \tt -> \ff -> do (VBool x) <- force cond; return $ if x then tt else ff)),
  ("error", VFunction "msg" emptyEnv $ do (VStr msg) <- readVar "msg"; throwError msg),
  ("toString", make1ArgFun $ \v -> VStr $ show v)
           ]

instance Eq Value where
  (VStr a) == (VStr b) = a == b
  (VInt a) == (VInt b) = a == b
  (VDouble a) == (VDouble b) = a == b
  (VBool a) == (VBool b) = a == b
  (VFunction _ _ _) == _ = error "Cannot compare functions" -- TODO promote this to interpreter error instead of crash
  _ == _ = False
