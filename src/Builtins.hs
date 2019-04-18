module Builtins(
  loadBuiltinDecls,
  withBuiltins,
  builtinsEnv
               ) where

import qualified Quartz.Syntax.AbsQuartz as Abs
import qualified Quartz.Syntax.ParQuartz as Par
import qualified Quartz.Syntax.ErrM as Err
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import AST.Desugared
import Passes.Desugar
import System.IO
import Interpreter
import Linker
import Debug.Trace

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
  inOtherEnv env m

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

makeNothing :: Value
makeNothing = VDataType "Nothing" []

makeJust :: Value -> Value
makeJust v = VDataType "Just" [v]

makeMaybe :: Maybe Value -> Value
makeMaybe (Just v) = makeJust v
makeMaybe (Nothing) = makeNothing

makeNil :: Value
makeNil = VDataType "Nil" []

makeCons :: Value -> Value -> Value
makeCons h t = VDataType "Cons" [h, t]

makeBool :: Bool -> Value
makeBool True = VDataType "True" []
makeBool False = VDataType "False" []

unpackBool :: Value -> Bool
unpackBool (VDataType "True" []) = True
unpackBool (VDataType "False" []) = False
unpackBool other = error (show other ++ " is not a valid Boolean type")

builtins :: [(String, Value)]
builtins = [
  ("True", makeBool True),
  ("False", makeBool False),
  -- TODO actually plus should be polymorphic... but for now let's skip this
  ("+", (make2ArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x + y)),
  ("*", (make2ArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x * y)),
  ("-", (make2ArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x - y)),
  ("/", (make2ArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x `div` y)),
  ("<+>", (make2ArgFun $ \(VStr a) -> \(VStr b) -> VStr $ a ++ b)),
  ("==", make2ArgFun $ \x -> \y -> makeBool $ x == y),
  ("<=", make2ArgFun $ \(VInt x) -> \(VInt y) -> makeBool $ x <= y),
  (">=", make2ArgFun $ \(VInt x) -> \(VInt y) -> makeBool $ x >= y),
  ("<", make2ArgFun $ \(VInt x) -> \(VInt y) -> makeBool $ x < y),
  (">", make2ArgFun $ \(VInt x) -> \(VInt y) -> makeBool $ x > y),
  ("if_then_else", (make3ArgFunLazy $ \cond -> \tt -> \ff -> do cond' <- force cond; return $ if unpackBool cond' then tt else ff)),
  ("error", VFunction "msg" emptyEnv $ do (VStr msg) <- readVar "msg"; throwError msg),
  ("toString", make1ArgFunLazy $ \v -> do s <- ishow JustShow v; makeLazy $ return $ VStr s),
  ("print", make1ArgFun $ \(VStr msg) -> VIO (makeLazy $ do liftIO $ putStrLn msg; return $ VUnit)),
  ("readLine", VIO (makeLazy $ VStr <$> (liftIO getLine))),
  (">>=", make2ArgFunLazy sequenceIO),
  (">>", make2ArgFunLazy sequenceIODiscard),
  ("return", make1ArgFunLazy $ \v -> makeLazy $ return $ VIO (return v)),
  ("limit", make2ArgFunLazy runWithLimit),
  ("countInstructions", make1ArgFunLazy countInstructions),
  ("seq", make2ArgFunLazy $ \a -> \b -> makeLazy $ do _ <- force a; force b),
  ("Nil", makeNil),
  ("Cons", make2ArgFun makeCons)
           ] where
  sequenceIO :: LazyValue -> LazyValue -> Interpreter LazyValue
  sequenceIO m' f' = makeLazy $ return $ VIO $ makeLazy $ do
    (VIO m) <- force m' -- compute 1st arg
    left <- m >>= force -- force first IO computation
    (VFunction argname funEnv computation) <- force f' -- compute 2nd arg
    (VIO res) <- inOtherEnv funEnv $ withVal argname (Lazy undefined $ return left) computation >>= force -- run 2nd arg with 1st's argument (force IO) and unpack it's result
    res >>= force
  sequenceIODiscard :: LazyValue -> LazyValue -> Interpreter LazyValue
  sequenceIODiscard a b = makeLazy $ return $ VIO $ makeLazy $ do
    (VIO ia) <- force a
    _ <- ia >>= force -- we force here so that the computation surely runs, TODO might want to reconsider this though
    -- the goal is to have print "a" >> print "b" work correctly, but ideally (return ???) >> print "a" should not crash (which it does now)
    (VIO ib) <- force b
    ib >>= force
  runWithLimit :: LazyValue -> LazyValue -> Interpreter LazyValue
  runWithLimit llimit lval = makeLazy $ do
    (VInt limit) <- force llimit
    -- traceShowM ("Will set limit to ", limit)
    result <- handleOutOfFuel $ withFuelLimit (fromInteger limit) (force lval)
    -- traceShowM ("Limit of ", limit, " has ended")
    return $ makeMaybe result
  countInstructions :: LazyValue -> Interpreter LazyValue
  countInstructions lval = makeLazy $ do
    start <- gets usedFuel
    _ <- force lval -- TODO change to returning a tuple with result
    end <- gets usedFuel
    return $ VInt $ fromIntegral (end - start)

-- def print(msg: String): IO () = ???;
-- def readLine() : IO String = ???;
-- defop >>= [a][b](m: IO a, f: (a -> IO b)): IO b = ???;

instance Eq Value where
  (VStr a) == (VStr b) = a == b
  (VInt a) == (VInt b) = a == b
  (VDouble a) == (VDouble b) = a == b
  (VFunction _ _ _) == _ = error "Cannot compare functions" -- TODO promote this to interpreter error instead of crash
  VUnit == VUnit = True
  (VDataType constr1 args1) == (VDataType constr2 args2) = constr1 == constr2 && args1 == args2
  _ == _ = False
