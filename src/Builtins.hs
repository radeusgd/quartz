module Builtins(
  loadBuiltinDecls,
  withBuiltins,
  builtinsEnv,
  builtinsModuleName
               ) where
import Prelude hiding (mod)
import qualified Quartz.Syntax.AbsQuartz as Abs
import qualified Quartz.Syntax.ParQuartz as Par
import qualified Quartz.Syntax.ErrM as Err
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import AST.Desugared
import Passes.Desugar
import System.IO
import System.IO.Error
import Interpreter
import Linker
import Constants

loadBuiltinDecls :: IO [Declaration]
loadBuiltinDecls = do
  stdlib <- findStdLib
  fd <- openFile (stdlib ++ "/" ++ builtinsModuleName ++ ".quartz") ReadMode
  contents <- hGetContents fd
  let toks = Par.myLexer contents
  let ed = (\(Abs.Prog [] decls) -> decls) <$> Par.pProgram toks
  case ed of
    Err.Bad s -> error $ "Fatal error: syntax error in Builtins.quartz: " ++ s
    Err.Ok decls -> return $ map desugarDeclaration decls

register :: String -> Env -> (String, Value) -> Interpreter Env
register mod env (name, v) = do
  loc <- alloc
  setValueEager loc v
  return $ bind (IQualified mod name) loc env

-- TODO it would be good to make sure all builtins have respective declarations and vice versa
builtinsEnv :: Interpreter Env
builtinsEnv = do
  env <- ask
  foldM (register builtinsModuleName) env builtins

withBuiltins :: Interpreter a -> Interpreter a
withBuiltins m = do
  env <- builtinsEnv
  inOtherEnv env m

make1ArgFunLazy :: (LazyValue -> Interpreter LazyValue) -> Value
make1ArgFunLazy f = VFunction "x" emptyEnv $ do
  x <- readVarLazy $ IDefault "x"
  f x

make2ArgFunLazy :: (LazyValue -> LazyValue -> Interpreter LazyValue) -> Value
make2ArgFunLazy f = VFunction "x" emptyEnv $ do
  x <- readVarLazy $ IDefault "x"
  makeLazy $ return $ VFunction "y" emptyEnv $ do
    y <- readVarLazy $ IDefault "y"
    f x y

make3ArgFunLazy :: (LazyValue -> LazyValue -> LazyValue -> Interpreter LazyValue) -> Value
make3ArgFunLazy f = VFunction "z" emptyEnv $ do
  z <- readVarLazy $ IDefault "z"
  makeLazy $ return $ make2ArgFunLazy (f z)

make1ArgFun :: (Value -> Value) -> Value
make1ArgFun f = VFunction "x" emptyEnv $ do
  x <- readVar $ IDefault "x"
  makeLazy $ return $ f x

make2ArgFun :: (Value -> Value -> Value) -> Value
make2ArgFun f = VFunction "x" emptyEnv $ do
  x <- readVar $ IDefault "x"
  makeLazy $ return $ VFunction "y" emptyEnv $ do
    y <- readVar $ IDefault "y"
    makeLazy $ return $ f x y

make3ArgFun :: (Value -> Value -> Value -> Value) -> Value
make3ArgFun f = VFunction "z" emptyEnv $ do
  z <- readVar $ IDefault "z"
  makeLazy $ return $ make2ArgFun (f z)

makeNothing :: Value
makeNothing = VDataType "Nothing" []

makeJust :: LazyValue -> Interpreter LazyValue
makeJust v = constructDataTypeInstance "Just" [v]

makeMaybe :: Maybe LazyValue -> Interpreter LazyValue
makeMaybe (Just v) = makeJust v
makeMaybe (Nothing) = makeLazy $ return makeNothing

makeNil :: Value
makeNil = VDataType "Nil" []

makeCons :: LazyValue -> LazyValue -> Interpreter LazyValue
makeCons h t = constructDataTypeInstance "Cons" [h, t]

makeBool :: Bool -> Value
makeBool True = VDataType "True" []
makeBool False = VDataType "False" []

unpackBool :: Value -> Bool
unpackBool (VDataType "True" []) = True
unpackBool (VDataType "False" []) = False
unpackBool other = error (show other ++ " is not a valid Boolean type")

makeTupleN :: Integer -> Value
makeTupleN n = go n [] where
  go :: Integer -> [Loc] -> Value
  go 0 vals = VDataType ("Tuple" ++ show n) $ reverse vals
  go k vals = if k < 0 then error "Negative tuple? Nonsense"
    else VFunction "x" emptyEnv $ do
       (loc, _) <- readThunk $ IDefault "x"
       makeLazy $ return $ go (k - 1) (loc:vals)

builtins :: [(String, Value)]
builtins = [
  ("True", makeBool True),
  ("False", makeBool False),
  ("+", (make2ArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x + y)),
  ("*", (make2ArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x * y)),
  ("-", (make2ArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x - y)),
  ("/", (make2ArgFunLazy $ \x' -> \y' -> do (VInt x) <- force x'; (VInt y) <- force y'; if y == 0 then throwError "Division by zero" else makeLazy $ return $ VInt $ x `div` y)),
  ("<+>", (make2ArgFun $ \(VStr a) -> \(VStr b) -> VStr $ a ++ b)),
  ("==", make2ArgFunLazy $ \x -> \y -> makeLazy $ do x' <- force x; y' <- force y; makeBool <$> (x' `compareValues` y')),
  ("<=", make2ArgFun $ \(VInt x) -> \(VInt y) -> makeBool $ x <= y),
  (">=", make2ArgFun $ \(VInt x) -> \(VInt y) -> makeBool $ x >= y),
  ("<", make2ArgFun $ \(VInt x) -> \(VInt y) -> makeBool $ x < y),
  (">", make2ArgFun $ \(VInt x) -> \(VInt y) -> makeBool $ x > y),
  ("if_then_else", (make3ArgFunLazy $ \cond -> \tt -> \ff -> do cond' <- force cond; return $ if unpackBool cond' then tt else ff)),
  ("error", VFunction "msg" emptyEnv $ do (VStr msg) <- readVar $ IDefault "msg"; throwError msg),
  ("toString", make1ArgFunLazy $ \v -> do s <- ishow JustShow v; makeLazy $ return $ VStr s),
  ("print", make1ArgFun $ \(VStr msg) -> VIO (makeLazy $ do liftIO $ putStrLn msg; return $ VUnit)),
  ("readLine", readLine),
  (">>=", make2ArgFunLazy sequenceIO),
  (">>", make2ArgFunLazy sequenceIODiscard),
  ("return", make1ArgFunLazy $ \v -> makeLazy $ return $ VIO (return v)),
  ("limit", make2ArgFunLazy runWithLimit),
  ("countInstructions", make1ArgFunLazy countInstructions),
  ("seq", make2ArgFunLazy $ \a -> \b -> makeLazy $ do _ <- force a; force b),
  ("Nil", makeNil),
  ("Cons", make2ArgFunLazy makeCons),
  ("Tuple1", makeTupleN 1),
  ("Tuple2", makeTupleN 2),
  ("Tuple3", makeTupleN 3),
  ("Tuple4", makeTupleN 4),
  ("Tuple5", makeTupleN 5)
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
    _ <- ia >>= force
    -- the goal is to have print "a" >> print "b" work correctly, but ideally (return ???) >> print "a" should not crash (which it does now)
    (VIO ib) <- force b
    ib >>= force
  runWithLimit :: LazyValue -> LazyValue -> Interpreter LazyValue
  runWithLimit llimit lval = do -- make sure makeLazy is not needed here
    (VInt limit) <- force llimit
    -- traceShowM ("Will set limit to ", limit)
    result <- handleOutOfFuel $ withFuelLimit (fromInteger limit) (force lval)
    -- traceShowM ("Limit of ", limit, " has ended")
    (makeMaybe $ (makeLazyValue <$> result)) -- TODO force
  countInstructions :: LazyValue -> Interpreter LazyValue
  countInstructions lval = do -- and as above, makeLazy, likely not needed
    start <- gets usedFuel
    res <- force lval
    end <- gets usedFuel
    constructDataTypeInstance "Tuple2" [makeLazyValue $ res, makeLazyValue $ VInt $ fromIntegral (end - start)]
  readLine :: Value
  readLine = VIO (makeLazy $ do
                     r <- liftIO $ catchIOError (Right <$> getLine) (\err -> return $ Left $ show err)
                     case r of
                       Left err -> throwError err
                       Right res -> return $ VStr res
                     )

-- def print(msg: String): IO () = ???;
-- def readLine() : IO String = ???;
-- defop >>= [a][b](m: IO a, f: (a -> IO b)): IO b = ???;

compareValues :: Value -> Value -> Interpreter Bool
(VStr a) `compareValues` (VStr b) = return $ a == b
(VInt a) `compareValues` (VInt b) = return $ a == b
(VDouble a) `compareValues` (VDouble b) = return $ a == b
(VFunction _ _ _) `compareValues` _ = throwError "Cannot compare functions"
VUnit `compareValues` VUnit = return True
(VDataType constr1 args1) `compareValues` (VDataType constr2 args2) = do
  args1' <- mapM forceThunk args1
  args2' <- mapM forceThunk args2
  argEqs <- mapM (uncurry compareValues) (zip args1' args2')
  return $ constr1 == constr2 && length args1 == length args2 && all id argEqs
_ `compareValues` _ = return False
