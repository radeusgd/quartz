module Interpreter where

import Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Debug.Trace

import qualified AST.Desugared as Desugared
import AST.Desugared

-- I base the laziness on memory immutability
data LazyValue = Lazy Env (Interpreter Value)

data Value
  = VStr String
  | VInt Integer
  | VDouble Double
  | VBool Bool
  -- function: argname, definition_environemnt (my closure), computation
  | VFunction String Env (Interpreter LazyValue)
  | VDataType Ident [Value] -- right now datatypes are strict (so forcing an instance of a datatpye forces all of it's arguments), this disallows infinite lists for example

instance Show Value where
  show (VStr s) = show s
  show (VInt i) = show i
  show (VDouble d) = show d
  show (VBool b) = show b
  show (VFunction arg _ _) = "λ" ++ arg ++ ". [function body]"

type ErrorType = String

type Loc = Int
data Env = Env { vars :: Map String Loc }
data Thunk = ThunkLazy LazyValue | ThunkComputed Value
instance Show Thunk where
  show (ThunkLazy _) = "[unevaluated thunk]"
  show (ThunkComputed v) = show v

data Memory = Memory { locs :: Map Loc Thunk, maxloc :: Loc }

type InState = StateT Memory (ExceptT ErrorType Identity)
type Interpreter = ReaderT Env InState

-- traceEnv :: Env -> Interpreter ()
-- traceEnv e = do
--   mem <- gets locs
--   traceShowM (bind e mem)
--   where
--     bind :: Env -> Map Loc Value -> Map String (Maybe Value)
--     bind e m = fmap (\loc -> M.lookup loc m) (vars e)

emptyEnv :: Env
emptyEnv = Env M.empty

emptyMemory :: Memory
emptyMemory = Memory M.empty 0

fromLiteral :: Desugared.Literal -> Interpreter Value
fromLiteral (LStr s) = return $ VStr s
fromLiteral (LInt s) = return $ VInt s
fromLiteral (LDouble s) = return $ VDouble s
fromLiteral (LBool s) = return $ VBool s
fromLiteral (LError s) = throwError s

getJustOrError :: MonadError e m => e -> Maybe a -> m a
getJustOrError _ (Just v) = return v
getJustOrError err Nothing = throwError err

readThunk :: String -> Interpreter (Loc, Thunk)
readThunk v = do
  env <- asks vars
  let locM = M.lookup v env
  loc <- getJustOrError ("Variable " ++ v ++ " not in scope (why typecheck didn't catch this?)") locM
  mem <- gets locs
  let valM = M.lookup loc mem
  thunk <- getJustOrError ("Memory location for variable " ++ v ++ " seems to be not allocated, this shouldn't happen.") valM
  return (loc, thunk)

readVar :: String -> Interpreter Value
readVar v = do
  (loc, thunk) <- readThunk v
  case thunk of
    ThunkComputed val -> return val
    ThunkLazy lv -> do
      val <- force lv
      modify (\s -> s { locs = M.insert loc (ThunkComputed val) (locs s) })
      return val

readVarLazy :: String -> Interpreter LazyValue
readVarLazy v = makeLazy $ readVar v

alloc :: Interpreter Loc
alloc = lift $ alloc'

alloc' :: InState Loc
alloc' = do
  m <- gets maxloc
  modify $ \s -> s { maxloc = m + 1 }
  return m


bind :: String -> Loc -> Env -> Env
bind name loc env = env { vars = M.insert name loc (vars env) }

setValue :: Loc -> LazyValue -> Interpreter ()
setValue loc val = lift $ setValue' loc val

setValueEager :: Loc -> Value -> Interpreter ()
setValueEager loc val = modify (\s -> s { locs = M.insert loc (ThunkComputed val) (locs s) })

setValue' :: Loc -> LazyValue -> InState ()
setValue' loc val = modify go where
  go s = s { locs = M.insert loc (ThunkLazy val) (locs s) }

force :: LazyValue -> Interpreter Value
force (Lazy e vi) = local (\_ -> e) vi

makeLazy :: Interpreter Value -> Interpreter LazyValue
makeLazy i = do
  env <- ask
  return $ Lazy env i

processDefinition :: Env -> Declaration -> Interpreter Env
processDefinition env (Function name args tt exp) = do
  loc <- alloc
  let env' = bind name loc env
  val <- local (\_ -> env') $ buildFunction args exp
  setValue loc val
  return env' where
    buildFunction :: [String] -> Exp -> Interpreter LazyValue
    buildFunction [] e = interpret e
    -- TODO actually want to use env'
    buildFunction [h] e =
      do
      env <- ask
      makeLazy $ return $ VFunction h env (interpret e)
    buildFunction (h:t) e = do
      env <- ask
      makeLazy $ return $ VFunction h env (buildFunction t e)

interpret :: Exp -> Interpreter LazyValue
interpret (EConst  c) = makeLazy $ fromLiteral c
interpret (EBlock decls e) = do
  env <- ask
  innerEnv <- foldM processDefinition env decls
  local (\_ -> innerEnv) $ interpret e
interpret (EVar v) = makeLazy $ readVar v
interpret (ELambda argname exp) = do
  env <- ask
  makeLazy $ return $ VFunction argname env (interpret exp)
interpret (EApplication fun arg) = makeLazy $ do
  fun' <- interpret fun >>= force
  arg' <- interpret arg
  case fun' of
    (VFunction argname funEnv computation) -> do
      argloc <- alloc
      setValue argloc arg'
      let innerEnv = bind argname argloc funEnv
      -- traceEnv innerEnv
      local (\_ -> innerEnv) computation >>= force -- TODO do we want to force here? FIXME likely not
    _ -> throwError "Trying to apply to a non-function (why didn't typechecker catch this?)"

runInterpreter :: Interpreter a -> Either ErrorType a
runInterpreter i = fst <$> execInterpreter emptyEnv emptyMemory i

withDeclared :: [Declaration] -> Interpreter a -> Interpreter a
withDeclared [] i = i
withDeclared (h:t) i = do
  env <- ask
  env' <- processDefinition env h
  local (\_ -> env') $ withDeclared t i

execInterpreter :: Env -> Memory -> Interpreter a -> Either ErrorType (a, Memory)
execInterpreter env mem i = runIdentity $ runExceptT $ runStateT (runReaderT i env) mem
