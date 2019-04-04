module Interpreter where

import Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Debug.Trace

import qualified AST.Desugared as Desugared
import AST.Typed

data Value
  = VStr String
  | VInt Integer
  | VDouble Double
  | VBool Bool
  -- function: argname, definition_environemnt (my closure), computation
  | VFunction String Env (Interpreter Value)

instance Show Value where
  show (VStr s) = show s
  show (VInt i) = show i
  show (VDouble d) = show d
  show (VBool b) = show b
  show (VFunction arg _ _) = "λ" ++ arg ++ ". [function body]"

type ErrorType = String

type Loc = Int
data Env = Env { vars :: Map String Loc }
data Memory = Memory { locs :: Map Loc Value, maxloc :: Loc }

type InState = StateT Memory (ExceptT ErrorType Identity)
type Interpreter = ReaderT Env InState

traceEnv :: Env -> Interpreter ()
traceEnv e = do
  mem <- gets locs
  traceShowM (bind e mem)
  where
    bind :: Env -> Map Loc Value -> Map String (Maybe Value)
    bind e m = fmap (\loc -> M.lookup loc m) (vars e)

emptyEnv :: Env
emptyEnv = Env M.empty

emptyMemory :: Memory
emptyMemory = Memory M.empty 0

fromLiteral :: Desugared.Value -> Interpreter Value
fromLiteral (Desugared.VStr s) = return $ VStr s
fromLiteral (Desugared.VInt s) = return $ VInt s
fromLiteral (Desugared.VDouble s) = return $ VDouble s
fromLiteral (Desugared.VBool s) = return $ VBool s
fromLiteral (Desugared.VUndefined) = throwError "Evaluating undefined"

getJustOrError :: MonadError e m => e -> Maybe a -> m a
getJustOrError _ (Just v) = return v
getJustOrError err Nothing = throwError err

readVar :: String -> Interpreter Value
readVar v = do
  env <- asks vars
  let locM = M.lookup v env
  loc <- getJustOrError ("Variable " ++ v ++ " not in scope (why typecheck didn't catch this?)") locM
  mem <- gets locs
  let valM = M.lookup loc mem
  val <- getJustOrError ("Memory location for variable " ++ v ++ " seems to be not allocated, this shouldn't happen.") valM
  return val

alloc :: Interpreter Loc
alloc = lift $ alloc'

alloc' :: InState Loc
alloc' = do
  m <- gets maxloc
  modify $ \s -> s { maxloc = m + 1 }
  return m


bind :: String -> Loc -> Env -> Env
bind name loc env = env { vars = M.insert name loc (vars env) }

setValue :: Loc -> Value -> Interpreter ()
setValue loc val = lift $ setValue' loc val

setValue' :: Loc -> Value -> InState ()
setValue' loc val = modify go where
  go s = s { locs = M.insert loc val (locs s) }

processDefinition :: Env -> Declaration -> Interpreter Env
processDefinition env (Function _ name args exp) = do
  loc <- alloc
  let env' = bind name loc env
  val <- buildFunction args exp
  setValue loc val
  return env'
  where
    buildFunction :: [String] -> Exp -> Interpreter Value
    buildFunction [] e = interpret e
    -- TODO actually want to use env'
    buildFunction [h] e = return $ VFunction h env (interpret e)
    buildFunction (h:t) e = do
      return $ VFunction h env (buildFunction t e)

interpret :: Exp -> Interpreter Value
interpret (EConst _ c) = fromLiteral c
interpret (EBlock decls e) = do
  env <- ask
  innerEnv <- foldM processDefinition env decls
  local (\_ -> innerEnv) $ interpret e
interpret (EVar _ v) = readVar v
interpret (EApplication _ fun arg) = do
  fun' <- interpret fun
  arg' <- interpret arg
  case fun' of
    (VFunction argname funEnv computation) -> do
      argloc <- alloc
      setValue argloc arg'
      let innerEnv = bind argname argloc funEnv
      traceEnv innerEnv
      local (\_ -> innerEnv) computation
    _ -> throwError "Trying to apply to a non-function (why didn't typechecker catch this?)"

runInterpreter :: Interpreter a -> Either ErrorType a
runInterpreter i = runIdentity $ runExceptT $ evalStateT (runReaderT i emptyEnv) emptyMemory

withDeclared :: [Declaration] -> Interpreter a -> Interpreter a
withDeclared [] i = i
withDeclared (h:t) i = do
  env <- ask
  env' <- processDefinition env h
  local (\_ -> env') $ withDeclared t i
