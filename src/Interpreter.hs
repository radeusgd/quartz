module Interpreter where

import Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

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
alloc = do
  m <- gets maxloc
  modify $ \s -> s { maxloc = m + 1 }
  return m

bind :: String -> Loc -> Env -> Env
bind name loc env = env { vars = M.insert name loc (vars env) }

setValue :: Loc -> Value -> Interpreter ()
setValue loc val = modify go where
  go s = s { locs = M.insert loc val (locs s) }

processDefinition :: Env -> Declaration -> InState Env
processDefinition env decl = return env -- TODO

interpret :: Exp -> Interpreter Value
interpret (EConst _ c) = fromLiteral c
interpret (EBlock decls e) = do
  env <- ask
  innerEnv <- lift $ foldM processDefinition env decls
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
      local (\_ -> innerEnv) computation
    _ -> throwError "Trying to apply to a non-function (why didn't typechecker catch this?)"
