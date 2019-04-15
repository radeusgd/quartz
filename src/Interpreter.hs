module Interpreter where

import Data.Map as M
import Data.List as List
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
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
  | VUnit
  -- function: argname, definition_environemnt (my closure), computation
  | VFunction String Env (Interpreter LazyValue)
  | VDataType Ident [Value] -- right now datatypes are strict (so forcing an instance of a datatpye forces all of it's arguments), this disallows infinite lists for example -- TODO change this to Loc
  | VIO (Interpreter LazyValue) -- it was prettier when we didn't have IO in interpreter and put it only here but this seems to be easier

instance Show Value where
  show (VStr s) = show s
  show (VInt i) = show i
  show (VDouble d) = show d
  show (VBool b) = show b
  show (VUnit) = "()"
  show (VFunction arg _ _) = "λ" ++ arg ++ ". [function body]"
  show d@(VDataType caseName args) = case caseName of
    "Cons" -> show $ unpackList d
    _ -> caseName ++ show args
  show (VIO _) = "IO ???"

unpackList :: Value -> [Value]
unpackList (VDataType "Cons" [v, tail]) = v : unpackList tail
unpackList (VDataType "Nil" []) = []
unpackList _ = error "Malformed list represtantation"


data ShowMode = RunIO | JustShow

class IShow a where
  ishow :: ShowMode -> a -> Interpreter String

instance IShow Value where
  -- ishow _ (VDataType caseName args) = return $ caseName ++ show args -- TODO when changing to lazy datatypes change this
  ishow RunIO (VIO computation) = do
    res <- computation
    ishow RunIO res
  ishow _ other = return $ show other

instance IShow LazyValue where
  ishow mode = force >=> ishow mode

type ErrorType = String

type Loc = Int
data Env = Env { vars :: Map String Loc }
data Thunk = ThunkLazy LazyValue | ThunkComputed Value
instance Show Thunk where
  show (ThunkLazy _) = "[unevaluated thunk]"
  show (ThunkComputed v) = show v

data Memory = Memory { locs :: Map Loc Thunk, maxloc :: Loc }

type InState = StateT Memory (ExceptT ErrorType IO)
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
fromLiteral (LUnit) = return VUnit

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

buildNArgFunction :: Int -> ([LazyValue] -> Interpreter LazyValue) -> Interpreter LazyValue
buildNArgFunction 0 builder = builder []
buildNArgFunction n builder = makeLazy $ return $ VFunction "x" emptyEnv $ do
  x <- readVarLazy "x"
  buildNArgFunction (n - 1) (\args -> builder (x:args))

processDefinition :: Env -> Declaration -> Interpreter Env
processDefinition env (Function name args _ exp) = do
  loc <- alloc
  let env' = bind name loc env
  val <- local (\_ -> env') $ buildFunction args exp
  setValue loc val
  return env' where
    buildFunction :: [String] -> Exp -> Interpreter LazyValue
    buildFunction [] e = interpret e
    buildFunction [h] e =
      do
      env <- ask
      makeLazy $ return $ VFunction h env (interpret e)
    buildFunction (h:t) e = do
      env <- ask
      makeLazy $ return $ VFunction h env (buildFunction t e)

processDefinition env (DataType name typeargs cases) = do -- TODO are typeargs here needed for anything? likely not
  locs <- mapM (\_ -> alloc) cases
  let casesWithLocs = zip cases locs
  let env' = List.foldl' (\env -> \(DataTypeCase name _, loc) -> bind name loc env) env casesWithLocs
  local (\_ -> env') $ mapM_ defineCase casesWithLocs
  return env'
  where
    defineCase :: (DataTypeCase, Loc) -> Interpreter ()
    defineCase (DataTypeCase caseName argtypes, loc) = do
      constructor <- buildNArgFunction (length argtypes) (constructDataTypeInstance caseName)
      setValue loc constructor
    constructDataTypeInstance :: Ident -> [LazyValue] -> Interpreter LazyValue
    constructDataTypeInstance caseName args = makeLazy $ do
      args' <- mapM force args
      return $ VDataType caseName args'

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
interpret (ECaseOf e cases) = do
  matched <- interpret e >>= force
  case matched of
    (VDataType constructorName args) -> matchCase constructorName args cases
    _ -> throwError "Trying to match over a non-data type (why didn't typechecker catch this?)"
  where
    matchCase constructorName dataargs [] =
      throwError "Non-exhaustive pattern match"
    matchCase constructorName dataargs (ECase name args e:t) =
      if name /= constructorName then matchCase constructorName dataargs t
      else if length dataargs /= length args then throwError "Datatype arguments count mismatch (data representation and case match have different number of arguments), shouldn't ever happen"
      else do
        withValsEager (zip args dataargs) $ interpret e

interpret (EApplication fun arg) = makeLazy $ do
  fun' <- interpret fun >>= force
  arg' <- interpret arg
  case fun' of
    (VFunction argname funEnv computation) -> local (\_ -> funEnv) $
      withVal argname arg' computation >>= force -- TODO not sure if want to leave it like this
    _ -> throwError $ "Trying to apply to a non-function (" ++ show fun ++ ") (why didn't typechecker catch this?)"

withVals :: [(Ident, LazyValue)] -> Interpreter a -> Interpreter a
withVals lst m = List.foldr (uncurry withVal) m lst

withVal :: Ident -> LazyValue -> Interpreter a -> Interpreter a
withVal name val m = do
  loc <- alloc
  setValue loc val
  local (bind name loc) m

withValsEager :: [(Ident, Value)] -> Interpreter a -> Interpreter a
withValsEager lst m = List.foldr (uncurry withValEager) m lst

withValEager :: Ident -> Value -> Interpreter a -> Interpreter a
withValEager name val m = do
  loc <- alloc
  setValueEager loc val
  local (bind name loc) m

runInterpreter :: Interpreter a -> IO (Either ErrorType a)
runInterpreter i = do
  r <- execInterpreter emptyEnv emptyMemory i
  return $ fst <$> r

withDeclared :: [Declaration] -> Interpreter a -> Interpreter a
withDeclared [] i = i
withDeclared (h:t) i = do
  env <- ask
  env' <- processDefinition env h
  local (\_ -> env') $ withDeclared t i

execInterpreter :: Env -> Memory -> Interpreter a -> IO (Either ErrorType (a, Memory))
execInterpreter env mem i = runExceptT $ runStateT (runReaderT i env) mem
