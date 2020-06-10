module Interpreter where

import Prelude hiding(mod, exp)
import Data.Map as M
import Data.List as List
import qualified ModuleMap as MM
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import qualified AST.Desugared as Desugared
import AST.Desugared

-- I base the laziness on memory immutability
data LazyValue = Lazy Env (Interpreter Value)

data Value
  = VStr String
  | VInt Integer -- TODO probably change into Int
  | VDouble Double
  | VUnit
  -- function: argname, definition_environemnt (my closure), computation
  | VFunction String Env (Interpreter LazyValue)
  | VDataType Ident [Loc] -- right now datatypes are strict (so forcing an instance of a datatpye forces all of it's arguments), this disallows infinite lists for example -- TODO change this to Loc
  | VIO (Interpreter LazyValue) -- it was prettier when we didn't have IO in interpreter and put it only here but this seems to be easier

instance Show Value where
  show (VStr s) = show s
  show (VInt i) = show i
  show (VDouble d) = show d
  show (VUnit) = "()"
  show (VFunction arg _ _) = "λ" ++ arg ++ ". [function body]"
  show (VIO _) = "IO ???"
  show (VDataType name vs) = name ++ "%% "++ show vs ++ "%%" -- these percentages mean it's locs



data ShowMode = RunIO | JustShow

class IShow a where
  ishow :: ShowMode -> a -> Interpreter String

instance IShow Value where
  ishow showmode d@(VDataType caseName args) = case caseName of -- TODO do I want to inherit showmode? should a function returning Just IO be evaluated? probably yes
    "Cons" -> unpackList d >>= ishow showmode
    "Nil" -> return "[]"
    _ -> do
      args' <- mapM (forceThunk >=> ishow showmode) args
      return $ caseName ++ case args' of
          [] -> ""
          _ -> "[" ++ intercalate "," args' ++ "]"
    where
      unpackList :: Value -> Interpreter [Value]
      unpackList (VDataType "Cons" [v, t]) = do
        v' <- forceThunk v
        t' <- forceThunk t
        t'' <- unpackList t'
        return $ v' : t''
      unpackList (VDataType "Nil" []) = return []
      unpackList _ = error "Malformed list represtantation"
  ishow RunIO (VIO computation) = do
    res <- computation
    ishow RunIO res
  ishow _ other = return $ show other

instance IShow LazyValue where
  ishow mode = force >=> ishow mode

instance IShow a => IShow [a] where
  ishow mode lst = do
    es <- mapM (ishow mode) lst
    return $ "[" ++ intercalate "," es ++ "]"

type ErrorType = String

type Loc = Int
data Env = Env { vars :: MM.ModuleMap Loc, fuelLimit :: Maybe Int }
data Thunk = ThunkLazy LazyValue | ThunkComputed Value
instance Show Thunk where
  show (ThunkLazy _) = "[unevaluated thunk]"
  show (ThunkComputed v) = show v

data Memory = Memory { thunks :: Map Loc Thunk, maxloc :: Loc, usedFuel :: Int }

type InState = StateT Memory (ExceptT ErrorType IO)
type Interpreter = ReaderT Env InState

-- TODO maybe make ErrorType into Either String OutOfFuel and handle this more cleanly?
outOfFuelMessage :: String
outOfFuelMessage = "~~OUT OF FUEL~~"

isFuelLimitExceeded :: Interpreter Bool
isFuelLimitExceeded = do
  used <- gets usedFuel
  limit <- asks fuelLimit
  case limit of
    Nothing -> return False
    Just l -> return (used >= l)

useFuel :: Int -> Interpreter ()
useFuel units = do
  modify $ \s -> s { usedFuel = usedFuel s + units }
  -- used <- gets usedFuel
  -- limit <- asks fuelLimit
  -- traceShowM ("Using ", units, "; ", used, "/", limit)
  exceeded <- isFuelLimitExceeded
  if exceeded then throwError outOfFuelMessage else return ()

withFuelLimit :: Int -> Interpreter a -> Interpreter a
withFuelLimit units m = do
  used <- gets usedFuel
  local (\r -> r { fuelLimit = calcNewLimit used (fuelLimit r) }) $ m
  where
    calcNewLimit used currentLimit = let newLimit = used + units in
      case currentLimit of
        Nothing -> Just newLimit
        Just someLimit -> Just $ min someLimit newLimit

handleOutOfFuel :: Interpreter a -> Interpreter (Maybe a)
handleOutOfFuel m = (Just <$> m) `catchError` handleError where
  handleError msg = if msg /= outOfFuelMessage then throwError msg -- rethrow other errors
    else do
       exceeded <- isFuelLimitExceeded
       if exceeded then throwError msg
       else return Nothing -- if limit is not exceeded at our level, recover from error, but return Nothing as the inner computation has failed

-- traceEnv :: Env -> Interpreter ()
-- traceEnv e = do
--   mem <- gets locs
--   traceShowM (bind e mem)
--   where
--     bind :: Env -> Map Loc Value -> Map String (Maybe Value)
--     bind e m = fmap (\loc -> M.lookup loc m) (vars e)

emptyEnv :: Env
emptyEnv = Env MM.empty Nothing

emptyMemory :: Memory
emptyMemory = Memory M.empty 0 0

inOtherEnv :: Env -> Interpreter a -> Interpreter a
inOtherEnv env = local (\r -> env { fuelLimit = fuelLimit r} ) -- change to different env but preserve the fuelLimit

localEnv :: (MM.ModuleMap Loc -> MM.ModuleMap Loc) -> Interpreter a -> Interpreter a
localEnv mod = local (\r -> r { vars = mod (vars r) })

fromLiteral :: Desugared.Literal -> Interpreter Value
fromLiteral (LStr s) = return $ VStr s
fromLiteral (LInt s) = return $ VInt s
fromLiteral (LDouble s) = return $ VDouble s
fromLiteral (LError s) = throwError s
fromLiteral (LUnit) = return VUnit

getJustOrError :: MonadError e m => e -> Maybe a -> m a
getJustOrError _ (Just v) = return v
getJustOrError err Nothing = throwError err

raiseEitherToError :: MonadError e m => Either e a -> m a
raiseEitherToError (Left e) = throwError e
raiseEitherToError (Right a) = return a

readThunk :: QualifiedIdent -> Interpreter (Loc, Thunk)
readThunk v = do
  env <- asks vars
  loc <- raiseEitherToError $ MM.lookup v env
  mem <- gets thunks
  let valM = M.lookup loc mem
  thunk <- getJustOrError ("Memory location for variable " ++ show v ++ " seems to be not allocated, this shouldn't happen.") valM
  return (loc, thunk)

readVar :: QualifiedIdent -> Interpreter Value
readVar v = do
  (loc, _) <- readThunk v
  forceThunk loc

forceThunk :: Loc -> Interpreter Value
forceThunk loc = do
  mem <- gets thunks
  let valM = M.lookup loc mem
  thunk <- getJustOrError ("Memory location " ++ show loc ++ " is read from, but hasn't been allocated. This shouldn't happen") valM
  case thunk of
    ThunkComputed val -> return val
    ThunkLazy lv -> do
      val <- force lv
      modify (\s -> s { thunks = M.insert loc (ThunkComputed val) (thunks s) })
      return val

readVarLazy :: QualifiedIdent -> Interpreter LazyValue
readVarLazy v = makeLazy $ readVar v

alloc :: Interpreter Loc
alloc = lift $ alloc'

alloc' :: InState Loc
alloc' = do
  m <- gets maxloc
  modify $ \s -> s { maxloc = m + 1 }
  return m


bind :: QualifiedIdent -> Loc -> Env -> Env
bind name loc env = env { vars = MM.insert name loc (vars env) }

setValue :: Loc -> LazyValue -> Interpreter ()
setValue loc val = lift $ setValue' loc val

setValueEager :: Loc -> Value -> Interpreter ()
setValueEager loc val = modify (\s -> s { thunks = M.insert loc (ThunkComputed val) (thunks s) })

setValue' :: Loc -> LazyValue -> InState ()
setValue' loc val = modify go where
  go s = s { thunks = M.insert loc (ThunkLazy val) (thunks s) }

force :: LazyValue -> Interpreter Value
force (Lazy e vi) = inOtherEnv e vi

makeLazy :: Interpreter Value -> Interpreter LazyValue
makeLazy i = do
  env <- ask
  return $ Lazy env i

makeLazyValue :: Value -> LazyValue
makeLazyValue v = Lazy (emptyEnv) (return v)

buildNArgFunction :: Int -> ([LazyValue] -> Interpreter LazyValue) -> Interpreter LazyValue
buildNArgFunction 0 builder = builder []
buildNArgFunction n builder = makeLazy $ return $ VFunction "x" emptyEnv $ do
  x <- readVarLazy $ IDefault "x"
  buildNArgFunction (n - 1) (\args -> builder (x:args))

processDefinition :: Maybe Ident -> Env -> Declaration -> Interpreter Env
processDefinition mod env def = processDefinitions mod env [def]

processDefinitions :: Maybe Ident -> Env -> [Declaration] -> Interpreter Env
processDefinitions mod env0 defs = do
  locs <- mapM allocDefinition defs
  let defsWithLocs = zip locs defs
  let bindDefs e = List.foldl' (\env -> \(loc, def) -> bindDefintion mod def loc env) e defsWithLocs
  local bindDefs $ mapM_ (uncurry setDefinitionValue) defsWithLocs
  return $ bindDefs env0

allocDefinition :: Declaration -> Interpreter [Loc]
allocDefinition (Function _ _ _ _) = (:[]) <$> alloc
allocDefinition (DataType _ _ cases) = mapM (\_ -> alloc) cases

bindDefintion :: Maybe Ident -> Declaration -> [Loc] -> Env -> Env
bindDefintion mod (Function name _ _ _) [loc] env = bind (qualifyIdent mod name) loc env
bindDefintion _ (Function _ _ _ _) _ _ = error "Wrong amount of locations prepared for function"
bindDefintion mod (DataType _ _ cases) locs env0 = do
  let casesWithLocs = zip cases locs
  List.foldl' (\env -> \(DataTypeCase caseName _, loc) -> bind (qualifyIdent mod caseName) loc env) env0 casesWithLocs

setDefinitionValue :: [Loc] -> Declaration -> Interpreter ()
setDefinitionValue [loc] (Function _ args _ exp) = do
  val <- buildFunction args exp
  setValue loc val
  where
    buildFunction :: [String] -> Exp -> Interpreter LazyValue
    buildFunction [] e = interpret e
    buildFunction [h] e =
      do
      env <- ask
      makeLazy $ return $ VFunction h env (interpret e)
    buildFunction (h:t) e = do
      env <- ask
      makeLazy $ return $ VFunction h env (buildFunction t e)
setDefinitionValue _ (Function _ _ _ _) = error "Wrong amount of locations prepared for function"
setDefinitionValue locations (DataType _ _ cases) = do
  let casesWithLocs = zip cases locations
  mapM_ defineCase casesWithLocs
  where
    defineCase :: (DataTypeCase, Loc) -> Interpreter ()
    defineCase (DataTypeCase caseName argtypes, loc) = do
      constructor <- buildNArgFunction (length argtypes) (constructDataTypeInstance caseName)
      setValue loc constructor

constructDataTypeInstance :: Ident -> [LazyValue] -> Interpreter LazyValue
constructDataTypeInstance caseName args = makeLazy $ do
  args' <- mapM saveLazyThunk args
  return $ VDataType caseName args'

saveLazyThunk :: LazyValue -> Interpreter Loc
saveLazyThunk lv = do
  loc <- alloc
  setValue loc lv
  return loc

interpret :: Exp -> Interpreter LazyValue
interpret (EConst  c) = makeLazy $ fromLiteral c
interpret (EBlock decls e) = do
  let introDef m decl = do env <- ask; env' <- processDefinition Nothing env decl; inOtherEnv env' m
  List.foldr (flip introDef) (interpret e) decls
interpret (EVar v) = makeLazy $ readVar v
interpret (ELambda argname exp) = do
  env <- ask
  makeLazy $ return $ VFunction argname env (interpret exp)
interpret (ECaseOf exp cases) = do
  matched <- interpret exp >>= force
  case matched of
    (VDataType constructorName args) -> matchCase constructorName args cases
    _ -> throwError "Trying to match over a non-data type (why didn't typechecker catch this?)"
  where
    matchCase _ _ [] =
      throwError "Non-exhaustive pattern match"
    -- TODO add support for pattern matching from different modules
    matchCase _ _ (ECase (IQualified _ _) _ _:_) = throwError "Pattern matching over qualified names is not implemented. Ensure your datatypes have unique constructor names."
    matchCase constructorName dataargs (ECase (IDefault name) args e:t) =
      if name /= constructorName then matchCase constructorName dataargs t
      else if length dataargs /= length args then throwError "Datatype arguments count mismatch (data representation and case match have different number of arguments), shouldn't ever happen"
      else do
        withLocs (zip args dataargs) $ interpret e

interpret (EApplication fun arg) = makeLazy $ do
  fun' <- interpret fun >>= force
  arg' <- interpret arg
  useFuel 1
  case fun' of
    (VFunction argname funEnv computation) -> inOtherEnv funEnv $
      withVal argname arg' computation >>= force
    _ -> throwError $ "Trying to apply to a non-function (" ++ show fun ++ ") (why didn't typechecker catch this?)"

withVals :: [(Ident, LazyValue)] -> Interpreter a -> Interpreter a
withVals lst m = List.foldr (uncurry withVal) m lst

withVal :: Ident -> LazyValue -> Interpreter a -> Interpreter a
withVal name val m = do
  loc <- alloc
  setValue loc val
  local (bind (IDefault name) loc) m

withValsEager :: [(Ident, Value)] -> Interpreter a -> Interpreter a
withValsEager lst m = List.foldr (uncurry withValEager) m lst

withValEager :: Ident -> Value -> Interpreter a -> Interpreter a
withValEager name val m = do
  loc <- alloc
  setValueEager loc val
  local (bind (IDefault name) loc) m

-- data is immutable so we can reuse already used locations to give them as arguments to other functions, because only thing that they can change is make them evaluated
withLoc :: Ident -> Loc -> Interpreter a -> Interpreter a
withLoc name loc m = local (bind (IDefault name) loc) m

withLocs :: [(Ident, Loc)] -> Interpreter a -> Interpreter a
withLocs lst m = List.foldr (uncurry withLoc) m lst

runInterpreter :: Interpreter a -> IO (Either ErrorType a)
runInterpreter i = do
  r <- execInterpreter emptyEnv emptyMemory i
  return $ fst <$> r

withDeclared :: Maybe Ident -> [Declaration] -> Interpreter a -> Interpreter a
withDeclared _ [] i = i
withDeclared mod (h:t) i = do
  env <- ask
  env' <- processDefinition mod env h
  inOtherEnv env' $ withDeclared mod t i

execInterpreter :: Env -> Memory -> Interpreter a -> IO (Either ErrorType (a, Memory))
execInterpreter env mem i = runExceptT $ runStateT (runReaderT i env) mem
