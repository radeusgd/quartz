module Passes.TypeCheck where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans.UnionFind
import qualified Data.Map as M
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe

import Debug.Trace

import AST.Desugared

data TypeError
  = UnboundVariable String
  -- | TooGeneralDeclaration Type Type -- TODO
  -- | TooFewArguments Type Exp Exp
  | TypeMismatch Type Type
  | TopLevelTypeNotSpecified String
  | Other String

instance Show TypeError where
  show (UnboundVariable v) = "Unbound variable: " ++ v
  -- show (TooGeneralDeclaration dec exp) = "Declaration wants " ++ show dec ++ ", but the expression realizes " ++ show exp ++ " which is not general enough"
  -- show (TooFewArguments s a b) = "Too few arguments, got: " ++ show s ++ ", expected a function when applying " ++ show b ++ " to " ++ show a
  show (TypeMismatch a b) = "Cannot unify " ++ show a ++ " with " ++ show b
  show (Other s) = "Other error: " ++ s
  show _ = "TODO unknown error"

data Env = Env { eBindings :: M.Map Ident QualifiedType, eFreeVars :: Set.Set Integer } -- eFreeVars can be computed from eBindings but are kept 'cached' for performance reasons

emptyEnv :: Env
emptyEnv = Env M.empty Set.empty

-- sygnatura TCM inspirowana slajdami
type TCM a = StateT TcState (ReaderT Env (Either TypeError)) a
type NameSupply = Integer
data TcState = TcState {
  tcsNameSupply :: NameSupply,
  tcsConstraints :: [(Type, Type)]
}

emptyTcState :: TcState
emptyTcState = TcState 0 []

-- traceEnv :: TCM ()
-- traceEnv = do
--   env <- ask
--   trace ("[ENV] " ++ show env) $ return ()

readVar' :: Ident -> TCM QualifiedType
readVar' v = do
  env <- asks eBindings
  case M.lookup v env of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable v

readVar :: Ident -> TCM Type
readVar = readVar' >=> instantiate

withVar :: Ident -> QualifiedType -> TCM a -> TCM a
withVar i t m = local (introduceType i t) m where
  introduceType :: Ident -> QualifiedType -> Env -> Env
  introduceType i qt@(ForAll _ tt) (Env b f) = Env (M.insert i qt b) (freeTypeVariables tt `Set.union` f)

withTopLevelDecls :: [Declaration] -> TCM a -> TCM a
withTopLevelDecls [] m = m
withTopLevelDecls (Function name _ maytype _ : t) m = case maytype of
  Just qt -> withVar name qt $ withTopLevelDecls t m
  Nothing -> throwError $ TopLevelTypeNotSpecified name

freeVarsInEnv :: TCM (Set.Set Integer)
freeVarsInEnv = asks eFreeVars

atomsForFreeVars :: [Ident]
atomsForFreeVars = map (\i -> '\'' : show i) ([1..] :: [Integer])

freeTypeVariables :: Type -> Set.Set Integer
freeTypeVariables (Atom _) = Set.empty
freeTypeVariables (Abstraction a b) = freeTypeVariables a `Set.union` freeTypeVariables b
freeTypeVariables (FreeVariable i) = Set.singleton i

substituteFreeVariables :: M.Map Integer Type -> Type -> Type
substituteFreeVariables _ a@(Atom _) = a
substituteFreeVariables m (Abstraction a b) = Abstraction (substituteFreeVariables m a) (substituteFreeVariables m b)
substituteFreeVariables m v@(FreeVariable i) = fromMaybe v $ M.lookup i m

substituteAtoms :: M.Map Ident Type -> Type -> Type
substituteAtoms m a@(Atom i) = case M.lookup i m of
  Just t -> t -- replace atom if it's in substitution map
  Nothing -> a -- otherwise, leave it as-is
substituteAtoms m (Abstraction a b) = Abstraction (substituteAtoms m a) (substituteAtoms m b)
substituteAtoms m fp@(FreeVariable _) = fp

generalize :: Type -> TCM QualifiedType
generalize tt = do
  fte <- freeVarsInEnv
  let ftv = Set.toList $ freeTypeVariables tt `Set.difference` fte
  return $ closeType' ftv tt

closeType :: Type -> QualifiedType
closeType tt = closeType' (Set.toList $ freeTypeVariables tt) tt

closeType' :: [Integer] -> Type -> QualifiedType
closeType' ftv tt = ForAll vars tt' where
   vars = take (length ftv) atomsForFreeVars
   subst = M.fromList $ zip ftv $ map Atom vars
   tt' = substituteFreeVariables subst tt

freshFreeType :: TCM Type
freshFreeType = do
  i <- gets tcsNameSupply
  modify $ \s -> s { tcsNameSupply = i + 1 }
  return $ FreeVariable i

freshRigidType :: String -> TCM Type
freshRigidType str = do
  i <- gets tcsNameSupply
  modify $ \s -> s { tcsNameSupply = i + 1 }
  return $ Atom ('\'' : show i ++ str)

instantiate :: QualifiedType -> TCM Type
instantiate qt = fst <$> instantiate' qt (const freshFreeType)

instantiate' :: QualifiedType -> (String -> TCM Type) -> TCM (Type, [Type]) -- TODO maybe remove this...
instantiate' (ForAll vars ttype) freshType = do
  substs <- mapM (\i -> freshType i >>= \v -> return (i, v)) vars
  return (substituteAtoms (M.fromList substs) ttype, map snd substs)

instantiateRigid :: QualifiedType -> TCM Type
instantiateRigid qt = fst <$> instantiate' qt freshRigidType

-- rigidify :: [Type] -> Env -> Env
-- rigidify vars e = e { eFreeVars = Set.union (Set.fromList ints) (eFreeVars e) } where
--   ints = map fvtoi vars
--   fvtoi (FreeVariable i) = i
--   fvtoi _ = error "Rigidify called on a non-FV"

unify :: Type -> Type -> TCM ()
unify ta tb =
  modify $ \s -> s { tcsConstraints = (ta, tb) : tcsConstraints s }

buildLambda :: [Ident] -> Exp -> Exp
buildLambda [] e = e
buildLambda (a:t) e = ELambda a (buildLambda t e)

inferD :: Declaration -> TCM Type
inferD (Function name args usertype e) = do
  ttype <- inferE $ buildLambda args e
  case usertype of
    Nothing -> return ()
    -- make sure user specified type fits with inferred
    Just tt -> do
      tt' <- instantiateRigid tt
      unify ttype tt'
  return ttype

inferE :: Exp -> TCM Type
inferE (EApplication f arg) = do
  f' <- inferE f
  arg' <- inferE arg
  res <- freshFreeType
  unify f' (Abstraction arg' res)
  return res
inferE (EVar v) = readVar v
inferE (EConst c) = literalType c
inferE (ELambda x e) = do
  xt <- freshFreeType
  et <- withVar x (ForAll [] xt) $ inferE e
  return $ Abstraction xt et
inferE (EBlock decls e) = inferBlock decls e

inferBlock :: [Declaration] -> Exp -> TCM Type
inferBlock [] e = inferE e
inferBlock (d@(Function name _ _ _) : tail) e = do
  declt <- inferD d
  d' <- generalize declt
  withVar name d' $ inferBlock tail e -- TODO this generalize looks very suspicious :/

literalType :: Literal -> TCM Type
literalType (LStr _) = return $ Atom "String"
literalType (LInt _) = return $ Atom "Int"
literalType (LDouble _) = return $ Atom "Double"
literalType (LBool _) = return $ Atom "Bool"
literalType (LUnit) = return $ Atom "()"
literalType (LError _) = freshFreeType
-- checkDeclaration :: Env -> Declaration -> Either TypeError Declaration
-- checkDeclaration env decl = runReaderT (checkDeclaration' decl) env

-- checkExpression :: Env -> Exp -> Either TypeError Exp
-- checkExpression env exp = runReaderT (checkExpression' exp) env

-- checkDeclaration :: Env -> Declaration -> Either TypeError Declaration
-- checkDeclaration env decl = runReaderT (checkDeclaration' decl) env

-- checkExpression :: Env -> Exp -> Either TypeError Exp
-- checkExpression env exp = runReaderT (checkExpression' exp) env

type Subst = M.Map Integer Type
type Unify = ExceptT TypeError (UnionFindT Type Identity)
type Unify' = ReaderT (M.Map Integer (Point Type)) Unify

compose :: Subst -> Subst -> Subst
compose sa sb = M.union (M.map (substituteFreeVariables sa) sb) (sa)

findRepr :: Integer -> Unify' (Point Type)
findRepr i = do
  env <- ask
  lift $ lift $ repr $ env M.! i

bindVar :: Integer -> Type -> Unify' ()
bindVar i ttype = do
  -- traceShowM (i, ttype)
  vr <- findRepr i
  tt <- lift $ lift $ descriptor vr -- find current eq class representative
  case tt of
    -- if it's an equivalence class of variables, bind it to the given type
    FreeVariable _ -> lift $ lift $ do
      tr <- fresh ttype
      union vr tr -- tr is second so that the equivalence class will have the concrete type as it's representative
    -- if the eq class is already bound to a type
    _ -> unifyOne tt ttype

unifyOne :: Type -> Type -> Unify' ()
unifyOne (FreeVariable i) t = bindVar i t
unifyOne t (FreeVariable i) = bindVar i t
unifyOne (Atom a) (Atom b) | a == b = return () -- same atoms are trivially unifiable
unifyOne (Abstraction a1 b1) (Abstraction a2 b2) = unifyMany [(a1, a2), (b1, b2)]
unifyOne ta tb = throwError $ TypeMismatch ta tb

unifyMany :: [(Type, Type)] -> Unify' ()
unifyMany [] = return ()
unifyMany ((a, b) : t) = unifyOne a b >> unifyMany t

makeEqClasses :: Set.Set Integer -> Unify (M.Map Integer (Point Type))
makeEqClasses ints = lift $ M.fromList <$> (mapM (\i -> do f <- fresh (FreeVariable i); return (i, f)) (Set.toList ints))

fetchSubstitutions :: M.Map Integer (Point Type) -> Unify Subst
fetchSubstitutions map = do
    let listDec = M.toList map
    listType <- lift $ mapM (\(i, pt) -> do t <- repr pt >>= descriptor; return (i, t)) listDec
    return $ M.fromList listType

unifyConstraints :: [(Type, Type)] -> Either TypeError Subst
unifyConstraints constraints = runIdentity $ runUnionFind $ runExceptT $ do
    eqs <- makeEqClasses ftv
    runReaderT (unifyMany constraints) eqs
    fetchSubstitutions eqs
  where
    ftv = foldl Set.union Set.empty (map ftvTuple constraints)
    ftvTuple (t1, t2) = Set.union (freeTypeVariables t1) (freeTypeVariables t2)

runInfer :: TCM a -> Either TypeError (a, Subst)
runInfer m = do
  (a, (TcState _ constraints)) <- runReaderT (runStateT m emptyTcState) emptyEnv
  -- traceShowM constraints
  subst <- unifyConstraints constraints
  return (a, subst)

evalInfer :: TCM a -> Either TypeError a
evalInfer m = fst <$> runInfer m

inferTypes :: TCM [Type] -> Either TypeError [QualifiedType]
inferTypes m = do
  (ts, subst) <- runInfer m
  let ts' = map (substituteFreeVariables subst) ts
  let qts = map closeType ts'
  return qts

inferType :: TCM Type -> Either TypeError QualifiedType
inferType m = head <$> inferTypes ((:[]) <$> m)

typeCheckTopLevel :: [Declaration] -> TCM ()
typeCheckTopLevel decls =
  withTopLevelDecls decls (mapM_ inferD decls)
