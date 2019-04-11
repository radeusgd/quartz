module Passes.TypeCheck(
  typeCheckTopLevel,
  withTopLevelDecls,
  inferType,
  inferD,
  inferE,
  evalInfer
                       ) where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Maybe

-- import Debug.Trace

import AST.Desugared

data WithContext a = WithContext a String

data TypeError
  = UnboundVariable String
  -- | TooGeneralDeclaration Type Type -- TODO
  -- | TooFewArguments Type Exp Exp
  | OccursCheck Type Type
  | TypeMismatch Type Type
  | TopLevelTypeNotSpecified String
  | Other String

instance Show TypeError where
  show (UnboundVariable v) = "Unbound variable: " ++ v
  -- show (TooGeneralDeclaration dec exp) = "Declaration wants " ++ show dec ++ ", but the expression realizes " ++ show exp ++ " which is not general enough"
  -- show (TooFewArguments s a b) = "Too few arguments, got: " ++ show s ++ ", expected a function when applying " ++ show b ++ " to " ++ show a
  show (OccursCheck a b) = "Type " ++ show a ++ " is part of " ++ show b ++ ", so they cannot be unified"
  show (TypeMismatch a b) = "Cannot unify " ++ show a ++ " with " ++ show b
  show (Other s) = "Other error: " ++ s
  show _ = "TODO unknown error"

instance Show a => Show (WithContext a) where
  show (WithContext a ctx) = show a ++ " in " ++ ctx

data Env = Env { eBindings :: M.Map Ident QualifiedType, eCtx :: String }

emptyEnv :: Env
emptyEnv = Env M.empty "???"

type Subst = M.Map Integer Type
type TCM a = StateT TcState (ReaderT Env (Either (WithContext TypeError))) a
type NameSupply = Integer
data TcState = TcState {
  tcsNameSupply :: NameSupply
}

throwErrorWithContext :: TypeError -> TCM a
throwErrorWithContext e = do
  ctx <- asks eCtx
  throwError $ WithContext e ctx

inContext :: String -> TCM a -> TCM a
inContext c = local (\e -> e { eCtx = c })

extendContext :: String -> TCM a -> TCM a
extendContext c = local (\e -> e { eCtx = c ++ " in " ++ eCtx e })

emptyTcState :: TcState
emptyTcState = TcState 0

-- traceEnv :: TCM ()
-- traceEnv = do
--   env <- asks eBindings
--   trace ("[ENV] " ++ show env) $ return ()

readVar' :: Ident -> TCM QualifiedType
readVar' v = do
  env <- asks eBindings
  case M.lookup v env of
    Just t -> return t
    Nothing -> throwErrorWithContext $ UnboundVariable v

readVar :: Ident -> TCM Type
readVar = readVar' >=> instantiate

withVar :: Ident -> QualifiedType -> TCM a -> TCM a
withVar i t m = local (introduceType i t) m where
  introduceType :: Ident -> QualifiedType -> Env -> Env
  introduceType i qt@(ForAll _ tt) (Env b ctx) = Env (M.insert i qt b) ctx -- not sure if I shouldn't add free variables of tt to Env here?

emptySubst :: Subst
emptySubst = M.empty

compose :: Subst -> Subst -> Subst
compose sa sb = M.union (M.map (substitute sa) sb) (M.map (substitute sb) sa)

(<#>) :: Subst -> Subst -> Subst
(<#>) = compose

bindVar :: Integer -> Type -> TCM Subst
bindVar i t =
  if Set.member i (freeTypeVariables t) then throwErrorWithContext $ OccursCheck (FreeVariable i) t
  else if t == FreeVariable i then return emptySubst -- same variable so no need to make a substitution
  else return $ M.singleton i t

noSubst :: TCM a -> TCM (a, Subst)
noSubst = ((\x -> (x, emptySubst)) <$>)

withTopLevelDecls :: [Declaration] -> TCM a -> TCM a
withTopLevelDecls [] m = m
withTopLevelDecls (Function name _ maytype _ : t) m = case maytype of
  Just qt -> withVar name qt $ withTopLevelDecls t m
  Nothing -> throwErrorWithContext $ TopLevelTypeNotSpecified name

atomsForFreeVars :: [Ident]
atomsForFreeVars = map (\i -> '\'' : show i) ([1..] :: [Integer])

freeTypeVariables :: Type -> Set.Set Integer
freeTypeVariables (Atom _) = Set.empty
freeTypeVariables (Abstraction a b) = freeTypeVariables a `Set.union` freeTypeVariables b
freeTypeVariables (FreeVariable i) = Set.singleton i

substitute :: Subst -> Type -> Type
substitute _ a@(Atom _) = a
substitute m (Abstraction a b) = Abstraction (substitute m a) (substitute m b)
substitute m v@(FreeVariable i) = fromMaybe v $ M.lookup i m

substituteAtoms :: M.Map Ident Type -> Type -> Type
substituteAtoms m a@(Atom i) = case M.lookup i m of
  Just t -> t -- replace atom if it's in substitution map
  Nothing -> a -- otherwise, leave it as-is
substituteAtoms m (Abstraction a b) = Abstraction (substituteAtoms m a) (substituteAtoms m b)
substituteAtoms m fp@(FreeVariable _) = fp

generalize :: Type -> TCM QualifiedType
generalize tt = do
  let ftv = Set.toList $ freeTypeVariables tt
  return $ closeType' ftv tt

closeType :: Type -> QualifiedType
closeType tt = closeType' (Set.toList $ freeTypeVariables tt) tt

closeType' :: [Integer] -> Type -> QualifiedType
closeType' ftv tt = ForAll vars tt' where
   vars = take (length ftv) atomsForFreeVars
   subst = M.fromList $ zip ftv $ map Atom vars
   tt' = substitute subst tt

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

instantiate' :: QualifiedType -> (String -> TCM Type) -> TCM (Type, [Type])
instantiate' (ForAll vars ttype) freshType = do
  substs <- mapM (\i -> freshType i >>= \v -> return (i, v)) vars
  return (substituteAtoms (M.fromList substs) ttype, map snd substs)

instantiate :: QualifiedType -> TCM Type
instantiate qt = fst <$> instantiate' qt (const freshFreeType)

instantiateRigid :: QualifiedType -> TCM Type
instantiateRigid qt = fst <$> instantiate' qt freshRigidType

unify :: Type -> Type -> TCM Subst
unify (FreeVariable i) t = bindVar i t
unify t (FreeVariable i) = bindVar i t
unify (Atom a) (Atom b) | a == b = return emptySubst
unify (Abstraction a1 b1) (Abstraction a2 b2) = do
  s1 <- unify a1 a2
  s2 <- unify (substitute s1 b1) (substitute s1 b2)
  return $ s1 <#> s2
unify ta tb = throwErrorWithContext $ TypeMismatch ta tb

buildLambda :: [Ident] -> Exp -> Exp
buildLambda [] e = e
buildLambda (a:t) e = ELambda a (buildLambda t e)

inferD' :: Declaration -> TCM (Type, Subst)
inferD' (Function name args usertype e) = inContext name $ do
  (ttype, s) <- inferE' $ buildLambda args e
  case usertype of
    Nothing -> return (ttype, s)
    -- make sure user specified type fits with inferred
    Just tt -> do
      tt' <- instantiateRigid tt
      s' <- unify ttype tt'
      return (substitute s' ttype, s <#> s')

inferE' :: Exp -> TCM (Type, Subst)
inferE' (EApplication f arg) = do
  (f', sf) <- inferE' f
  (arg', sa) <- inferE' arg
  res <- freshFreeType
  s <- unify f' (Abstraction arg' res)
  return (substitute s res, sf <#> sa <#> s)
inferE' (EVar v) = noSubst $ readVar v
inferE' (EConst c) = noSubst $ literalType c
inferE' (ELambda x e) = do
  xt <- freshFreeType
  (et, s) <- withVar x (ForAll [] xt) $ inferE' e
  return $ (Abstraction (substitute s xt) (substitute s et), s)
inferE' (EBlock decls e) = inferBlock decls e

inferBlock :: [Declaration] -> Exp -> TCM (Type, Subst)
inferBlock [] e = inferE' e
inferBlock (d@(Function name _ _ _) : tail) e = do
  (declt, ss) <- inferD' d
  d' <- generalize declt
  (bl, sb) <- withVar name d' $ inferBlock tail e
  return (bl, sb <#> ss)

literalType :: Literal -> TCM Type
literalType (LStr _) = return $ Atom "String"
literalType (LInt _) = return $ Atom "Int"
literalType (LDouble _) = return $ Atom "Double"
literalType (LBool _) = return $ Atom "Bool"
literalType (LUnit) = return $ Atom "()"
literalType (LError _) = freshFreeType

inferE :: Exp -> TCM Type
inferE e = fst <$> inferE' e

inferD :: Declaration -> TCM Type
inferD d = fst <$> inferD' d

evalInfer :: TCM a -> Either (WithContext TypeError) a
evalInfer m = runReaderT (evalStateT m emptyTcState) emptyEnv

inferTypes :: TCM [Type] -> Either (WithContext TypeError) [QualifiedType]
inferTypes m = do
  ts <- evalInfer m
  let qts = map closeType ts
  return qts

inferType :: TCM Type -> Either (WithContext TypeError) QualifiedType
inferType m = head <$> inferTypes ((:[]) <$> m)

typeCheckTopLevel :: [Declaration] -> TCM ()
typeCheckTopLevel decls =
  withTopLevelDecls decls (mapM_ inferD' decls)
