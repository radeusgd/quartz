module Passes.TypeCheck(
  typeCheckTopLevel,
  withTopLevelDecls, -- TODO consider deprecating these
  inferType,
  inferExpType,
  inferE,
  evalInfer,
  extendEnvironment,
  withEnvironment,
  TypeEnv,
  emptyTypeEnv
                       ) where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.List as List
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
  show (TopLevelTypeNotSpecified s) = "Top level definition " ++ s ++ " missing type signature"

instance Show a => Show (WithContext a) where
  show (WithContext a ctx) = show a ++ " in " ++ ctx

type TypeEnv = M.Map Ident QualifiedType
emptyTypeEnv :: TypeEnv
emptyTypeEnv = M.empty

data Env = Env { eBindings :: TypeEnv, eCtx :: String, eFree :: Set.Set Integer, eFreeAtoms :: Set.Set Ident } -- TODO cleanup these

emptyEnv :: Env
emptyEnv = Env M.empty "???" Set.empty Set.empty

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
  introduceType i qt@(ForAll _ tt) (Env b ctx f a) = Env (M.insert i qt b) ctx (freeTypeVariables tt `Set.union` f) a -- not sure if I shouldn't add free variables of tt to Env here?

withVars :: [(Ident, QualifiedType)] -> TCM a -> TCM a
withVars lst m = foldr (uncurry withVar) m lst

emptySubst :: Subst
emptySubst = M.empty

compose :: Subst -> Subst -> Subst
compose sa sb = M.union (M.map (substitute sa) sb) (M.map (substitute sb) sa)

(<#>) :: Subst -> Subst -> Subst
(<#>) = compose

bindVar :: Integer -> Type -> TCM Subst
bindVar i t =
  if t == FreeVariable i then return emptySubst -- same variable so no need to make a substitution
  else if Set.member i (freeTypeVariables t) then throwErrorWithContext $ OccursCheck (FreeVariable i) t
  else return $ M.singleton i t

noSubst :: TCM a -> TCM (a, Subst)
noSubst = ((\x -> (x, emptySubst)) <$>)

withTopLevelDecls :: [Declaration] -> TCM a -> TCM a
withTopLevelDecls [] m = m
withTopLevelDecls (Function name _ maytype _ : t) m = case maytype of
  Just qt -> withTopLevelDecls t $ withVar name qt m
  Nothing -> throwErrorWithContext $ TopLevelTypeNotSpecified name
withTopLevelDecls (d@(DataType _ _ _) : t) m =
  -- TODO once we have kind checking we need to tell ourselves about our existence so recursive types work
  withTopLevelDecls t $ withDeclaration d m

atomsForFreeVars :: [Ident]
atomsForFreeVars = map (\i -> '\'' : show i) ([1..] :: [Integer])

freeTypeVariables :: Type -> Set.Set Integer
freeTypeVariables (Atom _) = Set.empty
freeTypeVariables (Abstraction a b) = freeTypeVariables a `Set.union` freeTypeVariables b
freeTypeVariables (Construction a b) = freeTypeVariables a `Set.union` freeTypeVariables b
freeTypeVariables (FreeVariable i) = Set.singleton i

substitute :: Subst -> Type -> Type
substitute _ a@(Atom _) = a
substitute m (Abstraction a b) = Abstraction (substitute m a) (substitute m b)
substitute m (Construction a b) = Construction (substitute m a) (substitute m b)
substitute m v@(FreeVariable i) = fromMaybe v $ M.lookup i m

substituteAtoms :: M.Map Ident Type -> Type -> Type
substituteAtoms m a@(Atom i) = case M.lookup i m of
  Just t -> t -- replace atom if it's in substitution map
  Nothing -> a -- otherwise, leave it as-is
substituteAtoms m (Abstraction a b) = Abstraction (substituteAtoms m a) (substituteAtoms m b)
substituteAtoms m (Construction a b) = Construction (substituteAtoms m a) (substituteAtoms m b)
substituteAtoms m fp@(FreeVariable _) = fp

withFreeAtoms :: [Ident] -> TCM a -> TCM a
withFreeAtoms atoms m = local (\s -> s { eFreeAtoms = eFreeAtoms s `Set.union` Set.fromList atoms }) m

generalize :: Type -> TCM QualifiedType
generalize tt = do
  freeAts <- asks eFreeAtoms
  let ftv = Set.toList $ freeTypeVariables tt
  let (ForAll vars tt') = closeType' ftv tt
  let freeAtoms = freeAts `Set.intersection` allAtoms tt'
  let allVars = freeAtoms `Set.union` Set.fromList vars
  return $ ForAll (Set.toList allVars) tt'
  where
    allAtoms (Atom a) = Set.singleton a
    allAtoms (Abstraction a b) = allAtoms a `Set.union` allAtoms b
    allAtoms (Construction a b) = allAtoms a `Set.union` allAtoms b
    allAtoms (FreeVariable _) = Set.empty

-- TODO also caonicalize free variable names to a,b,c,...,'1,'2,...
closeType :: Type -> QualifiedType
closeType tt =
  let ForAll vars tt' = closeType' (Set.toList $ freeTypeVariables tt) tt in
    ForAll (Set.toList (freeAtoms tt' `Set.union` Set.fromList vars)) tt'
    where
      freeAtoms (Atom i@('\'' : _)) = Set.singleton i
      freeAtoms (Atom _) = Set.empty
      freeAtoms (Abstraction a b) = Set.union (freeAtoms a) (freeAtoms b)
      freeAtoms (Construction a b) = Set.union (freeAtoms a) (freeAtoms b)
      freeAtoms (FreeVariable _) = Set.empty

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
unify (Construction a1 b1) (Construction a2 b2) = unifyPairs (a1, a2) (b1, b2)
unify (Abstraction a1 b1) (Abstraction a2 b2) = unifyPairs (a1, a2) (b1, b2)
unify ta tb = throwErrorWithContext $ TypeMismatch ta tb

unifyPairs :: (Type, Type) -> (Type, Type) -> TCM Subst
unifyPairs (a1, a2) (b1, b2) = do
  s1 <- unify a1 a2
  s2 <- unify (substitute s1 b1) (substitute s1 b2)
  return $ s1 <#> s2


unifyMany :: [Type] -> TCM Subst
unifyMany [] = return emptySubst
unifyMany [_] = return emptySubst
unifyMany [a, b] = unify a b
unifyMany (a:b:t) = do
  s <- unify a b
  s' <- unifyMany (substitute s b : t)
  return $ s <#> s'

buildLambda :: [Ident] -> Exp -> Exp
buildLambda [] e = e
buildLambda (a:t) e = ELambda a (buildLambda t e)

-- inferFunction :: Declaration -> TCM (Type, Subst)
-- inferFunction (Function name args usertype e) = 

inferE' :: Exp -> TCM (Type, Subst)
inferE' (EApplication f arg) = do
  (f', sf) <- inferE' f
  (arg', sa) <- inferE' arg
  res <- freshFreeType
  -- traceShowM ("app", f', arg')
  s <- unify f' (Abstraction arg' res)
  return (substitute s res, sf <#> sa <#> s)
inferE' (EVar v) = noSubst $ readVar v
inferE' (EConst c) = noSubst $ literalType c
inferE' (ELambda x e) = do
  xt <- freshFreeType
  (et, s) <- withVar x (ForAll [] xt) $ inferE' e
  return $ (Abstraction (substitute s xt) (substitute s et), s)
inferE' (ECaseOf e cases) = do
  (inpT, outT, sc) <- inferCases cases
  (e', se) <- inferE' e
  su <- unify inpT e'
  let s' = se <#> su
  return (substitute s' outT, sc <#> s')
inferE' (EBlock decls e) = inferBlock decls e

-- TODO these may be more complex for polymorphic types
getConstructorType :: ECase -> TCM Type
getConstructorType (ECase name _ _) = do
  constructor <- readVar name
  return $ getLastType constructor
  where
    getLastType (Abstraction _ b) = getLastType b
    getLastType other = other

getCaseArgTypes :: ECase -> TCM [(Ident, QualifiedType)]
getCaseArgTypes (ECase name args _) = do
  constructor <- readVar name
  let argtypes = getFirstNTypes (length args) constructor
  return $ zip args $ map (ForAll []) argtypes
  where
    getFirstNTypes 0 _ = []
    getFirstNTypes 1 (Abstraction a b) = [a]
    getFirstNTypes 1 other = [other]
    getFirstNTypes n (Abstraction a b) = a : getFirstNTypes (n-1) b
    getFirstNTypes _ _ = error "Constructor arity error"

inferCaseResult :: ECase -> TCM (Type, Subst)
inferCaseResult c@(ECase _ _ e) = do
  typedargs <- getCaseArgTypes c
  withVars typedargs $ inferE' e

-- infers the input type of case set and its output type
inferCases :: [ECase] -> TCM (Type, Type, Subst)
inferCases cases = do
  inputs <- mapM getConstructorType cases
  si <- unifyMany inputs
  let inputType = head inputs
  (outputTypes, substs) <- unzip <$> mapM inferCaseResult cases
  let sr = List.foldl' (<#>) emptySubst substs
  so <- unifyMany outputTypes
  let s' = si <#> so <#> sr
  let outputType = head outputTypes
  return (substitute s' inputType, substitute s' outputType, s')

withDeclaration :: Declaration -> TCM a -> TCM a
withDeclaration d m = fst <$> withDeclaration' d m

-- TODO this freeAtoms over Expressions and Declarations should be refactored into some kind of fmap
freeAtomsQT :: M.Map Ident Type -> QualifiedType -> QualifiedType
freeAtomsQT m (ForAll vars tt) =
  let m' = foldr (M.delete) m vars in
    ForAll vars (substituteAtoms m' tt)

freeAtomsD :: M.Map Ident Type -> Declaration -> Declaration
freeAtomsD m (Function name args maytype body) = Function name args (freeAtomsQT m <$> maytype) (freeAtomsE m body)
freeAtomsD m (DataType _ _ _) = error "TODO"

freeAtomsE :: M.Map Ident Type -> Exp -> Exp
freeAtomsE m (EApplication a b) = EApplication (freeAtomsE m a) (freeAtomsE m b)
freeAtomsE m (ELambda x e) = ELambda x (freeAtomsE m e)
freeAtomsE m (ECaseOf e cases) = ECaseOf (freeAtomsE m e) (map freeAtomsCase cases) where
  freeAtomsCase (ECase n args e) = ECase n args $ freeAtomsE m e
freeAtomsE m (EBlock decls e) = EBlock (map (freeAtomsD m) decls) (freeAtomsE m e)
freeAtomsE m v@(EVar _) = v
freeAtomsE m c@(EConst _) = c

freeTheAtoms :: [Ident] -> Exp -> TCM Exp
freeTheAtoms atoms exp = do
  subst <- mapM (\i -> do t <- freshFreeType; return (i, t)) atoms
  return $ freeAtomsE (M.fromList subst) exp

withDeclaration' :: Declaration -> TCM a -> TCM (a, Subst)
withDeclaration' (Function name args usertype body) m = do
  (ttype, subst) <- case usertype of -- this implementation would allow for recursion only if the signature is provided
    Nothing -> inferred body
    Just t@(ForAll vars _) -> do
      body' <- freeTheAtoms vars body
      withVar name t $ inferred body'
  -- body' <- case usertype of -- this could allow for recursion without explicit type signature
  --   Nothing -> return body
  --   Just (ForAll vars _) -> freeTheAtoms vars body
  -- rectype <- freshFreeType
  -- (ttype, subst) <- withVar name (ForAll [] rectype) $ inferred body'
  -- subst'' <- 
  -- let qtype = closeType ttype
  qtype <- generalize ttype
  -- traceShowM ("withdecl", name, ttype, qtype)
  res <- withVar name qtype $ m
  return (res, subst)
  where inferred body =
          inContext name $ do
          (ttype, s) <- inferE' $ buildLambda args body
          case usertype of
            Nothing -> return (ttype, s)
              -- make sure user specified type fits with inferred
            Just tt@(ForAll vars _) -> do
              tt' <- instantiateRigid tt
              s' <- unify ttype tt'
              let ts = substitute s' ttype
              return (ts, s <#> s')

withDeclaration' (DataType dataTypeName typeArgs cases) m = do
  r <- foldr withConstructor m cases
  return (r, emptySubst) -- no substitutions here as types are already generic, TODO: right?
  where
    withConstructor (DataTypeCase name argtypes) =
      withVar name $ ForAll typeArgs (buildConstructor argtypes $ dataTypeInstanceType typeArgs) -- TODO polymorphic datatypes
    buildConstructor [] resultType = resultType
    buildConstructor (h:t) resultType = Abstraction h (buildConstructor t resultType)
    dataTypeInstanceType args = List.foldl' (\t -> \a -> Construction t (Atom a)) (Atom dataTypeName) args

inferBlock :: [Declaration] -> Exp -> TCM (Type, Subst)
inferBlock [] e = inferE' e
inferBlock (d : tail) e = do
  ((bl, s1), s2) <- withDeclaration' d $ inferBlock tail e
  return (substitute s2 bl, s1 <#> s2)

literalType :: Literal -> TCM Type
literalType (LStr _) = return $ Atom "String"
literalType (LInt _) = return $ Atom "Int"
literalType (LDouble _) = return $ Atom "Double"
literalType (LBool _) = return $ Atom "Bool"
literalType (LUnit) = return $ Atom "()"
literalType (LError _) = freshFreeType

inferE :: Exp -> TCM Type
inferE e = fst <$> inferE' e

inferExpType :: Exp -> TCM QualifiedType
inferExpType e = closeType <$> inferE e

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
  withTopLevelDecls decls (inferBlock decls (EConst $ LUnit)) >> return () -- dummy expression, we just want to make sure everything typechecks

extendEnvironment :: TypeEnv -> [Declaration] -> TCM TypeEnv
extendEnvironment e decls = withEnvironment e $ withTopLevelDecls decls $ typeCheckTopLevel decls >> asks eBindings

withEnvironment :: TypeEnv -> TCM a -> TCM a
withEnvironment te = local (\_ -> Env te "???" Set.empty Set.empty)
