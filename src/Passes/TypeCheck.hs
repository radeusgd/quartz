module Passes.TypeCheck where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.List as List

import Debug.Trace

import AST.Desugared

data TypeError
  = UnboundVariable String
  | TooGeneralDeclaration Type Type
  | TooFewArguments Type Exp Exp
  | TypeMismatch Type Type
  | TopLevelTypeNotSpecified String
  | Other String

instance Show TypeError where
  show (UnboundVariable v) = "Unbound variable: " ++ v
  show (TooGeneralDeclaration dec exp) = "Declaration wants " ++ show dec ++ ", but the expression realizes " ++ show exp ++ " which is not general enough"
  show (TooFewArguments s a b) = "Too few arguments, got: " ++ show s ++ ", expected a function when applying " ++ show b ++ " to " ++ show a
  show (TypeMismatch a b) = "Cannot unify " ++ show a ++ " with " ++ show b
  show (Other s) = "Other error: " ++ s
  show _ = "TODO unknown error"

type Env = M.Map Ident QualifiedType

-- sygnatura TCM inspirowana slajdami
type TCM a = ExceptT TypeError (StateT TcState (Reader Env)) a
type NameSupply = Integer
data TcState = TcState {
  tcsNameSupply :: NameSupply,
  tcsConstraints :: [(Type, Type)]
}

-- traceEnv :: TCM ()
-- traceEnv = do
--   env <- ask
--   trace ("[ENV] " ++ show env) $ return ()

readVar' :: Ident -> TCM QualifiedType
readVar' v = do
  env <- ask
  case M.lookup v env of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable v

readVar :: Ident -> TCM Type
readVar = readVar' >=> instantiate

withVar :: Ident -> QualifiedType -> TCM a -> TCM a
withVar i t m = local (M.insert i t) m

atomsForFreeVars :: [Ident]
atomsForFreeVars = map (\i -> '\'' : show i) ([1..] :: [Integer])

freeTypeVariables :: Type -> [Integer]
freeTypeVariables (Atom _) = []
freeTypeVariables (Abstraction a b) = List.nub $ freeTypeVariables a ++ freeTypeVariables b -- TODO probably better use Set?
freeTypeVariables (FreeVariable i) = [i]

substituteFreeVariables :: M.Map Integer Type -> Type -> Type
substituteFreeVariables _ a@(Atom _) = a
substituteFreeVariables m (Abstraction a b) = Abstraction (substituteFreeVariables m a) (substituteFreeVariables m b)
substituteFreeVariables m (FreeVariable i) = m M.! i

substituteAtoms :: M.Map Ident Type -> Type -> Type
substituteAtoms m a@(Atom i) = case M.lookup i m of
  Just t -> t -- replace atom if it's in substitution map
  Nothing -> a -- otherwise, leave it as-is
substituteAtoms m (Abstraction a b) = Abstraction (substituteAtoms m a) (substituteAtoms m b)
substituteAtoms m fp@(FreeVariable _) = fp

generalize :: Type -> QualifiedType
generalize tt =  ForAll vars tt' where
  ftv = freeTypeVariables tt
  vars = take (length ftv) atomsForFreeVars
  subst = M.fromList $ zip ftv $ map Atom vars
  tt' = substituteFreeVariables subst tt

freshType :: TCM Type
freshType = do
  i <- gets tcsNameSupply
  modify $ \s -> s { tcsNameSupply = i + 1}
  return $ FreeVariable i

freshQualifiedType :: QualifiedType
freshQualifiedType = ForAll ["'a"] (Atom "'a")

instantiate :: QualifiedType -> TCM Type
instantiate (ForAll vars ttype)= do
  substs <- mapM (\i -> freshType >>= \v -> return (i, v)) vars
  return $ substituteAtoms (M.fromList substs) ttype

unify :: Type -> Type -> TCM ()
unify ta tb =
  modify $ \s -> s { tcsConstraints = (ta, tb) : tcsConstraints s }

buildLambda :: [Ident] -> Exp -> Exp
buildLambda [] e = e
buildLambda (a:t) e = ELambda a (buildLambda t e)

inferD :: Declaration -> TCM Type
inferD (Function name args usertype e) = do
  ttype <- inferE $ buildLambda args e
  traverse (instantiate >=> unify ttype) usertype -- make sure user specified type fits with inferred
  return ttype

inferE :: Exp -> TCM Type
inferE (EApplication f arg) = do
  f' <- inferE f
  arg' <- inferE arg
  res <- freshType
  unify f' (Abstraction arg' res)
  return res
inferE (EVar v) = readVar v
inferE (EConst c) = literalType c
inferE (ELambda x e) = do
  xt <- freshType
  et <- withVar x (ForAll [] xt) $ inferE e
  return $ Abstraction xt et
inferE (EBlock decls e) = inferBlock decls e

inferBlock :: [Declaration] -> Exp -> TCM Type
inferBlock [] e = inferE e
inferBlock (d@(Function name _ _ _) : tail) e = do
  declt <- inferD d
  withVar name (generalize declt) $ inferBlock tail e -- TODO this generalize looks very suspicious :/

literalType :: Literal -> TCM Type
literalType (LStr _) = return $ Atom "String"
literalType (LInt _) = return $ Atom "Int"
literalType (LDouble _) = return $ Atom "Double"
literalType (LBool _) = return $ Atom "Bool"
literalType (LUnit) = return $ Atom "()"
literalType (LError _) = freshType
-- checkDeclaration :: Env -> Declaration -> Either TypeError Declaration
-- checkDeclaration env decl = runReaderT (checkDeclaration' decl) env

-- checkExpression :: Env -> Exp -> Either TypeError Exp
-- checkExpression env exp = runReaderT (checkExpression' exp) env

-- checkDeclaration :: Env -> Declaration -> Either TypeError Declaration
-- checkDeclaration env decl = runReaderT (checkDeclaration' decl) env

-- checkExpression :: Env -> Exp -> Either TypeError Exp
-- checkExpression env exp = runReaderT (checkExpression' exp) env

type Subst = M.Map Integer Type
type Unify = Either TypeError

compose :: Subst -> Subst -> Subst
compose sa sb = M.union (M.map (substituteFreeVariables sa) sb) (sa)

bindVar :: Integer -> Type -> Subst
bindVar i ttype = M.fromList [(i, ttype)]

solveUnify :: Type -> Type -> Unify Subst
solveUnify (FreeVariable i) t = return $ bindVar i t
solveUnify t (FreeVariable i) = return $ bindVar i t
solveUnify (Abstraction a1 b1) (Abstraction a2 b2) = unifyConstraints [(a1, a2), (b1, b2)]
solveUnify ta tb = throwError $ TypeMismatch ta tb

unifyConstraints :: [(Type, Type)] -> Unify Subst
unifyConstraints [] = return M.empty
unifyConstraints ((a,b) : t) = do
  fst <- solveUnify a b
  rest <- unifyConstraints t
  return $ compose fst rest
