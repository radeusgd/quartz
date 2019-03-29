module Passes.TypeCheck(
  checkDeclaration,
  Env
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Map as M

import Debug.Trace

import AST.Typed

data TypeError
  = UnboundVariable String
  | TooGeneralDeclaration Type Type
  | TooFewArguments Type Exp Exp
  | TypeMismatch Type Type
  | Other String

instance Show TypeError where
  show (UnboundVariable v) = "Unbound variable: " ++ v
  show (TooGeneralDeclaration dec exp) = "Declaration wants " ++ show dec ++ ", but the expression realizes " ++ show exp ++ " which is not general enough"
  show (TooFewArguments s a b) = "Too few arguments, got: " ++ show s ++ ", expected a function when applying " ++ show b ++ " to " ++ show a
  show (TypeMismatch a b) = "Cannot unify " ++ show a ++ " with " ++ show b
  show (Other s) = "Other error: " ++ s

type Env = Map Ident Type

type Check = ReaderT Env (Either TypeError)

traceEnv :: Check ()
traceEnv = do
  env <- ask
  trace ("[ENV] " ++ show env) $ return ()

checkDeclaration :: Env -> Declaration -> Either TypeError Declaration
checkDeclaration env decl = runReaderT (checkDeclaration' decl) env

-- takes away n args from the type and returns the rest (return type of n-arg function)
extractArgs :: Int -> Type -> Either TypeError ([Type], Type)
extractArgs 0 t = return ([], t)
extractArgs n t | n > 0 = case t of
                    Abstraction a b -> do
                      (rest, ret) <- extractArgs (n - 1) b
                      return (a : rest, ret)
                    _ -> throwError $ Other "Internal error: function type has less arguments than its arity, shouldn't ever happen"
extractArgs _ _ = throwError $ Other "Internal error: extract args called with negative arguments, shouldn't ever happen"

introduceArgs :: [(Ident, Type)] -> Env -> Env
introduceArgs [] e = e
introduceArgs ((name, ttype):tail) e =
  introduceArgs tail (insert name ttype e)

introduceDeclaration :: Declaration -> Env -> Env
introduceDeclaration (Function ttype name _ _) e =
  insert name ttype e

-- we always choose the smaller of two ids so unification should converge to same results ??? TODO prove
unify :: Type -> Type -> Check Type
unify (FreeParameter a) (FreeParameter b) = return $ FreeParameter $ min a b
unify (FreeParameter _) t = return t
unify t (FreeParameter _) = return t
unify ta@(Atom a) tb@(Atom b) = if a == b then return ta
                               else throwError $ TypeMismatch ta tb
unify fa@(Abstraction a1 a2) fb@(Abstraction b1 b2) = do -- TODO I think in general it will be more complex but in STLC it should be ok
  u1 <- unify a1 b1
  u2 <- unify a2 b2
  return $ Abstraction u1 u2
unify ta tb = throwError $ TypeMismatch ta tb -- wrong shapes

unifyDeclared :: Type -> Type -> Check Type
unifyDeclared decl inner = do
  u <- unify decl inner
  if u /= decl then throwError $ TooGeneralDeclaration decl inner
  else return u

reconstructFunctionType :: [Type] -> Type -> Type
reconstructFunctionType [] ret = ret
reconstructFunctionType (h:t) ret = Abstraction h (reconstructFunctionType t ret)

checkDeclaration' :: Declaration -> Check Declaration
checkDeclaration' (Function ttype name args exp) = do
  (argtypes, returntype) <- lift $ extractArgs (length args) ttype
  exp' <- local (introduceArgs $ zip args argtypes) (checkExpression exp)
  let exptype = typeOfE exp'
  u <- unifyDeclared returntype exptype
  let ftype = reconstructFunctionType argtypes u
  return $ Function ftype name args exp'

readVar :: Ident -> Check Type
readVar v = do
  env <- ask
  case M.lookup v env of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable v

checkExpression :: Exp -> Check Exp
checkExpression c@(EConst _ _) = return c -- consts always have known type
checkExpression (EVar t v) = do
  vt <- readVar v
  u <- unify t vt
  return $ EVar u v
checkExpression (EApplication t a b) = do
  a' <- checkExpression a
  b' <- checkExpression b
  case typeOfE a' of
    (Abstraction x y) -> do
      _ <- unify (typeOfE b') x -- check if arguments match
      return $ EApplication y a' b'
    atom@(Atom _) -> throwError $ TooFewArguments atom a' b'
    (FreeParameter _) -> throwError $ Other "TODO: fresh parameters in abstraction not implemented"
checkExpression (EBlock decls exp) = do
  (decls', exp') <- checkBlock decls exp
  return $ EBlock decls' exp'
  where
    checkBlock :: [Declaration] -> Exp -> Check ([Declaration], Exp)
    checkBlock [] exp = do
      exp' <- checkExpression exp
      return ([], exp')
    checkBlock (decl:t) exp = do
      d' <- checkDeclaration' decl
      (t', exp') <- local (introduceDeclaration d') $ checkBlock t exp
      return (d' : t', exp')
