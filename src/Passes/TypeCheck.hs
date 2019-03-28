module Passes.TypeCheck(
  checkDeclaration
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Map as M

import AST.Typed

data TypeError = Other String deriving Show

type Env = Map Ident Type

type Check = ReaderT Env (Either TypeError)

checkDeclaration :: Declaration -> Either TypeError Declaration
checkDeclaration decl = runReaderT (checkDeclaration' decl) empty

-- takes away n args from the type and returns the rest (return type of n-arg function)
extractArgs :: Int -> Type -> Either TypeError ([Type], Type)
extractArgs 0 t = return ([], t)
extractArgs n t | n > 0 = case t of
                    Abstraction a b -> do
                      (rest, ret) <- extractArgs (n - 1) b
                      return (a : rest, ret)
                    _ -> throwError $ Other "Internal error: function type has less arguments than its arity, shouldn't ever happen"

introduceArgs :: [(Ident, Type)] -> Env -> Env
introduceArgs [] e = e
introduceArgs ((name, ttype):tail) e = introduceArgs tail (insert name ttype e)

unify :: Type -> Type -> Check Type
unify a b = if a == b then return a
            else throwError $ Other "Simple unification failed, TODO"

unifyDeclared :: Type -> Type -> Check Type
unifyDeclared decl inner = do
  u <- unify decl inner
  if u /= decl then throwError $ Other "TODO: declared type is too general"
  else return u

checkDeclaration' :: Declaration -> Check Declaration
checkDeclaration' (Function ttype name args exp) = do
  (argtypes, returntype) <- lift $ extractArgs (length args) ttype
  exp' <- local (introduceArgs $ zip args argtypes) (checkExpression exp)
  let exptype = typeOfE exp'
  u <- unifyDeclared returntype exptype
  return $ Function u name args exp'

checkExpression :: Exp -> Check Exp
checkExpression e = throwError $ Other "TODO exp"
