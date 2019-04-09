module Passes.TypeCheck where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Map as M

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

type Env = Map Ident Type

type Check = ReaderT Env (Either TypeError)

traceEnv :: Check ()
traceEnv = do
  env <- ask
  trace ("[ENV] " ++ show env) $ return ()

-- checkDeclaration :: Env -> Declaration -> Either TypeError Declaration
-- checkDeclaration env decl = runReaderT (checkDeclaration' decl) env

-- checkExpression :: Env -> Exp -> Either TypeError Exp
-- checkExpression env exp = runReaderT (checkExpression' exp) env

-- takes away n args from the type and returns the rest (return type of n-arg function)
extractArgs :: Int -> Type -> Either TypeError ([Type], Type)
extractArgs 0 t = return ([], t)
extractArgs n t | n > 0 = case t of
                    Abstraction a b -> do
                      (rest, ret) <- extractArgs (n - 1) b
                      return (a : rest, ret)
                    _ -> throwError $ Other "Internal error: function type has less arguments than its arity, shouldn't ever happen"
extractArgs _ _ = throwError $ Other "Internal error: extract args called with negative arguments, shouldn't ever happen"

readVar :: Ident -> Check Type
readVar v = do
  env <- ask
  case M.lookup v env of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable v

