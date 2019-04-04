module Passes.EmbedTypes(
  embedDeclaration,
  embedExp
                       ) where

import Control.Monad.State
import Control.Monad.Identity

import AST.Desugared as D
import AST.Typed as T


type WithFreeParams = StateT Integer Identity

getFreeParam :: WithFreeParams Integer
getFreeParam = do
  p <- get
  modify (+1)
  return p

getFreshType :: WithFreeParams T.Type
getFreshType = getFreeParam >>= return . T.FreeParameter

typeOfV :: Value -> WithFreeParams T.Type
typeOfV (VStr _) = return $ T.Atom "String"
typeOfV (VInt _) = return $ T.Atom "Int"
typeOfV (VDouble _) = return $ T.Atom "Double"
typeOfV (VBool _) = return $ T.Atom "Bool"
typeOfV VUndefined = getFreshType

embedDeclaration :: D.Declaration -> T.Declaration
embedDeclaration decl =
  runIdentity $ evalStateT (embedDeclaration' decl) 0

embedExp :: D.Exp -> T.Exp
embedExp exp =
  runIdentity $ evalStateT (embedExp' exp) 0

embedDeclaration' :: D.Declaration -> WithFreeParams T.Declaration
embedDeclaration' (D.Function name args ttype exp) = do
  type' <- embedType ttype
  let args' = map embedArg args
  exp' <- embedExp' exp
  return $ T.Function type' name args' exp'
  where
    embedArg :: D.Arg -> Ident
    embedArg (Argument i) = i

embedType :: D.Type -> WithFreeParams T.Type
embedType (D.Atom i) = return $ T.Atom i
embedType (D.Abstraction a b) = liftM2 T.Abstraction (embedType a) (embedType b)
embedType (D.Unbound _) = getFreshType -- TODO not sure if just discarding these is a good idea, but may work

embedExp' :: D.Exp -> WithFreeParams T.Exp
embedExp' (D.EApplication a b) = do
  a' <- embedExp' a
  b' <- embedExp' b
  t <- getFreshType
  return $ T.EApplication t a' b'
embedExp' (D.EVar i) = do
  t <- getFreshType
  return $ T.EVar t i
embedExp' (D.EConst v) = do
  t <- typeOfV v
  return $ T.EConst t v
embedExp' (D.EBlock decls exp) = do
  decls' <- mapM embedDeclaration' decls
  exp' <- embedExp' exp
  return $ T.EBlock decls' exp'
