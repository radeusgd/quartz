{-# LANGUAGE OverloadedStrings #-}
module AST.Desugared where

import Data.Text.Prettyprint.Doc

type Ident = String

data Value = VStr String | VInt Integer | VDouble Double | VUndefined deriving Show

data Type = Atom Ident | Abstraction Type Type | Unbound Integer deriving Show

data Declaration = Function Ident [Arg] Type Exp deriving Show
-- type of functions is the type of the whole function in contrast to just return type that was the case in Abstract Syntax
-- TODO values may be 0-arg functions, but evaluation should be lazy then

data Arg = Argument Ident -- TODO default values are discarded for now
  deriving Show

data Exp
  = EApplication Exp Exp
  | EVar Ident
  | EConst Value
  | EBlock [Declaration] Exp
  deriving Show

instance Pretty Value where
  pretty (VStr s) = pretty s
  pretty (VInt i) = pretty i
  pretty (VDouble d) = pretty d
  pretty VUndefined = "???"

instance Pretty Type where
  pretty (Atom ident) = pretty ident
  pretty (Abstraction a b) = (pretty a) <+> "->" <+> (pretty b)
  pretty (Unbound i) = pretty ("?t" ++ show i ++ "?")

instance Pretty Arg where
  pretty (Argument ident) = pretty ident

instance Pretty Declaration where
  pretty (Function name args ttype exp) = "def" <+> pretty name <+> parens (pretty args) <+> ":" <+> pretty ttype <+> "=" <+> pretty exp

instance Pretty Exp where
  pretty (EApplication a b) = parens (pretty a <+> pretty b)
  pretty (EVar v) = pretty v
  pretty (EConst v) = pretty v
  pretty (EBlock decls exp) = braces (prettyList decls <+> pretty exp)
