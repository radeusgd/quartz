{-# LANGUAGE OverloadedStrings #-}
module AST.Desugared where

import Data.Text.Prettyprint.Doc

type Ident = String

data Value = VStr String | VInt Integer | VDouble Double | VBool Bool | VUndefined deriving Show

data Type
  = Atom Ident
  | Abstraction Type Type
  | Unbound
  | ForAll Ident Type
  deriving Show

data Declaration
  = Function Ident [Arg] Type Exp
  | Import String
  | DataType Ident [DataTypeCase]
  deriving Show
-- type of functions is the type of the whole function in contrast to just return type that was the case in Abstract Syntax
-- TODO values may be 0-arg functions, but evaluation should be lazy then

data DataTypeCase = DataTypeCase Ident [Type] deriving Show

newtype Arg = Argument Ident -- TODO default values are discarded for now
  deriving Show

data Exp
  = EApplication Exp Exp
  | EVar Ident
  | EConst Value
  | ELambda Ident Exp
  | EBlock [Declaration] Exp
  deriving Show

instance Pretty Value where
  pretty (VStr s) = "\"" <> pretty s <> "\""
  pretty (VInt i) = pretty i
  pretty (VDouble d) = pretty d
  pretty (VBool True) = "true"
  pretty (VBool False) = "false"
  pretty VUndefined = "???"

instance Pretty Type where
  pretty (Atom ident) = pretty ident
  pretty (Abstraction a b) = parens $ (pretty a) <+> "->" <+> (pretty b)
  pretty Unbound = "?t?"
  pretty (ForAll ident tt) = "∀" <> pretty ident <> "." <> pretty tt

instance Pretty Arg where
  pretty (Argument ident) = pretty ident

instance Pretty Declaration where
  pretty (Function name args ttype exp) = "def" <+> pretty name <+> parens (pretty args) <+> ":" <+> pretty ttype <+> "=" <+> pretty exp
  pretty (Import name) = "import" <+> pretty name
  pretty (DataType name cases) = "data" <+> pretty name <+> (braces $ nest 4 (line <> vsep (map pretty cases)))

instance Pretty DataTypeCase where
  pretty (DataTypeCase name fields) = pretty name <+> hsep (map pretty fields)

instance Pretty Exp where
  pretty (EApplication a b) = parens (pretty a <+> pretty b)
  pretty (EVar v) = pretty v
  pretty (EConst v) = pretty v
  pretty (ELambda v e) = "λ" <> pretty v <> "." <+> pretty e
  pretty (EBlock decls exp) =
    braces $ nest 4 (line <> vsep (map pretty decls) <> line <> pretty exp <> line)
