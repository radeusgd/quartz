{-# LANGUAGE OverloadedStrings #-}
module AST.Desugared where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

type Ident = String

data Literal
  = LStr String
  | LInt Integer
  | LDouble Double
  | LUnit
  | LError String

data Type
  = Atom Ident
  | Abstraction Type Type
  | Construction Type Type
  | FreeVariable Integer
  deriving Eq

data QualifiedType = ForAll [Ident] Type

data Declaration
  = Function Ident [Ident] (Maybe QualifiedType) Exp
--  | Import String
  | DataType Ident [Ident] [DataTypeCase]
  deriving Show
-- type of functions is the type of the whole function in contrast to just return type that was the case in Abstract Syntax
-- TODO values may be 0-arg functions, but evaluation should be lazy then

data DataTypeCase = DataTypeCase Ident [Type] deriving Show

declarationName :: Declaration -> String
declarationName (Function name _ _ _) = name
declarationName (DataType name _ _) = name

-- newtype Arg = Argument Ident -- TODO default values are discarded for now
--   deriving Show

data Exp
  = EApplication Exp Exp
  | EVar Ident
  | EConst Literal
  | ELambda Ident Exp
  | ECaseOf Exp [ECase]
  | EBlock [Declaration] Exp

data ECase = ECase Ident [Ident] Exp

instance Pretty Literal where
  pretty (LStr s) = dquotes $ pretty s
  pretty (LInt i) = pretty i
  pretty (LDouble d) = pretty d
  pretty LUnit = "()"
  pretty (LError s) = "error" <+> (dquotes $ pretty s)

instance Pretty Type where
  pretty (Atom ident) = pretty ident
  pretty (Abstraction a b) = parens $ (pretty a) <+> "->" <+> (pretty b)
  pretty (FreeVariable i) = "'" <> pretty i
  pretty (Construction a b) = parens $ pretty a <+> pretty b

instance Show Type where
  show = renderString . layoutSmart defaultLayoutOptions . pretty

instance Show QualifiedType where
  show = renderString . layoutSmart defaultLayoutOptions . pretty

instance Pretty QualifiedType where
  pretty (ForAll idents tt) = qualifs <> pretty tt where
    qualifs = case idents of
      [] -> ""
      _ -> "∀" <> pretty idents <> "."

-- instance Pretty Arg where
--   pretty (Argument ident) = pretty ident

instance Pretty Declaration where
  pretty (Function name args ttype exp) = "def" <+> pretty name <+> parens (pretty args) <+> ":" <+> pretty ttype <+> "=" <+> pretty exp
  -- pretty (Import name) = "import" <+> pretty name
  pretty (DataType name typeargs cases) = "data" <+> pretty name <+> (hsep $ map pretty typeargs) <+> (braces $ nest 4 (line <> vsep (map pretty cases) <> line))

instance Pretty DataTypeCase where
  pretty (DataTypeCase name fields) = "|" <+> pretty name <+> hsep (map pretty fields)

instance Pretty Exp where
  pretty (EApplication a b) = parens (pretty a <+> pretty b)
  pretty (EVar v) = pretty v
  pretty (EConst v) = pretty v
  pretty (ELambda v e) = "λ" <> pretty v <> "." <+> pretty e
  pretty (ECaseOf e cases) = "case" <+> pretty e <+> "of" <+> (braces $ nest 4 (line <> vsep (map pretty cases) <> line))
  pretty (EBlock decls exp) =
    braces $ nest 4 (line <> vsep (map pretty decls) <> line <> pretty exp <> line)

instance Pretty ECase where
  pretty (ECase name args e) = "|" <+> pretty name <+> hsep (map pretty args) <+> "->" <+> pretty e

instance Show Literal where
  show = renderString . layoutSmart defaultLayoutOptions . pretty

instance Show Exp where
  show = renderString . layoutSmart defaultLayoutOptions . pretty
