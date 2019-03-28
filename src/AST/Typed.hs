{-# LANGUAGE OverloadedStrings #-}
module AST.Typed where

import Data.Text.Prettyprint.Doc
import AST.Desugared(Ident, Value)

data Type = Atom Ident | Abstraction Type Type | FreeParameter Integer

data Exp
  = EApplication Type Exp Exp
  | EVar Type Ident
  | EConst Type Value
  | EBlock Type [Declaration] Exp

data Declaration =
  Function Type Ident [Ident] Exp

instance Pretty Type where
  pretty (Atom ident) = pretty ident
  pretty (Abstraction a b) = (pretty a) <+> "->" <+> (pretty b)
  pretty (FreeParameter i) = pretty ("'" ++ show i)

instance Pretty Declaration where
  pretty (Function t name args exp) = "def" <+> pretty name <+> parens (pretty args) <+> ":" <+> pretty t <+> "=" <+> pretty exp

instance Pretty Exp where
  pretty (EApplication t a b) = parens (pretty a <+> pretty b) <+> ":" <+> pretty t
  pretty (EVar t v) = pretty v <+> ":" <+> pretty t
  pretty (EConst t v) = pretty v <+> ":" <+> pretty t
  pretty (EBlock _ decls exp) =
    braces $ nest 4 (line <> vsep (map pretty decls) <> line <> pretty exp <> line) -- <+> ":" <+> pretty t -- type is exactly same as last expression, no sense to write it
