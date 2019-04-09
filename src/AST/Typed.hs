{-# LANGUAGE OverloadedStrings #-}
module AST.Typed(module AST.Typed) where

-- import Data.Text.Prettyprint.Doc
-- import Data.Text.Prettyprint.Doc.Render.String
-- import AST.Desugared(Ident, Value)


-- data Exp
--   = EApplication Type Exp Exp
--   | EVar Type Ident
--   | EConst Type Value
--   | EBlock [Declaration] Exp

-- typeOfE :: Exp -> Type
-- typeOfE (EApplication t _ _) = t
-- typeOfE (EVar t _) = t
-- typeOfE (EConst t _) = t
-- typeOfE (EBlock _ e) = typeOfE e

-- data Declaration =
--   Function Type Ident [Ident] Exp

-- declarationName :: Declaration -> String
-- declarationName (Function _ name _ _) = name

-- instance Pretty Type where
--   pretty (Atom ident) = pretty ident
--   pretty (Abstraction a b) = parens $ (pretty a) <+> "->" <+> (pretty b)
--   pretty (FreeParameter i) = pretty ("'" ++ show i)

-- instance Show Type where
--   show = renderString . layoutSmart defaultLayoutOptions . pretty

-- instance Pretty Declaration where
--   pretty (Function t name args exp) = "def" <+> pretty name <+> parens (pretty args) <+> ":" <+> pretty t <+> "=" <+> pretty exp

-- instance Pretty Exp where
--   pretty (EApplication t a b) = parens (pretty a <+> pretty b) <+> ":" <+> pretty t
--   pretty (EVar t v) = pretty v <+> ":" <+> pretty t
--   pretty (EConst t v) = pretty v <+> ":" <+> pretty t
--   pretty (EBlock decls exp) =
--     braces $ nest 4 (line <> vsep (map pretty decls) <> line <> pretty exp <> line) -- <+> ":" <+> pretty t -- type is exactly same as last expression, no sense to write it

-- instance Show Exp where
--   show = renderString . layoutSmart defaultLayoutOptions . pretty
