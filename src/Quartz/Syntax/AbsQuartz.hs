

module Quartz.Syntax.AbsQuartz where

-- Haskell module generated by the BNF converter




newtype QIdent = QIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype CustomOperator = CustomOperator String
  deriving (Eq, Ord, Show, Read)
data QualifiedIdentifier
    = Qualified QIdent QIdent | DefaultScope QIdent
  deriving (Eq, Ord, Show, Read)

data Program = Prog [Import] [Declaration]
  deriving (Eq, Ord, Show, Read)

data Import = NormalImport QIdent
  deriving (Eq, Ord, Show, Read)

data Declaration
    = Func QIdent [TypeQualifier] [Arg] Type Exp
    | ParameterlessFunc QIdent [TypeQualifier] Type Exp
    | Operator CustomOperator [TypeQualifier] Arg Arg Type Exp
    | Value QIdent Type Exp
    | ValueInferred QIdent Exp
    | Data QIdent [QIdent] [DataCase]
  deriving (Eq, Ord, Show, Read)

data DataCase = DataConstructor QIdent [Type]
  deriving (Eq, Ord, Show, Read)

data Arg
    = Argument QIdent Type | ArgumentWithDefault QIdent Type Exp
  deriving (Eq, Ord, Show, Read)

data TypeQualifier = FreeTypeVariable QIdent
  deriving (Eq, Ord, Show, Read)

data Type
    = Atom QualifiedIdentifier
    | UnitAtom
    | Abstraction Type Type
    | Constructor QualifiedIdentifier [Type]
  deriving (Eq, Ord, Show, Read)

data Exp
    = ECustomOp Exp CustomOperator Exp
    | EApp Exp Exp
    | EIfThenElse Exp Exp Exp
    | ELambda QIdent Exp
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | EVar QualifiedIdentifier
    | EStr String
    | EInt Integer
    | ENegInt Integer
    | EDouble Double
    | ENegDouble Double
    | EUndefined
    | EUnit
    | EBlock [Declaration] Exp
    | EList [Exp]
    | ETuple Exp [Exp]
    | EMatch Exp [Case]
    | EDo [DoClause]
  deriving (Eq, Ord, Show, Read)

data Case = SimpleCase QualifiedIdentifier [QIdent] Exp
  deriving (Eq, Ord, Show, Read)

data DoClause = DoExp Exp | DoLet QIdent Exp
  deriving (Eq, Ord, Show, Read)

