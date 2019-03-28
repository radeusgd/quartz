module AST.RawAST where

type Ident = String

data Value = VStr String | VInt Integer | VDouble Double | VUndefined

data Type = Arom Ident | Abstraction Type Type | Unbound Integer

data Declaration = Function Ident [Arg] Type Exp
-- TODO values may be 0-arg functions, but evaluation should be lazy then

data Arg = Argument Ident Type -- TODO default values are discarded for now

data Exp
  = EApplication Exp Exp
  | EVar Ident
  | EConst Value
  | EBlock [Declaration] Exp
