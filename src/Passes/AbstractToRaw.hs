module Passes.AbstractToRaw(
  convertExpression,
  convertType,
  convertDeclaration,
                    ) where

import Quartz.Syntax.AbsQuartz as A
import AST.RawAST as R


applyArgsTo :: R.Exp -> [R.Exp] -> R.Exp
applyArgsTo fun [] = fun
applyArgsTo fun (h:t) = applyArgsTo (R.EApplication fun h) t

convertExpression :: A.Exp -> R.Exp
convertExpression (A.EApp a b) =
  R.EApplication (convertExpression a) (convertExpression b)
convertExpression (A.ECustomOp a (CustomOperator op) b) =
  applyArgsTo (R.EVar op) [convertExpression a, convertExpression b]
convertExpression (A.EIfThenElse e tt ff) = -- thanks to laziness we translate if into a casual function
  applyArgsTo (R.EVar "if_then_else") [convertExpression e, convertExpression tt, convertExpression ff]
convertExpression (A.EAdd a b) =
  convertExpression (A.ECustomOp a (CustomOperator "+") b)
convertExpression (A.ESub a b) =
  convertExpression (A.ECustomOp a (CustomOperator "-") b)
convertExpression (A.EMul a b) =
  convertExpression (A.ECustomOp a (CustomOperator "*") b)
convertExpression (A.EDiv a b) =
  convertExpression (A.ECustomOp a (CustomOperator "/") b)
convertExpression (A.EVar (QIdent (_, v))) = R.EVar v
convertExpression (A.EStr s) = R.EConst $ VStr s
convertExpression (A.EInt i) = R.EConst $ VInt i
convertExpression (A.EDouble d) = R.EConst $ VDouble d
convertExpression (A.EUndefined) = R.EVar "undefined" -- TODO not sure if that's how I want this?
convertExpression (A.EBlock decls e) =
  R.EBlock (map convertDeclaration decls) (convertExpression e)

-- TODO the typing pass may need to use some Reader or State to keep track of fresh variables once we add better inference
convertType :: A.Type -> R.Type
convertType (A.Atom (QIdent (_, name))) = R.Atom name
convertType (A.Abstraction ta tb) = R.Abstraction (convertType ta) (convertType tb)

-- returns type of a functions with given list of arguments and return value
buildApplicationType :: [R.Type] -> R.Type -> R.Type
buildApplicationType args rettype = go (reverse args) rettype where
  go [] t = t
  go (h:t) rettype = go t (R.Abstraction h rettype) -- we first apply the innermost abstraction, that is the last argument

convertDeclaration :: A.Declaration -> R.Declaration
convertDeclaration (A.Func (QIdent (_, f)) args rettype exp) =
  R.Function f args' type' exp' where
  args' = map convertArgs args
  type' = buildApplicationType (map argType args) $ convertType rettype
  exp' = convertExpression exp
  convertArgs :: A.Arg -> R.Arg
  convertArgs (A.Argument (QIdent (_, a)) _) = R.Argument a
  convertArgs (A.ArgumentWithDefault (QIdent (_, a)) _ _) = R.Argument a -- TODO default values
  argType :: A.Arg -> R.Type
  argType (A.Argument _ t) = convertType t
  argType (A.ArgumentWithDefault _ t _) = convertType t
convertDeclaration (A.Operator (CustomOperator op) a1 a2 rettype exp) = -- TODO add Position to CustomOperator to get rid of this undefined
  convertDeclaration (A.Func (QIdent (undefined, op)) [a1, a2] rettype exp)
convertDeclaration (A.Value name rettype exp) = -- treat values as 0-arg functions
  convertDeclaration (A.Func name [] rettype exp)
