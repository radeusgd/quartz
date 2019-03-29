module Passes.Desugar(
  desugarExpression,
  desugarType,
  desugarDeclaration,
                    ) where

import Quartz.Syntax.AbsQuartz as A
import AST.Desugared as D


applyArgsTo :: D.Exp -> [D.Exp] -> D.Exp
applyArgsTo fun [] = fun
applyArgsTo fun (h:t) = applyArgsTo (D.EApplication fun h) t

desugarExpression :: A.Exp -> D.Exp
desugarExpression (A.EApp a b) =
  D.EApplication (desugarExpression a) (desugarExpression b)
desugarExpression (A.ECustomOp a (CustomOperator op) b) =
  applyArgsTo (D.EVar op) [desugarExpression a, desugarExpression b]
desugarExpression (A.EIfThenElse e tt ff) = -- thanks to laziness we translate if into a casual function
  applyArgsTo (D.EVar "if_then_else") [desugarExpression e, desugarExpression tt, desugarExpression ff]
desugarExpression (A.EAdd a b) =
  desugarExpression (A.ECustomOp a (CustomOperator "+") b)
desugarExpression (A.ESub a b) =
  desugarExpression (A.ECustomOp a (CustomOperator "-") b)
desugarExpression (A.EMul a b) =
  desugarExpression (A.ECustomOp a (CustomOperator "*") b)
desugarExpression (A.EDiv a b) =
  desugarExpression (A.ECustomOp a (CustomOperator "/") b)
desugarExpression (A.EVar (QIdent (_, v))) = D.EVar v
desugarExpression (A.EStr s) = D.EConst $ VStr s
desugarExpression (A.EInt i) = D.EConst $ VInt i
desugarExpression (A.EDouble d) = D.EConst $ VDouble d
desugarExpression A.ETrue = D.EConst $ VBool True
desugarExpression A.EFalse = D.EConst $ VBool False
desugarExpression (A.EUndefined) = D.EVar "undefined" -- TODO not sure if that's how I want this?
desugarExpression (A.EBlock decls e) =
  D.EBlock (map desugarDeclaration decls) (desugarExpression e)

-- TODO the typing pass may need to use some Reader or State to keep track of fresh variables once we add better inference
desugarType :: A.Type -> D.Type
desugarType (A.Atom (QIdent (_, name))) = D.Atom name
desugarType (A.Abstraction ta tb) = D.Abstraction (desugarType ta) (desugarType tb)

-- returns type of a functions with given list of arguments and return value
buildApplicationType :: [D.Type] -> D.Type -> D.Type
buildApplicationType args rettype = go (reverse args) rettype where
  go [] t = t
  go (h:t) rettype = go t (D.Abstraction h rettype) -- we first apply the innermost abstraction, that is the last argument

desugarDeclaration :: A.Declaration -> D.Declaration
desugarDeclaration (A.Func (QIdent (_, f)) args rettype exp) =
  D.Function f args' type' exp' where
  args' = map desugarArgs args
  type' = buildApplicationType (map argType args) $ desugarType rettype
  exp' = desugarExpression exp
  desugarArgs :: A.Arg -> D.Arg
  desugarArgs (A.Argument (QIdent (_, a)) _) = D.Argument a
  desugarArgs (A.ArgumentWithDefault (QIdent (_, a)) _ _) = D.Argument a -- TODO default values
  argType :: A.Arg -> D.Type
  argType (A.Argument _ t) = desugarType t
  argType (A.ArgumentWithDefault _ t _) = desugarType t
desugarDeclaration (A.Operator (CustomOperator op) a1 a2 rettype exp) = -- TODO add Position to CustomOperator to get rid of this undefined
  desugarDeclaration (A.Func (QIdent (undefined, op)) [a1, a2] rettype exp)
desugarDeclaration (A.Value name rettype exp) = -- treat values as 0-arg functions
  desugarDeclaration (A.Func name [] rettype exp)
