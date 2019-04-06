module Passes.Desugar(
  desugarExpression,
  desugarType,
  desugarDeclaration,
                    ) where

import Data.Maybe
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
desugarExpression A.EUnit = D.EVar "TODO"
desugarExpression (A.EMatch e cases) = D.EVar "TODO"
desugarExpression (A.ELambda (QIdent (_, v)) e) = D.ELambda v (desugarExpression e)
desugarExpression (A.EDo clauses) = desugarDoNotation clauses
desugarExpression (A.EList elems) = desugarList elems

-- TODO the typing pass may need to use some Reader or State to keep track of fresh variables once we add better inference
desugarType :: A.Type -> D.Type
desugarType (A.Atom (QIdent (_, name))) = D.Atom name
desugarType A.UnitAtom = D.Atom "()"
desugarType (A.Constructor (QIdent (_, name)) args) = D.Atom name -- TODO FIXME constructor args support!!!
desugarType (A.Abstraction ta tb) = D.Abstraction (desugarType ta) (desugarType tb)

-- returns type of a functions with given list of arguments and return value
buildApplicationType :: [D.Type] -> D.Type -> D.Type
buildApplicationType args rettype = go (reverse args) rettype where
  go [] t = t
  go (h:t) rettype = go t (D.Abstraction h rettype) -- we first apply the innermost abstraction, that is the last argument

desugarDeclaration :: A.Declaration -> D.Declaration
desugarDeclaration (A.Func (QIdent (_, name)) qualifiers args rettype exp) =
  desugarFunction name qualifiers args (Just rettype) exp
desugarDeclaration (A.ParameterlessFunc (QIdent (_, name)) qualifiers rettype exp) =
  desugarFunction name qualifiers [] (Just rettype) exp
desugarDeclaration (A.Operator (CustomOperator op) qualifiers a1 a2 rettype exp) = -- TODO add Position to CustomOperator to get rid of this undefined
  desugarFunction op qualifiers [a1, a2] (Just rettype) exp
desugarDeclaration (A.Value (QIdent (_, name)) rettype exp) = -- treat values as 0-arg functions
  desugarFunction name [] [] (Just rettype) exp
desugarDeclaration (A.ValueInferred (QIdent (_, name)) exp) = -- treat values as 0-arg functions
  desugarFunction name [] [] Nothing exp
desugarDeclaration (A.Import (Ident name)) = D.Import name
desugarDeclaration (A.Data (QIdent (_, typename)) args cases) = D.DataType typename (map desugarDataCase cases) -- TODO
  where
    desugarDataCase (A.DataConstructor (QIdent (_, name)) types) = D.DataTypeCase name (map desugarType types)

desugarFunction :: String -> [TypeQualifier] -> [A.Arg] -> Maybe A.Type -> A.Exp -> D.Declaration
desugarFunction f qualifiedTypes args rettype exp = -- TODO polymorphism
  D.Function f args' type' exp' where
  args' = map desugarArgs args
  rettype' = fromMaybe D.Unbound (desugarType <$> rettype)
  type' = bindQualified qualifiedTypes $ buildApplicationType (map argType args) $ rettype'
  exp' = desugarExpression exp
  desugarArgs :: A.Arg -> D.Arg
  desugarArgs (A.Argument (QIdent (_, a)) _) = D.Argument a
  desugarArgs (A.ArgumentWithDefault (QIdent (_, a)) _ _) = D.Argument a -- TODO default values
  argType :: A.Arg -> D.Type
  argType (A.Argument _ t) = desugarType t
  argType (A.ArgumentWithDefault _ t _) = desugarType t
  bindQualified :: [TypeQualifier] -> D.Type -> D.Type
  bindQualified [] t = t
  bindQualified (FreeTypeVariable (QIdent (_, v)) : tail) t = ForAll v (bindQualified tail t)

desugarDoNotation :: [A.DoClause] -> D.Exp
desugarDoNotation [] = error "Empty do"
desugarDoNotation [DoExp e] = desugarExpression e
desugarDoNotation [DoLet _ e] = desugarExpression e
desugarDoNotation (DoExp e :t) =
  (D.EApplication
   (D.EApplication
    (D.EVar ">>") (desugarExpression e))
   (desugarDoNotation t)
  )
desugarDoNotation (DoLet (QIdent (_, v)) e :t) =
  (D.EApplication
   (D.EApplication
    (D.EVar ">>=") (desugarExpression e))
   (D.ELambda v (desugarDoNotation t))
  )

desugarList :: [A.Exp] -> D.Exp
desugarList [] = D.EVar "Nil"
desugarList (h:t) =
  (D.EApplication (D.EApplication (D.EVar "Cons") (desugarExpression h)) (desugarList t))
