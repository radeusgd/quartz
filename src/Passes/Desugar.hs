module Passes.Desugar(
  desugarExpression,
  desugarType,
  desugarDeclaration,
                    ) where

import Prelude hiding (mod, exp)
-- import Data.String.Utils
import Data.List as List
import Quartz.Syntax.AbsQuartz as A
import AST.Desugared as D
import Constants

applyArgsTo :: D.Exp -> [D.Exp] -> D.Exp
applyArgsTo fun [] = fun
applyArgsTo fun (h:t) = applyArgsTo (D.EApplication fun h) t

desugarExpression :: A.Exp -> D.Exp
desugarExpression (A.EApp a b) =
  D.EApplication (desugarExpression a) (desugarExpression b)
desugarExpression (A.ECustomOp a (CustomOperator op) b) =
  applyArgsTo (D.EVar $ IDefault op) [desugarExpression a, desugarExpression b]
desugarExpression (A.EIfThenElse e tt ff) = -- thanks to laziness we translate if into a casual function
  applyArgsTo (D.EVar $ IQualified builtinsModuleName "if_then_else") [desugarExpression e, desugarExpression tt, desugarExpression ff]
desugarExpression (A.EAdd a b) =
  desugarExpression (A.ECustomOp a (CustomOperator "+") b)
desugarExpression (A.ESub a b) =
  desugarExpression (A.ECustomOp a (CustomOperator "-") b)
desugarExpression (A.EMul a b) =
  desugarExpression (A.ECustomOp a (CustomOperator "*") b)
desugarExpression (A.EDiv a b) =
  desugarExpression (A.ECustomOp a (CustomOperator "/") b)
desugarExpression (A.EVar i) = D.EVar $ desugarQualified i
desugarExpression (A.EStr s) = D.EConst $ LStr s where
  -- desugarStringLiteral str = replaceAll [("\\n", "\n"), ("\\\\", "\\"), ("\\\"", "\""), ("\\t", "\t")] str
  -- replaceAll reps str = foldr (uncurry replace) str reps
desugarExpression (A.EInt i) = D.EConst $ LInt i
desugarExpression (A.ENegInt i) = D.EConst $ LInt (-i)
desugarExpression (A.EDouble d) = D.EConst $ LDouble d
desugarExpression (A.ENegDouble d) = D.EConst $ LDouble (-d)
desugarExpression (A.EUndefined) = D.EConst $ LError "undefined"
desugarExpression (A.EBlock decls e) =
  D.EBlock (map desugarDeclaration decls) (desugarExpression e)
desugarExpression A.EUnit = D.EConst $ LUnit
desugarExpression (A.EMatch e cases) =
  if null cases
  then D.EConst $ LError "empty case of"
  else D.ECaseOf (desugarExpression e) (map desugarCase cases)
desugarExpression (A.ELambda (QIdent (_, v)) e) = D.ELambda v (desugarExpression e)
desugarExpression (A.EDo clauses) = desugarDoNotation clauses
desugarExpression (A.EList elems) = desugarList elems
desugarExpression (A.ETuple first rest) =
  let elems = first : rest in
    List.foldl' D.EApplication (D.EVar $ IQualified "Builtins" $ "Tuple" ++ show (length elems)) $ map desugarExpression elems

desugarQualified :: A.QualifiedIdentifier -> D.QualifiedIdent
desugarQualified (A.Qualified mod ident) = IQualified (desugarQIdent mod) (desugarQIdent ident)
desugarQualified (A.DefaultScope ident) = IDefault (desugarQIdent ident)

desugarQIdent :: QIdent -> Ident
desugarQIdent (QIdent (_, i)) = i

desugarCase :: A.Case -> D.ECase
desugarCase (A.SimpleCase name args e) = D.ECase (desugarQualified name) (map desugarQIdent args) (desugarExpression e)

desugarType :: A.Type -> D.Type
desugarType (A.Atom name) = D.Atom $ desugarQualified name
desugarType A.UnitAtom = D.Atom $ IDefault "()"
desugarType (A.Constructor name args) =
  desugarConstructor (D.Atom $ desugarQualified name) (map desugarType args)
  where
    desugarConstructor tt [] = tt
    desugarConstructor tt (h:t) = desugarConstructor (D.Construction tt h) t
desugarType (A.Abstraction ta tb) = D.Abstraction (desugarType ta) (desugarType tb)

-- returns type of a functions with given list of arguments and return value
buildApplicationType :: [D.Type] -> D.Type -> D.Type
buildApplicationType args rettype = go (reverse args) rettype where
  go [] t = t
  go (h:t) tt = go t (D.Abstraction h tt) -- we first apply the innermost abstraction, that is the last argument

desugarDeclaration :: A.Declaration -> D.Declaration
desugarDeclaration (A.Func (QIdent (_, name)) qualifiers args rettype exp) =
  desugarFunction name qualifiers args (Just rettype) exp
desugarDeclaration (A.ParameterlessFunc (QIdent (_, name)) qualifiers rettype exp) =
  desugarFunction name qualifiers [] (Just rettype) exp
desugarDeclaration (A.Operator (CustomOperator op) qualifiers a1 a2 rettype exp) =
  desugarFunction (desugarOpName op) qualifiers [a1, a2] (Just rettype) exp
  where
    desugarOpName name =
      if head name == '(' && last name == ')' then removeParens name
      else name
      where
        removeParens str = tail $ take (length str - 1) str
desugarDeclaration (A.Value (QIdent (_, name)) rettype exp) = -- treat values as 0-arg functions
  desugarFunction name [] [] (Just rettype) exp
desugarDeclaration (A.ValueInferred (QIdent (_, name)) exp) = -- treat values as 0-arg functions
  desugarFunction name [] [] Nothing exp
desugarDeclaration (A.Data (QIdent (_, typename)) typeargs cases) = D.DataType typename (map desugarQIdent typeargs) (map desugarDataCase cases)
  where
    desugarDataCase (A.DataConstructor (QIdent (_, name)) types) = D.DataTypeCase name (map desugarType types)

desugarFunction :: String -> [TypeQualifier] -> [A.Arg] -> Maybe A.Type -> A.Exp -> D.Declaration
desugarFunction f qualifiedTypes args rettype exp = -- TODO polymorphism
  D.Function f args' type' exp' where
  args' = map desugarArgs args
  rettype' = desugarType <$> rettype
  type' = bindQualified qualifiedTypes . buildApplicationType (map argType args) <$> rettype'
  exp' = desugarExpression exp
  desugarArgs :: A.Arg -> Ident
  desugarArgs (A.Argument (QIdent (_, a)) _) = a
  desugarArgs (A.ArgumentWithDefault (QIdent (_, a)) _ _) = a -- TODO default values
  argType :: A.Arg -> D.Type
  argType (A.Argument _ t) = desugarType t
  argType (A.ArgumentWithDefault _ t _) = desugarType t
  bindQualified :: [TypeQualifier] -> D.Type -> D.QualifiedType
  bindQualified quals tt = ForAll (map (\(FreeTypeVariable (QIdent (_, v))) -> v) quals) tt

desugarDoNotation :: [A.DoClause] -> D.Exp
desugarDoNotation [] = error "Empty do"
desugarDoNotation [DoExp e] = desugarExpression e
desugarDoNotation [DoLet _ e] = desugarExpression e
desugarDoNotation (DoExp e :t) =
  (D.EApplication
   (D.EApplication
    (D.EVar $ IQualified builtinsModuleName ">>") (desugarExpression e))
   (desugarDoNotation t)
  )
desugarDoNotation (DoLet (QIdent (_, v)) e :t) =
  (D.EApplication
   (D.EApplication
    (D.EVar $ IQualified builtinsModuleName ">>=") (desugarExpression e))
   (D.ELambda v (desugarDoNotation t))
  )

desugarList :: [A.Exp] -> D.Exp
desugarList [] = D.EVar $ IQualified builtinsModuleName "Nil"
desugarList (h:t) =
  (D.EApplication (D.EApplication (D.EVar $ IQualified builtinsModuleName "Cons") (desugarExpression h)) (desugarList t))
