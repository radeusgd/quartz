module Builtins(initialEnv) where

import Data.Map
import AST.Typed
import Passes.TypeCheck(Env)


data TypeHelper = Abs TypeHelper TypeHelper | Atm String
makeType :: TypeHelper -> Type
makeType (Atm s) = Atom s
makeType (Abs a b) = Abstraction (makeType a) (makeType b)

int = Atm "Int"
str = Atm "String"

-- TODO this will be more complex, but a stub to test typechecking
initialEnv :: Env
initialEnv = fromList [
  -- TODO actually plus should be polymorphic... but for now let's skip this
  ("+", makeType (Abs int (Abs int int))),
  ("<>", makeType (Abs str (Abs str str))),
  ("*", makeType (Abs int (Abs int int)))
                      ]
