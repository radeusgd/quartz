module Linker where

import Data.Map as M
import Control.Monad.Except
import AST.Typed
import Passes.TypeCheck

ensureTypeSpecified :: Type -> Bool
ensureTypeSpecified (Atom _) = True
ensureTypeSpecified (Abstraction a b) = ensureTypeSpecified a && ensureTypeSpecified b
ensureTypeSpecified (FreeParameter _) = False

ensureTyped :: Declaration -> Bool
ensureTyped (Function tt _ _ _) = ensureTypeSpecified tt

introduceTopLevelTypes :: [Declaration] -> Env -> Either TypeError Env
introduceTopLevelTypes [] e = return e
introduceTopLevelTypes (d@(Function tt name _ _) : t) e =
  if not $ ensureTyped d then throwError (TopLevelTypeNotSpecified name)
  else introduceTopLevelTypes t (M.insert name tt e)
