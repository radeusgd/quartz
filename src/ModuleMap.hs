module ModuleMap where

import Data.Map.Strict as M
import AST.Desugared

data ModuleMap a = ModuleMap { modules :: Map Ident (Map Ident a), local :: Map Ident a }

empty :: ModuleMap a
empty = ModuleMap M.empty M.empty

lookup :: QualifiedIdent -> ModuleMap a -> Either String a
lookup (IQualified mod ident) m = case M.lookup mod (modules m) of
  Nothing -> Left $ "Module " ++ mod ++ " not found in current scope"
  Just module' -> case M.lookup ident module' of
    Nothing -> Left $ "Identifier " ++ ident ++ " not found in module " ++ mod
    Just e -> Right e
lookup (IDefault ident) m = case M.lookup ident (local m) of
  Just e -> Right e
  Nothing -> Left "TODO: default module names not yet supported" -- TODO

insertLocal :: Ident -> a -> ModuleMap a -> ModuleMap a
insertLocal ident e m = m { local = M.insert ident e (local m) }

insertQualified :: Ident -> Ident -> a -> ModuleMap a -> ModuleMap a
insertQualified mod ident e m = m { modules = modules' } where
  modules' = M.insertWith M.union mod (M.singleton ident e) (modules m)
