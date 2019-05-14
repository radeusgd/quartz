module ModuleMap where
import Prelude hiding (lookup, mod)
import qualified Data.Map.Strict as M
import AST.Desugared

data ModuleMap a = ModuleMap { modules :: M.Map Ident (M.Map Ident a), local :: M.Map Ident a } deriving (Eq, Show)

empty :: ModuleMap a
empty = ModuleMap M.empty M.empty

lookup :: Show a => QualifiedIdent -> ModuleMap a -> Either String a -- FIXME remove show
lookup (IQualified mod ident) m = case M.lookup mod (modules m) of
  Nothing -> Left $ "Module " ++ mod ++ " not found in current scope"
  Just module' -> case M.lookup ident module' of
    Nothing -> Left $ "Identifier " ++ ident ++ " not found in module " ++ mod
    Just e -> Right e
lookup (IDefault ident) m = case M.lookup ident (local m) of
  Just e -> Right e
  Nothing -> case findCandidateModules of
    [] -> Left $ "Identifier " ++ ident ++ " not found, " ++ show m
    [e] -> Right $ snd e
    more -> Left $ "Ambigouous reference, identifier " ++ ident ++ " may refer to one in the following modules: " ++ show (map fst more)
    where
      findCandidateModules = M.toList (modules m) >>= tryGettingIdent
      tryGettingIdent (modName, mapping) = case M.lookup ident mapping of
        Just e -> [(modName, e)]
        Nothing -> []

insertLocal :: Ident -> a -> ModuleMap a -> ModuleMap a
insertLocal ident e m = m { local = M.insert ident e (local m) }

insertQualified :: Ident -> Ident -> a -> ModuleMap a -> ModuleMap a
insertQualified mod ident e m = m { modules = modules' } where
  modules' = M.insertWith M.union mod (M.singleton ident e) (modules m)

insert :: QualifiedIdent -> a -> ModuleMap a -> ModuleMap a
insert (IDefault i) e m = insertLocal i e m
insert (IQualified mod i) e m = insertQualified mod i e m
