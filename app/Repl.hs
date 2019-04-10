module Repl where

import Passes.Desugar
import Passes.TypeCheck
import AST.Desugared
import Builtins

data RState = RState { rsTypeEnv :: [Declaration] }

makeInitialState :: IO RState
makeInitialState = do
  builtins <- loadBuiltinDecls
  return $ RState builtins

runRepl :: IO ()
runRepl = undefined
