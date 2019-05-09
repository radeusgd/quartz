module Runtime where

import Passes.TypeCheck
import AST.Desugared
import Builtins
import Interpreter

import System.Console.Repline
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List
import qualified ModuleMap as MM
import qualified Data.Map as Map

data RState = RState { rsTypeEnv :: TypeEnv, rsVarEnv :: Env, rsMem :: Memory }

makeInitialState :: IO RState
makeInitialState = do
  builtins <- loadBuiltinDecls
  let tenv = case evalInfer $ extendEnvironment emptyTypeEnv builtins of
               Right env -> env
               Left e -> error $ "Fatal error, cannot typecheck builtins: " ++ show e
  res <- execInterpreter emptyEnv emptyMemory builtinsEnv
  case res of
    Right (env, mem) ->
      return $ RState tenv env mem
    Left e -> error $ "Fatal error, cannot initialie builtins: " ++ e

