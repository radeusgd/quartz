module Builtins where --(builtins, Builtin, makeTypeEnv, runWithBuiltins, register) where

import qualified Quartz.Syntax.AbsQuartz as Abs
import qualified Quartz.Syntax.ParQuartz as Par
import qualified Quartz.Syntax.ErrM as Err
import AST.Desugared
import Passes.Desugar
import System.IO
import Linker

loadBuiltinDecls :: IO [Declaration]
loadBuiltinDecls = do
  stdlib <- findStdLib
  fd <- openFile (stdlib ++ "/Builtins.quartz") ReadMode
  contents <- hGetContents fd
  let toks = Par.myLexer contents
  let ed = (\(Abs.Prog decls) -> decls) <$> Par.pProgram toks
  case ed of
    Err.Bad s -> error $ "Fatal error: syntax error in Builtins.quartz: " ++ s
    Err.Ok decls -> return $ map desugarDeclaration decls

-- import qualified Data.Map as M
-- import AST.Typed
-- import Passes.TypeCheck(Env)
-- import Interpreter(Value(..), emptyEnv, readVar)
-- import qualified Interpreter as I
-- import Control.Monad.Reader


-- data TypeHelper = Abs TypeHelper TypeHelper | Atm String
-- makeType :: TypeHelper -> Type
-- makeType (Atm s) = Atom s
-- makeType (Abs a b) = Abstraction (makeType a) (makeType b)

-- data Builtin = Builtin Type Value

-- int = Atm "Int"
-- str = Atm "String"
-- bool = Atm "Bool"

-- makeSimpleType :: [TypeHelper] -> Type
-- makeSimpleType [t] = makeType t
-- makeSimpleType (h:t) = Abstraction (makeType h) (makeSimpleType t)

-- makeTwoArgFun :: (Value -> Value -> Value) -> Value
-- makeTwoArgFun f = VFunction "x" emptyEnv $ do
--   x <- readVar "x"
--   return $ VFunction "y" emptyEnv $ do
--     y <- readVar "y"
--     return $ f x y

-- make3ArgFun :: (Value -> Value -> Value -> Value) -> Value
-- make3ArgFun f = VFunction "z" emptyEnv $ do
--   z <- readVar "z"
--   return $ makeTwoArgFun (f z)

-- builtins :: [(String, Builtin)]
-- builtins = [
--   -- TODO actually plus should be polymorphic... but for now let's skip this
--   ("+", Builtin (makeSimpleType [int, int, int]) (makeTwoArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x + y)),
--   ("*", Builtin (makeSimpleType [int, int, int]) (makeTwoArgFun $ \(VInt x) -> \(VInt y) -> VInt $ x * y)),
--   ("if_then_else", Builtin (makeType (Abs bool (Abs bool (Abs bool bool)))) (make3ArgFun $ \(VBool x) -> \tt -> \ff -> if x then tt else ff)),
--   ("<+>", Builtin (makeType (Abs str (Abs str str))) (makeTwoArgFun $ \(VStr a) -> \(VStr b) -> VStr $ a ++ b))
--            ]


-- makeTypeEnv :: [(String, Builtin)] -> Env
-- makeTypeEnv = foldl register M.empty where
--   register :: Env -> (String, Builtin) -> Env
--   register env (name, Builtin tt _) =
--     M.insert name tt env

-- runWithBuiltins :: [(String, Builtin)] -> I.Interpreter a -> Either I.ErrorType a
-- runWithBuiltins builtins i = I.runInterpreter $ do
--   env <- foldM register I.emptyEnv builtins
--   local (\_ -> env) i

-- register :: I.Env -> (String, Builtin) -> I.Interpreter I.Env
-- register env (name, Builtin _ v) = do
--   loc <- I.alloc
--   I.setValue loc v
--   return $ I.bind name loc env
