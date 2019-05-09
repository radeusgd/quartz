module AppCommon where

import System.IO
import Quartz.Syntax.ErrM
import qualified Quartz.Syntax.AbsQuartz as Abs
import Quartz.Syntax.ParQuartz
import Passes.Desugar
import AST.Desugared

parseFile' :: String -> IO (Err ([Abs.Import], [Abs.Declaration]))
parseFile' fname = do
  fd <- openFile fname ReadMode
  contents <- hGetContents fd
  let toks = myLexer contents
  return $ (\(Abs.Prog imports decls) -> (imports, decls)) <$> pProgram toks

parseFile :: String -> IO (Err ([String], [Declaration]))
parseFile fname = do
  parsed <- parseFile' fname
  return $ (\(imports, raw) -> (map desugarImport imports, map desugarDeclaration raw)) <$> parsed
  where
    desugarImport (Abs.NormalImport (Abs.QIdent (_, v))) = v
