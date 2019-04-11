module AppCommon where

import System.IO
import Quartz.Syntax.ErrM
import qualified Quartz.Syntax.AbsQuartz as Abs
import Quartz.Syntax.ParQuartz
import Passes.Desugar
import AST.Desugared

parseFile' :: String -> IO (Err [Abs.Declaration])
parseFile' fname = do
  fd <- openFile fname ReadMode
  contents <- hGetContents fd
  let toks = myLexer contents
  return $ (\(Abs.Prog decls) -> decls) <$> pProgram toks

parseFile :: String -> IO (Err [Declaration])
parseFile fname = do
  raw <- parseFile' fname
  return $ (map desugarDeclaration) <$> raw
