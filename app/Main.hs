module Main where

import System.IO
import System.Environment
import System.Exit ( exitFailure, exitSuccess )

import Quartz.Syntax.LexQuartz
import Quartz.Syntax.ParQuartz
import Quartz.Syntax.SkelQuartz
import Quartz.Syntax.PrintQuartz
import Quartz.Syntax.AbsQuartz

import Quartz.Syntax.ErrM

type ParseFun a = [Token] -> Err a

showTree :: (Show a, Print a) => a -> IO ()
showTree tree
 = do
      putStrLn $ "\n[AST]\n\n" ++ show tree
      putStrLn $ "\n[Linearized]\n\n" ++ printTree tree

run :: ParseFun Program -> String -> IO ()
run parse str = let toks = myLexer str in case parse toks of
  Bad s -> putStrLn s >> exitFailure
  Ok (Prog defs) -> mapM_ showTree defs >> exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run pProgram
    [fname] -> do
      fd <- openFile fname ReadMode
      hGetContents fd >>= run pProgram
