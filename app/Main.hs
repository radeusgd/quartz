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

import Passes.Desugar
import Passes.EmbedTypes

import Builtins
import Passes.TypeCheck
import Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.Text as PrettyText

type ParseFun a = [Token] -> Err a

showDeclaration :: Declaration -> IO ()
showDeclaration decl
 = do
      putStrLn $ "[AST]\n\n" ++ show decl
      putStrLn $ "[Linearized]\n" ++ printTree decl
      let raw = desugarDeclaration decl
      putStrLn "[Raw]"
      PrettyText.putDoc $ Pretty.pretty raw
      putStrLn ""
      let embedded = embedDeclaration raw
      putStrLn "[Not unified]"
      PrettyText.putDoc $ Pretty.pretty embedded
      putStrLn ""
      let unified = checkDeclaration initialEnv embedded
      case unified of
        Left err -> putStrLn "Unification error" >> print err
        Right unified -> do
          putStrLn "[Unified]"
          PrettyText.putDoc $ Pretty.pretty unified
      putStrLn ""

run :: ParseFun Program -> String -> IO ()
run parse str = let toks = myLexer str in case parse toks of
  Bad s -> putStrLn s >> exitFailure
  Ok (Prog defs) -> mapM_ showDeclaration defs >> exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run pProgram
    [fname] -> do
      fd <- openFile fname ReadMode
      hGetContents fd >>= run pProgram
    _ -> putStrLn "too many args"
