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
import qualified AST.Typed as Typed
import Interpreter

import Builtins
import Passes.TypeCheck
import Linker
import Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.Text as PrettyText

type ParseFun a = [Token] -> Err a

preprocessDeclaration :: Declaration -> IO Typed.Declaration
preprocessDeclaration decl
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
      return embedded

processDeclaration :: Passes.TypeCheck.Env -> Typed.Declaration -> IO Typed.Declaration
processDeclaration env embedded = do
      let unified = checkDeclaration env embedded
      case unified of
        Left err -> putStrLn "Unification error" >> print err >> error "Declaration failed to typecheck, aborting."
        Right unified -> do
          putStrLn "[Unified]"
          PrettyText.putDoc $ Pretty.pretty unified
          putStrLn ""
          return unified

run :: ParseFun Program -> String -> IO ()
run parse str = let toks = myLexer str in case parse toks of
  Bad s -> putStrLn s >> exitFailure
  Ok (Prog defs) -> do
    embedded <- mapM preprocessDeclaration defs
    let env1 = makeTypeEnv builtins
    let env2 = introduceTopLevelTypes embedded env1
    case env2 of
      Right env -> do
        defsTyped <- mapM (processDeclaration env) embedded
        putStrLn "[Executing main()]"
        let res = runWithBuiltins builtins $ withDeclared defsTyped $ interpret (Typed.EVar undefined "main")
        print res
        exitSuccess
      Left error -> do
        print error
        exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run pProgram
    [fname] -> do
      fd <- openFile fname ReadMode
      hGetContents fd >>= run pProgram
    _ -> putStrLn "too many args"
