{-# LANGUAGE FlexibleContexts #-}
module Linker where

import Prelude hiding (mod)
import Control.Monad.Except
import System.Directory

findStdLib :: IO FilePath
findStdLib = return "stdlib" -- TODO

moduleSearchDirectories :: IO [FilePath]
moduleSearchDirectories = return [".", "./stdlib/"] -- TODO add env, detect stdlib better

findModule :: MonadError String m => MonadIO m => String -> m String
findModule mod = do
  dirs <- liftIO $ moduleSearchDirectories
  f <- liftIO $ findFile dirs (mod ++ ".quartz")
  case f of
    Nothing -> throwError $ ("Cannot find module " ++ mod)
    Just path -> return path
