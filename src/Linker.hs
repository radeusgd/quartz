{-# LANGUAGE FlexibleContexts #-}
module Linker where

import Prelude hiding (mod)
import Control.Monad.Except
import System.Directory
import System.Environment

stdlibEnvName :: String
stdlibEnvName = "QUARTZ_LIBRARY_PATH"

findStdLib :: IO FilePath
findStdLib = do
  p <- lookupEnv stdlibEnvName
  case p of
    Nothing -> do
      putStrLn (stdlibEnvName ++ " not set, assuming you stdlib/ is in current directory")
      return "./stdlib/"
    Just d -> return d

moduleSearchDirectories :: IO [FilePath]
moduleSearchDirectories = do
  std <- findStdLib
  return [".", std]

findModule :: MonadError String m => MonadIO m => String -> m String
findModule mod = do
  dirs <- liftIO $ moduleSearchDirectories
  f <- liftIO $ findFile dirs (mod ++ ".quartz")
  case f of
    Nothing -> throwError $ ("Cannot find module " ++ mod)
    Just path -> return path
