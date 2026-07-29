{-# OPTIONS_GHC -Wunused-imports #-}

-- | Agda's self-setup (stub for dual-package compilation).
-- No Paths_*, no TemplateHaskell, no filelock dependency.

module Agda.Setup
  ( getAgdaAppDir
  , getDataDir
  , getDataFileName
  , setup
  )
where

import System.Environment ( lookupEnv )
import System.FilePath    ( (</>), takeDirectory )
import System.Directory   ( getAppUserDataDirectory, getCurrentDirectory
                          , makeAbsolute, doesDirectoryExist )

-- | Get the path to ~/.agda (overridable via AGDA_DIR env var).
getAgdaAppDir :: IO FilePath
getAgdaAppDir = do
  env <- lookupEnv "AGDA_DIR"
  case env of
    Just d  -> return d
    Nothing -> getAppUserDataDirectory "agda"

-- | Get the data directory (prim library etc).
--   Walks up from CWD to find src/data/; no env var required.
getDataDir :: IO FilePath
getDataDir = do
  cwd <- getCurrentDirectory
  dir <- findDataDir cwd
  makeAbsolute dir
  where
    findDataDir d = do
      let candidate = d </> "src" </> "data"
      exists <- doesDirectoryExist candidate
      if exists then return candidate
      else do
        let parent = takeDirectory d
        if parent == d then return candidate  -- reached root, use CWD fallback
        else findDataDir parent

-- | Get a specific data file path.
getDataFileName :: FilePath -> IO FilePath
getDataFileName f = (</> f) <$> getDataDir

-- | Run setup (no-op in stub).
setup :: Bool -> IO ()
setup _ = return ()
