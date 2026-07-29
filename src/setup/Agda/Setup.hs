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
import System.FilePath    ( (</>) )
import System.Directory   ( getAppUserDataDirectory, getCurrentDirectory, makeAbsolute )

-- | Get the path to ~/.agda (overridable via AGDA_DIR env var).
getAgdaAppDir :: IO FilePath
getAgdaAppDir = do
  env <- lookupEnv "AGDA_DIR"
  case env of
    Just d  -> return d
    Nothing -> getAppUserDataDirectory "agda"

-- | Get the data directory (prim library etc).
--   Respects AGDA_DATADIR env var, falls back to src/data relative to executable.
getDataDir :: IO FilePath
getDataDir = do
  env <- lookupEnv "AGDA_DATADIR"
  case env of
    Just d  -> makeAbsolute d
    Nothing -> do
      cwd <- getCurrentDirectory
      makeAbsolute (cwd </> "src/data")

-- | Get a specific data file path.
getDataFileName :: FilePath -> IO FilePath
getDataFileName f = (</> f) <$> getDataDir

-- | Run setup (no-op in stub).
setup :: Bool -> IO ()
setup _ = return ()
