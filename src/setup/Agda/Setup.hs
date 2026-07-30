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

import qualified Paths_dype_core

-- | Get the path to ~/.agda (overridable via AGDA_DIR env var).
getAgdaAppDir :: IO FilePath
getAgdaAppDir = do
  env <- lookupEnv "AGDA_DIR"
  case env of
    Just d  -> return d
    Nothing -> getAppUserDataDirectory "agda"

-- | Get the data directory (prim library etc).
--   Uses cabal Paths module like agda original.
getDataDir :: IO FilePath
getDataDir = Paths_dype_core.getDataDir

-- | Get a specific data file path.
getDataFileName :: FilePath -> IO FilePath
getDataFileName f = (</> f) <$> getDataDir

-- | Run setup (no-op in stub).
setup :: Bool -> IO ()
setup _ = return ()
