{-# OPTIONS_GHC -Wunused-imports #-}

module Agda.VersionCommit where

import Agda.Version

-- | dype's version suffixed with the git commit hash.
-- Hardcoded to avoid gitrev TemplateHaskell dependency conflicts
-- between agda-syntax and dype-core packages.
versionWithCommitInfo :: String
versionWithCommitInfo = version ++ "-dype"

-- | Information about current git commit.
commitInfo :: Maybe String
commitInfo = Just "dype"
