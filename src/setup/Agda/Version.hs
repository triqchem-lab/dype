{-# OPTIONS_GHC -Wunused-imports #-}

module Agda.Version
  ( version
  , package
  , docsUrl
  ) where

-- | The version of dype (hardcoded to avoid Paths_* dependency conflicts
-- between agda-syntax and dype-core packages).
version :: String
version = "2.9.0"

-- | This package name.
package :: String
package = "dype-core"

-- | URL to the documentation for the given section.
docsUrl :: String -> String
docsUrl section = "https://agda.readthedocs.io/en/v" ++ version ++ "/" ++ section
