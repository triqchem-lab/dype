-- Stub: Agda.Setup — data file management (not needed for parsing)
module Agda.Setup (getDataFileName, getAgdaAppDir) where

import System.FilePath ((</>))

getDataFileName :: FilePath -> IO FilePath
getDataFileName f = pure ("data" </> f)

getAgdaAppDir :: IO FilePath
getAgdaAppDir = pure "."
