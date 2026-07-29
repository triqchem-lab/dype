-- | Wrapper for "Agda.Main".
module Main (main) where

import Agda.Main ( runAgda )
import Prelude ( IO )

main :: IO ()
main = runAgda []
