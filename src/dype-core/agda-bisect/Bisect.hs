-- | dype git bisect wrapper (stub for future implementation)
module Main where
import System.Process (callProcess)
import System.Directory (getCurrentDirectory)
main :: IO ()
main = do
  dir <- getCurrentDirectory
  putStrLn $ "Running dype tests in " ++ dir
  callProcess "cabal" ["test", "dype-test"]
