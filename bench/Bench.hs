module Main where
import System.CPUTime
import Dayan.Core.Trit
import Dayan.Core.Tryte (mkTryte, decode)
import Dayan.Compute.CRT (lookupCrt)
import Data.Word (Word8)

main = do
  putStrLn "=== Da-Yan Benchmarks ==="
  bench "CRT lookupCrt"  500000  (lookupCrt 3312 :: (Word8, Word8))
  bench "Tryte decode"   100000  (length (decode (mkTryte 364)))
  bench "Trit add"      1000000  (add N Z)

bench :: String -> Int -> a -> IO ()
bench name n f = do
  start <- getCPUTime
  let go 0 = return ()
      go k = f `seq` go (k-1)
  go n
  end <- getCPUTime
  let ns = (end - start) `div` fromIntegral n
  putStrLn $ name ++ replicate (20 - length name) ' ' ++ show ns ++ " ns/op"
