{-# LANGUAGE BangPatterns #-}
module Main where
import System.CPUTime
import Data.Word (Word8)
import Data.Bits (xor)
import Dayan.Core.Trit
import Dayan.Core.Tryte (mkTryte, decode)
import Dayan.Compute.CRT (lookupCrt)
import Dayan.Compute.Det

main :: IO ()
main = do
  putStrLn "=== Da-Yan Benchmarks ==="
  putStrLn ""
  putStrLn "--- Core ---"
  bench "CRT lookupCrt"  500000  (lookupCrt 3312 :: (Word8, Word8))
  bench "Tryte decode"   100000  (length (decode (mkTryte 364)))
  bench "Trit add"      1000000  (add N Z)
  bench "Trit mul"      1000000  (mul P P)
  putStrLn ""
  putStrLn "--- Det Engine (CRT 行列式) ---"
  benchIO "det3Fast (O(1) table)" 1000000 $ \i ->
    let m = idxToMat3 i in toNat (det3Fast m)
  benchIO "det3 (Sarrus direct)"  1000000 $ \i ->
    let m = idxToMat3 i in toNat (det3 m)
  bench "det4 (I4)"       100000  (toNat (det4 identity4))
  bench "det2 (I2)"      1000000  (toNat (det2 identity2))
  bench "crtDetNonzero"  1000000  (crtDetNonzero (crtDecompose3 identity3))
  bench "detNonzeroStructural" 100000 (detNonzeroStructural id)
  putStrLn ""
  putStrLn "--- Comparison ---"
  putStrLn "Agda O(N!) 9x9 det: >2min timeout"
  putStrLn "dype O(1) det3Fast:  see above (ns/op)"

bench :: String -> Int -> a -> IO ()
bench name n f = do
  start <- getCPUTime
  let go 0 = return ()
      go k = f `seq` go (k-1)
  go n
  end <- getCPUTime
  let ns = (end - start) `div` fromIntegral n
  putStrLn $ name ++ replicate (30 - length name) ' ' ++ show ns ++ " ns/op"

benchIO :: String -> Int -> (Int -> Word8) -> IO ()
benchIO name n f = do
  start <- getCPUTime
  let go !acc 0 = return acc
      go !acc k = go (acc `xor` f k) (k-1)
  checksum <- go 0 n
  end <- getCPUTime
  let ns = (end - start) `div` fromIntegral n
  putStrLn $ name ++ replicate (30 - length name) ' ' ++ show ns ++ " ns/op (ck=" ++ show checksum ++ ")"

idxToMat3 :: Int -> Mat3
idxToMat3 idx =
  let (a, r1) = idx `divMod` 6561
      (b, r2) = r1  `divMod` 2187
      (c, r3) = r2  `divMod` 729
      (d, r4) = r3  `divMod` 243
      (e, r5) = r4  `divMod` 81
      (f, r6) = r5  `divMod` 27
      (g, r7) = r6  `divMod` 9
      (h, i') = r7  `divMod` 3
      toT 0 = N; toT 1 = Z; toT _ = P
  in (toT a, toT b, toT c, toT d, toT e, toT f, toT g, toT h, toT i')
