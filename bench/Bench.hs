module Main where
import Criterion.Main
import Dayan.Core.Tryte (mkTryte, decode)
import Dayan.Core.Trit
import Data.Word

main :: IO ()
main = defaultMain
  [ bgroup "Trit"
      [ bench "add N Z" $ nf (add N) Z
      , bench "mul P P" $ whnf (mul P) P
      , bench "superpose N P" $ nf (superpose N) P
      ]
  , bgroup "Tryte"
      [ bench "length . decode . mkTryte" $ nf (length . decode . mkTryte) (365 :: Word8)
      ]
  ]
