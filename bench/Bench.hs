module Main where
import Criterion.Main
import Dayan.Core.Tryte (mkTryte, decode)
import Dayan.Core.Trit
import Data.Word (Word16)

main :: IO ()
main = defaultMain
  [ bgroup "Trit"
      [ bench "add N Z" $ whnf (add N) Z
      , bench "mul P P" $ whnf (mul P) P
      , bench "superpose N P" $ whnf (superpose N) P
      ]
  , bgroup "Tryte"
      [ bench "mkTryte 365" $ whnf mkTryte (365 :: Word16)
      , bench "decode 365" $ whnf (length . decode . mkTryte) (365 :: Word16)
      ]
  ]
