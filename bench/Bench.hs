module Main where
import Criterion.Main
import Dayan.Compute.CRT
import Dayan.Core.Tryte (mkTryte, decode)
import Dayan.Core.Trit
import Data.Word

main :: IO ()
main = defaultMain
  [ bgroup "CRT" 
      [ bench "lookupCrt 0"    $ nf lookupCrt 0
      , bench "lookupCrt 3312" $ nf lookupCrt 3312
      , bench "lookupCrt 6623" $ nf lookupCrt 6623
      , bench "lookupIndex (0,0)" $ nf (lookupIndex 0 0) ()
      , bench "lookupIndex (143,45)" $ nf (lookupIndex 143 45) ()
      ]
  , bgroup "Trit"
      [ bench "add N Z" $ nf (add N) Z
      , bench "mul P P" $ nf (mul P P) ()
      , bench "superpose N P" $ nf (superpose N) P
      ]
  , bgroup "Tryte"
      [ bench "decode 364" $ nf (length . decode . mkTryte) 364
      ]
  ]
