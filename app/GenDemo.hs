{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Dayan.ProofGen.AST
import Dayan.ProofGen.Emit

main = do
  let proof n = DDef ("p" <> pack (show n))
                  (TApp (TApp (TDef "_≡_") (Lit (LNat n))) (Lit (LNat n)))
                  [Clause [] Refl]
      f = AgdaFile "" "Generated.Verified" $
            [ DOpen "Agda.Builtin.Nat"
            , DOpen "Agda.Builtin.Equality"
            , DComment "Da-Yan Engine generated — 5 trivial identity proofs"
            ] ++ map proof [0,1,2,3,4]
  TIO.writeFile "/tmp/Verified.agda" (emitFile f)
  putStrLn "Done"
