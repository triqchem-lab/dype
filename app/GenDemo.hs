{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Dayan.ProofGen.AST
import Dayan.ProofGen.Emit
import Dayan.Verify.Agda (verify)

main = do
  let f = AgdaFile "" "Generated.Verified" $
            [ DOpen "Agda.Builtin.Nat"
            , DOpen "Agda.Builtin.Equality"
            , DComment "Da-Yan Engine + Verify pipeline"
            , DDef "p0" (TApp (TApp (TDef "_≡_") (Lit (LNat 0))) (Lit (LNat 0)))
                [Clause [] Refl]
            ]
  TIO.writeFile "/tmp/GenVerify.agda" (emitFile f)
  result <- verify f
  putStrLn $ "Verify result: " ++ show result
