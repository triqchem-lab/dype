{-# LANGUAGE OverloadedStrings #-}
module Main where
import Dayan.ProofGen.AST
import Dayan.ProofGen.Emit
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  let proof n = DDef ("p" <> show n)
                  (TApp (TApp (TDef "≡") (apps (Def "id") [Lit (LNat n)])) (Lit (LNat n)))
                  [Clause [] Refl]
      full = AgdaFile ""
        "Generated.Verified"
        ( [ DOpen "Agda.Builtin.Nat"
          , DPostulate "id" (TFun TNat TNat)
          ] ++ map proof [0,1,2,3,4,5,6,7,8,9] )
  TIO.writeFile "/tmp/Verified.agda" (emitFile full)
  putStrLn "Generated"
