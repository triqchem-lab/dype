{-# LANGUAGE OverloadedStrings #-}
module Main where
import Dayan.ProofGen.AST
import Dayan.ProofGen.Emit
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  let f = AgdaFile "{-# OPTIONS --rewriting #-}" "Generated.T6Verification"
            [ DOpenUsing "Data.Nat" ["_≤_"]
            , DOpen "Sovereign.Structology.T6"
            , DComment "Da-Yan Engine auto-generated"
            , DPostulate "all-729-bounded"
                (TPi "x" (TDef "T6Lattice")
                  (TApp (TApp (TDef "_≤_") (apps (Def "toℕ-sum") [Var "x"])) (Lit (LNat 728))))
            ]
  TIO.writeFile "/tmp/T6Verification.agda" (emitFile f)
  putStrLn "Generated /tmp/T6Verification.agda"
