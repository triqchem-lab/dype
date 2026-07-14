{-# LANGUAGE OverloadedStrings #-}
module Main where
import Dayan.ProofGen.AST
import Dayan.ProofGen.Emit (emitFile)
import Dayan.Parse.Dy (parseDy)
import Dayan.Verify.Agda (verify)
import qualified Data.Text.IO as TIO

main = do
  let f = AgdaFile "" "Generated.DyTest"
            [ DOpen "Agda.Builtin.Nat"
            , DOpen "Agda.Builtin.Equality"
            , DDef "p0" (TApp (TApp (TDef "_≡_") (Lit (LNat 0))) (Lit (LNat 0))) [Clause [] Refl]
            ]
  result <- verify f
  putStrLn $ "Gen→Emit→Verify: " ++ show result
  case parseDy "-- test\ndef p: Set\np = Set" of
    Left err -> putStrLn $ "Parse: " ++ err
    Right (_, f2) -> putStrLn $ "Parse: " ++ show (length (fileDecls f2)) ++ " decls"
