{-# LANGUAGE OverloadedStrings #-}
module Main where
import Dayan.ProofGen.AST
import Dayan.ProofGen.Emit
import Dayan.ProofGen.Templates
import Dayan.Compute.CRT (projectAll)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  let entries = take 10 [(i, (fromIntegral p, fromIntegral t)) | (i, (p, t)) <- projectAll]
      full = AgdaFile ""
        "Generated.Verified"
        ( [ DImport "Agda.Builtin.Nat"
          , DImport "Agda.Builtin.Equality"
          , DComment "CRT lookup — Da-Yan Engine generated"
          , DPostulate "lookupCrt" (TFun TNat TNat)
          , DComment "CRT table proofs (10 sample entries)"
          ] ++ genAllCrtLookups entries )
  TIO.writeFile "/tmp/Verified.agda" (emitFile full)
  putStrLn "Generated"
