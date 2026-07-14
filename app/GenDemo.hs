{-# LANGUAGE OverloadedStrings #-}
module Main where
import Dayan.ProofGen.AST
import Dayan.ProofGen.Emit
import Dayan.ProofGen.Templates
import Dayan.Compute.CRT
import qualified Data.Text.IO as TIO
import Data.Word (Word16)

main :: IO ()
main = do
  let entries = take 10 [(i, (fromIntegral p, fromIntegral t)) | (i, (p, t)) <- projectAll]
      f = genT6VerificationFile "Generated.CRT" entries
  TIO.writeFile "/tmp/generated_crt.agda" (emitFile f)
  putStrLn "Generated /tmp/generated_crt.agda"
