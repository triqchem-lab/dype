{-# LANGUAGE OverloadedStrings #-}
module Main where
import Dayan.ProofGen.AST (AgdaFile(..), fileDecls)
import Dayan.ProofGen.Emit (emitFile)
import Dayan.ProofGen.Templates (genT6VerificationFile)
import Dayan.Compute.CRT (projectAll)
import qualified Data.Text.IO as TIO
import System.CPUTime

main :: IO ()
main = do
  start <- getCPUTime
  let entries = [(i, (fromIntegral p, fromIntegral t)) | (i, (p, t)) <- projectAll]
      f = genT6VerificationFile "Generated.FullCRT" entries
      agda = emitFile f
  TIO.writeFile "/tmp/FullCRT.agda" agda
  end <- getCPUTime
  let ms = (end - start) `div` 1000000000
  putStrLn $ "Generated " ++ show (length entries) ++ " CRT entries in " ++ show ms ++ "s"
  putStrLn $ "Decls: " ++ show (length (fileDecls f))
