module Main where
import Dayan.ProofGen.AST
import Dayan.ProofGen.Emit
import Dayan.ProofGen.Templates
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  -- 生成一个最小 CRT 验证文件
  let smallFile = genT6VerificationFile "Generated.CRT" [(0, (0,0)), (1, (1,1)), (144, (0,6))]
  TIO.writeFile "/tmp/generated_crt.agda" (emitFile smallFile)
  putStrLn "Generated /tmp/generated_crt.agda"

  -- 生成 Tryte roundtrip 验证文件
  let tryteFile = genTryteVerificationFile "Generated.Tryte"
  TIO.writeFile "/tmp/generated_tryte.agda" (emitFile tryteFile)
  putStrLn "Generated /tmp/generated_tryte.agda"
