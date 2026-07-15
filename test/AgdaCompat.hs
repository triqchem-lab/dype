-- test/AgdaCompat.hs — dype vs Agda 测试套件兼容性扫描
--
-- 对 Agda 测试目录中的 .agda 文件运行 dype 管线:
--   .agda → parseDy → emitFile → write temp → agda verify
-- 输出统计报告

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import Text.Printf (printf)
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dayan.Parse.Dy (parseDy)
import Dayan.ProofGen.Emit (emitFile)

data FileResult = ParseFail | VerifyFail Text | VerifyOk
  deriving (Show, Eq)

resultLabel :: FileResult -> String
resultLabel ParseFail    = "PARSE"
resultLabel (VerifyFail _) = "VERIFY_FAIL"
resultLabel VerifyOk     = "OK"

scanDir :: FilePath -> IO ([FilePath], [FileResult])
scanDir dir = do
  ents <- listDirectory dir
  let agdaFiles = filter (\f -> takeExtension f == ".agda") ents
  results <- mapM (testFile dir . (dir </>)) agdaFiles
  pure (agdaFiles, results)

testFile :: FilePath -> FilePath -> IO FileResult
testFile _agdaRoot fp = do
  content <- TIO.readFile fp
  case parseDy content of
    Left _ -> pure ParseFail
    Right (_modName, agdaFile) ->
      withSystemTempDirectory "dayan-agt" $ \tmp -> do
        let agdaSrc = emitFile agdaFile
            agdaPath = tmp </> "Test.agda"
        TIO.writeFile agdaPath agdaSrc
        (exit, _, stderr) <- readProcessWithExitCode "agda"
          ["--include-path=" <> tmp, agdaPath] ""
        pure $ case exit of
          ExitSuccess -> VerifyOk
          ExitFailure _ -> VerifyFail (T.pack stderr)

main :: IO ()
main = do
  let testDir = "/data/work/functional-programming/agda/test/Succeed"
  putStrLn $ "Scanning: " ++ testDir
  (files, results) <- scanDir testDir
  let total = length results
      parseFail = length $ filter (== ParseFail) results
      verifyFail = length [() | VerifyFail _ <- results]
      verifyOk   = length [() | VerifyOk <- results]
  putStrLn $ replicate 60 '='
  printf "Total files:      %d\n" total
  printf "Parse FAIL:       %d (%.1f%%)\n" parseFail (100.0 * fromIntegral parseFail / fromIntegral total :: Double)
  printf "Verify FAIL:      %d (%.1f%%)\n" verifyFail (100.0 * fromIntegral verifyFail / fromIntegral total :: Double)
  printf "Verify OK:        %d (%.1f%%)\n" verifyOk (100.0 * fromIntegral verifyOk / fromIntegral total :: Double)
  putStrLn $ replicate 60 '='
  -- Show all Verify OK
  let okFiles = [f | (f, VerifyOk) <- zip files results]
  unless (null okFiles) $ do
    putStrLn "\nVerify OK files:"
    mapM_ (putStrLn . ("  " ++)) okFiles
  -- Show first 5 verify failures
  let failures = [(f, r) | (f, VerifyFail r) <- zip files results]
  unless (null failures) $ do
    putStrLn "\nFirst 5 verify failures:"
    mapM_ (\(f, r) -> do
      putStrLn $ "  " ++ takeFileName f
      putStrLn $ "    " ++ T.unpack (T.take 120 r)
      ) (take 5 failures)
