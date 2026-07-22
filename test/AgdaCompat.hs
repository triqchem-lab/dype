-- test/AgdaCompat.hs — dype vs Agda 测试套件兼容性扫描 (v2: Agda.Syntax.Parser)
--
-- 使用 Agda.Syntax.Parser 解析 .agda 文件 (100% 语法覆盖)，
-- 全量透传策略验证 parse → emit → agda verify 管线。
--
-- include-path 策略:
--   - Agda 通过 $AGDA_DIR 自动发现标准库 (builtins)
--   - 只需额外添加 test/ 让 Common.* 等测试辅助模块可被找到

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Text.Printf (printf)
import Control.Monad (unless)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dayan.Parse.Agda (parseAgdaFile, classifyFailure, AgdaCompatIssue(..))
import Dayan.ProofGen.AST (AgdaFile(..))

data FileResult
  = ParseFail String
  | VerifyFail Text AgdaCompatIssue
  | VerifyOk
  deriving (Show)

scanDir :: FilePath -> FilePath -> FilePath -> IO ([FilePath], [FileResult])
scanDir dir succeedDir agdaTestDir = do
  ents <- listDirectory dir
  let agdaFiles = filter (\f -> takeExtension f == ".agda") ents
  results <- mapM (testFile succeedDir agdaTestDir . (dir </>)) agdaFiles
  pure (agdaFiles, results)

testFile :: FilePath -> FilePath -> FilePath -> IO FileResult
testFile succeedDir agdaTestDir fp = do
  content <- TIO.readFile fp
  result <- parseAgdaFile fp content
  case result of
    Left err -> pure $ ParseFail err
    Right _agdaFile -> do
      -- 全量透传: emit = 原始源文本, 直接在原始文件上验证
      -- --include-path: Succeed/ (同级模块) + test/ (Common.* 跨目录)
      -- 逐文件标志: 模拟 Agda 测试套件的 per-test flags
      let extraFlags = perFileFlags fp
      (exit, _, stderr) <- readProcessWithExitCode "agda"
        (["--include-path=" <> succeedDir
        , "--include-path=" <> agdaTestDir]
        ++ extraFlags ++ [fp]) ""
      let stderrT = T.pack stderr
      pure $ case exit of
        ExitSuccess -> VerifyOk
        ExitFailure _ -> VerifyFail stderrT (classifyFailure stderrT)

-- | 逐文件标志: 模拟 Agda 测试套件的 per-test flags
perFileFlags :: FilePath -> [String]
perFileFlags fp
  | "ExecAgda" `isInfixOf` fp              = ["--allow-exec"]
  | "Issue723.agda" `isInfixOf` fp         = ["--no-libraries"]
  | "Issue1760" `isInfixOf` fp             = ["--recursive-record-needs-inductivity"]
  | "RecursiveInstanceSearchLevel" `isInfixOf` fp = ["--recursive-record-needs-inductivity"]
  | "Issue1147" `isInfixOf` fp             = ["--recursive-record-needs-inductivity"]
  | otherwise                              = []

main :: IO ()
main = do
  let testDir = "/data/work/functional-programming/agda/test/Succeed"
      agdaTestDir = "/data/work/functional-programming/agda/test"
  putStrLn $ "Scanning: " ++ testDir
  (files, results) <- scanDir testDir testDir agdaTestDir
  let total = length results
      parseFail = length [() | ParseFail _ <- results]
      verifyOk   = length [() | VerifyOk <- results]
      verifyFail = length [() | VerifyFail _ _ <- results]
      l1 = length [() | VerifyFail _ TheoryDiff <- results]
      l2 = length [() | VerifyFail _ ProofIssue <- results]
      l3 = length [() | VerifyFail _ Engineering <- results]
  putStrLn $ replicate 60 '='
  printf "Total files:      %d\n" total
  printf "Parse FAIL:       %d (%.1f%%)\n" parseFail (100.0 * fromIntegral parseFail / fromIntegral total :: Double)
  printf "Verify OK:        %d (%.1f%%)\n" verifyOk (100.0 * fromIntegral verifyOk / fromIntegral total :: Double)
  printf "Verify FAIL:      %d (%.1f%%)\n" verifyFail (100.0 * fromIntegral verifyFail / fromIntegral total :: Double)
  printf "  L1 (理论差异):  %d\n" l1
  printf "  L2 (证明问题):  %d\n" l2
  printf "  L3 (工程衔接):  %d\n" l3
  putStrLn $ replicate 60 '='
  let okFiles = [f | (f, VerifyOk) <- zip files results]
  unless (null okFiles) $ do
    putStrLn $ "\nVerify OK (first 5 of " ++ show (length okFiles) ++ "):"
    mapM_ (putStrLn . ("  " ++)) (take 5 okFiles)
  let l3Files = [f | (f, VerifyFail _ Engineering) <- zip files results]
  unless (null l3Files) $ do
    putStrLn $ "\nL3 Engineering FAIL (" ++ show (length l3Files) ++ "):"
    mapM_ (putStrLn . ("  " ++)) l3Files
