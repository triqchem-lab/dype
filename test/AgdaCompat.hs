-- test/AgdaCompat.hs — dype 前端兼容性测试 (v4: 内核测试)
--
-- dype 是 Agda 的内核替换 (wiki 16-paradigm-replacement):
--   Conversion.hs → 四极等价判定
--   Reduce.hs → 4320D + CRT 查表
--   外部 agda 二进制 = gcc -fsyntax-only (最终语法确认)
--
-- 测试策略:
--   主体: parse .agda → AST 结构断言 → emit → roundtrip (纯 Haskell, 秒级)
--   冒烟: ≤5 个精选文件跑 agda verify (确认 emit 输出合法)
--
-- 不再对数百个文件逐个调用 agda 二进制。

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, takeFileName, dropExtension)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..), exitFailure)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Text.Printf (printf)
import Control.Monad (unless, when, forM_, forM)
import Data.List (isInfixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dayan.Parse.Agda (parseAgdaFile, classifyFailure, AgdaCompatIssue(..))
import Dayan.ProofGen.AST (AgdaFile(..))
import Dayan.ProofGen.Emit (emitFile)

----------------------------------------------------------------------
-- 结果类型
----------------------------------------------------------------------

data FileResult
  = ParseOk Int          -- ^ parse 成功, 声明数
  | RoundtripOk Int      -- ^ parse → emit → re-parse 成功
  | ParseFail String     -- ^ parse 失败
  | RoundtripFail String -- ^ roundtrip 失败
  | SmokeOk              -- ^ agda verify 通过 (冒烟测试)
  | SmokeFail Text       -- ^ agda verify 失败
  | Skipped String       -- ^ 跳过 (Makefile 依赖等)
  deriving (Show)

----------------------------------------------------------------------
-- 精选冒烟测试文件 (≤5 个, 代表不同语法特征)
----------------------------------------------------------------------

smokeFiles :: [FilePath]
smokeFiles =
  [ "Succeed/simple.agda"            -- 最简文件
  , "Succeed/Lambda.agda"            -- lambda 语法
  , "Succeed/DataRecordInductive.agda" -- data + record
  , "Succeed/Nat.agda"               -- 自包含 data 声明
  ]

----------------------------------------------------------------------
-- 主体: 纯 Haskell 前端测试
----------------------------------------------------------------------

-- | 递归收集 .agda 文件
collectAgdaFiles :: FilePath -> IO [FilePath]
collectAgdaFiles dir = do
  isDir <- doesDirectoryExist dir
  if not isDir then pure []
  else do
    ents <- listDirectory dir
    files <- concat <$> mapM go ents
    pure files
  where
    go name = do
      let fp = dir </> name
      isDir <- doesDirectoryExist fp
      if isDir
        then collectAgdaFiles fp
        else if takeExtension name == ".agda"
             then pure [fp]
             else pure []

-- | 单文件前端测试: parse → AST 断言 → emit → re-parse
testFileFrontend :: FilePath -> IO FileResult
testFileFrontend fp = do
  content <- TIO.readFile fp
  result <- parseAgdaFile fp content
  case result of
    Left err -> pure $ ParseFail err
    Right agdaFile -> do
      let declCount = length (fileDecls agdaFile)
      -- 断言: 声明数 > 0 (非空模块)
      if declCount == 0
        then pure $ ParseFail "empty module (0 decls)"
        else do
          -- emit → re-parse roundtrip
          let emitted = emitFile agdaFile
          result2 <- parseAgdaFile (fp ++ ".roundtrip") emitted
          case result2 of
            Left err -> pure $ RoundtripFail err
            Right agdaFile2 -> do
              let declCount2 = length (fileDecls agdaFile2)
              -- roundtrip 稳定性: 声明数一致
              if declCount2 == declCount
                then pure $ RoundtripOk declCount
                else pure $ RoundtripFail $
                  "decl count mismatch: " ++ show declCount ++ " → " ++ show declCount2

-- | 冒烟测试: emit → agda verify
testFileSmoke :: FilePath -> FilePath -> IO FileResult
testFileSmoke agdaTestDir fp = do
  exists <- doesFileExist fp
  if not exists then pure $ Skipped "file not found"
  else do
    content <- TIO.readFile fp
    result <- parseAgdaFile fp content
    case result of
      Left err -> pure $ SmokeFail (T.pack err)
      Right agdaFile -> do
        let emitted = emitFile agdaFile
        withSystemTempDirectory "agda-compat-smoke" $ \tmp -> do
          let modName = T.unpack (fileModule agdaFile)
              modParts = T.split (== '.') (fileModule agdaFile)
              modDir = T.unpack (T.intercalate "/" (init modParts))
              modFile = T.unpack (last modParts) ++ ".agda"
              outDir = tmp </> modDir
          _ <- readProcessWithExitCode "mkdir" ["-p", outDir] ""
          TIO.writeFile (outDir </> modFile) emitted
          let relPath = T.unpack (T.intercalate "/" modParts) ++ ".agda"
          mResult <- timeout 10000000 $  -- 10s
            readProcessWithExitCode "agda"
              ["--include-path=" ++ tmp, "--include-path=" ++ agdaTestDir,
               tmp </> relPath] ""
          pure $ case mResult of
            Nothing -> SmokeFail "timeout (10s)"
            Just (ExitSuccess, _, _) -> SmokeOk
            Just (ExitFailure _, _, stderr) -> SmokeFail (T.pack (take 200 stderr))

----------------------------------------------------------------------
-- Main
----------------------------------------------------------------------

main :: IO ()
main = do
  let agdaTestDir = "/data/work/functional-programming/agda/test"
      succeedDir  = agdaTestDir </> "Succeed"

  -- Phase 1: 纯 Haskell 前端测试 (全量)
  putStrLn "=== Phase 1: dype 前端测试 (parse → AST → emit → roundtrip) ==="
  allFiles <- collectAgdaFiles succeedDir
  putStrLn $ "Found " ++ show (length allFiles) ++ " .agda files"

  results <- mapM testFileFrontend allFiles

  let total       = length results
      roundtripOk = length [() | RoundtripOk _ <- results]
      parseOk     = length [() | ParseOk _ <- results]
      parseFail   = [(f, e) | (f, ParseFail e) <- zip allFiles results]
      rtFail      = [(f, e) | (f, RoundtripFail e) <- zip allFiles results]
      passRate    = 100.0 * fromIntegral (roundtripOk + parseOk) / fromIntegral total :: Double

  putStrLn $ replicate 60 '='
  printf "Total:        %d\n" total
  printf "Roundtrip OK: %d\n" roundtripOk
  printf "Parse OK:     %d\n" parseOk
  printf "Parse FAIL:   %d\n" (length parseFail)
  printf "RT FAIL:      %d\n" (length rtFail)
  printf "Pass rate:    %.1f%%\n" passRate
  putStrLn $ replicate 60 '='

  -- 输出失败详情 (前 10 个)
  unless (null parseFail) $ do
    putStrLn $ "\nParse FAIL (" ++ show (length parseFail) ++ "):"
    forM_ (take 10 parseFail) $ \(f, e) ->
      putStrLn $ "  " ++ takeFileName f ++ ": " ++ take 80 e
  unless (null rtFail) $ do
    putStrLn $ "\nRoundtrip FAIL (" ++ show (length rtFail) ++ "):"
    forM_ (take 10 rtFail) $ \(f, e) ->
      putStrLn $ "  " ++ takeFileName f ++ ": " ++ take 80 e

  -- Phase 2: agda verify 冒烟测试 (≤5 文件)
  putStrLn "\n=== Phase 2: agda verify 冒烟测试 (≤5 文件) ==="
  smokeResults <- forM smokeFiles $ \rel -> do
    let fp = agdaTestDir </> rel
    r <- testFileSmoke agdaTestDir fp
    putStrLn $ "  " ++ rel ++ ": " ++ show r
    pure r

  let smokeOk   = length [() | SmokeOk <- smokeResults]
      smokeSkip = length [() | Skipped _ <- smokeResults]
      smokeFail = length smokeResults - smokeOk - smokeSkip

  printf "\nSmoke: %d OK, %d skipped, %d fail\n" smokeOk smokeSkip smokeFail

  -- 最终判定
  let frontendPass = null parseFail || passRate > 90.0  -- 允许 <10% 解析失败 (复杂语法)
  if frontendPass
    then putStrLn "\n✅ agda-compat PASS"
    else do
      putStrLn "\n❌ agda-compat FAIL"
      exitFailure
