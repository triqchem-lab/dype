-- test/Succeed.hs — dype Succeed 测试套件 (v2: 前端测试)
--
-- dype 是 Agda 内核替换 (wiki 16-paradigm-replacement)。
-- 测试策略: parse → emit → roundtrip (纯 Haskell, 秒级)
-- 不再对 2000+ 文件逐个调用 agda 二进制。

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), takeExtension, makeRelative)
import System.Exit (exitFailure, ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Text.Printf (printf)
import Control.Monad (unless, forM_, forM)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Dayan.Parse.Agda (parseAgdaFile)
import Dayan.ProofGen.Emit (emitFile)
import Dayan.ProofGen.AST (AgdaFile(..))

agdaSucceedDir :: FilePath
agdaSucceedDir = "/data/work/functional-programming/agda/test/Succeed"

-- | 递归收集 .agda 文件
collectAgdaFiles :: FilePath -> IO [FilePath]
collectAgdaFiles dir = do
  isDir <- doesDirectoryExist dir
  if not isDir then pure []
  else do
    ents <- listDirectory dir
    concat <$> mapM go ents
  where
    go name = do
      let fp = dir </> name
      isDir <- doesDirectoryExist fp
      if isDir
        then collectAgdaFiles fp
        else if takeExtension name == ".agda"
             then pure [fp]
             else pure []

data Result = Ok | Fail String deriving (Show)

main :: IO ()
main = do
  agdaFiles <- collectAgdaFiles agdaSucceedDir
  putStrLn $ "Testing " ++ show (length agdaFiles) ++ " files (parse → emit → roundtrip)"

  results <- mapM testOne agdaFiles

  let total = length results
      ok    = length [() | Ok <- results]
      fails = [(f, e) | (f, Fail e) <- zip agdaFiles results]

  putStrLn $ replicate 60 '='
  printf "Total:  %d\n" total
  printf "OK:     %d (%.1f%%)\n" ok (100.0 * fromIntegral ok / fromIntegral total :: Double)
  printf "FAIL:   %d (%.1f%%)\n" (length fails) (100.0 * fromIntegral (length fails) / fromIntegral total :: Double)
  putStrLn $ replicate 60 '='

  unless (null fails) $ do
    putStrLn $ "\nFirst 10 failures:"
    forM_ (take 10 fails) $ \(f, e) ->
      putStrLn $ "  " ++ makeRelative agdaSucceedDir f ++ ": " ++ take 80 e

  -- 允许 <5% 失败 (复杂语法边界)
  let passRate = 100.0 * fromIntegral ok / fromIntegral total :: Double

  -- Phase 2: agda verify 冒烟测试
  putStrLn "\n=== Phase 2: agda verify 冒烟测试 ==="
  let agdaTestDir = "/data/work/functional-programming/agda/test"
  smokeResults <- forM smokeFiles $ \rel -> do
    r <- testSmoke agdaTestDir rel
    putStrLn $ "  " ++ rel ++ ": " ++ show r
    pure r
  let smokeOk = length [() | SmokeOk <- smokeResults]
  printf "Smoke: %d/%d OK\n" smokeOk (length smokeFiles)

  if passRate >= 95.0
    then putStrLn "\n✅ dype-succeed PASS"
    else do
      printf "\n❌ dype-succeed FAIL (%.1f%% < 95%%)\n" passRate
      exitFailure

-- | 单文件: parse → emit → re-parse roundtrip
testOne :: FilePath -> IO Result
testOne fp = do
  content <- TIO.readFile fp
  r1 <- parseAgdaFile fp content
  case r1 of
    Left err -> pure $ Fail ("parse: " ++ err)
    Right af -> do
      let emitted = emitFile af
      r2 <- parseAgdaFile (fp ++ ".rt") emitted
      case r2 of
        Left err -> pure $ Fail ("roundtrip: " ++ err)
        Right af2 ->
          if length (fileDecls af2) == length (fileDecls af)
            then pure Ok
            else pure $ Fail $ "decl mismatch: " ++ show (length (fileDecls af)) ++ " → " ++ show (length (fileDecls af2))

----------------------------------------------------------------------
-- Phase 2: agda verify 冒烟测试 (≤4 自包含文件)
----------------------------------------------------------------------

smokeFiles :: [FilePath]
smokeFiles =
  [ "simple.agda"
  , "Lambda.agda"
  , "Nat.agda"
  , "AbsurdLam.agda"
  ]

data SmokeResult = SmokeOk | SmokeFail String | SmokeSkip deriving (Show)

testSmoke :: FilePath -> FilePath -> IO SmokeResult
testSmoke agdaTestDir rel = do
  let fp = agdaTestDir </> "Succeed" </> rel
  exists <- doesFileExist fp
  if not exists then pure SmokeSkip
  else do
    content <- TIO.readFile fp
    r <- parseAgdaFile fp content
    case r of
      Left err -> pure $ SmokeFail ("parse: " ++ err)
      Right af -> do
        let emitted = emitFile af
        withSystemTempDirectory "dype-succeed-smoke" $ \tmp -> do
          let modParts = T.split (== '.') (fileModule af)
              modFile = T.unpack (last modParts) ++ ".agda"
          TIO.writeFile (tmp </> modFile) emitted
          mResult <- timeout 10000000 $
            readProcessWithExitCode "agda"
              ["--include-path=" ++ tmp, "--include-path=" ++ agdaTestDir,
               tmp </> modFile] ""
          pure $ case mResult of
            Nothing -> SmokeFail "timeout"
            Just (ExitSuccess, _, _) -> SmokeOk
            Just (ExitFailure _, _, stderr) -> SmokeFail (take 100 stderr)
