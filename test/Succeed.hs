-- test/Succeed.hs — dype Succeed 测试套件
--
-- 对 Agda test/Succeed/ 中的 .agda 文件运行 dype 完整管线。
-- 策略: 将所有文件复制到临时目录，用 dype emit 的输出覆盖目标文件，
--       再在临时目录内运行 agda。避免与原始文件冲突。

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Directory (listDirectory, copyFile, createDirectoryIfMissing, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, takeFileName, takeDirectory)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import Text.Printf (printf)
import Control.Monad (unless, forM_)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Dayan.Parse.Agda (parseAgdaFile)
import Dayan.ProofGen.Emit (emitFile)
import Dayan.ProofGen.AST (AgdaFile(..))

agdaTestDir, agdaSucceedDir :: FilePath
agdaTestDir    = "/data/work/functional-programming/agda/test"
agdaSucceedDir = "/data/work/functional-programming/agda/test/Succeed"

main :: IO ()
main = do
  -- 递归收集所有 .agda 文件 (含子目录)
  agdaFiles <- collectAgdaFiles agdaSucceedDir
  putStrLn $ "Testing " ++ show (length agdaFiles) ++ " files"

  results <- withSystemTempDirectory "dype-succeed" $ \tmp -> do
    -- 递归复制 Agda succeed 目录树到临时目录
    putStrLn "Copying test files..."
    copyDirRecursive agdaSucceedDir tmp
    -- 逐个: parse → emit → 覆盖 → agda verify
    mapM (testOne tmp agdaSucceedDir) (map (makeRelative agdaSucceedDir) agdaFiles)

  let total = length results
      ok    = length [() | Right () <- results]
      fails = [(f, e) | (f, Left e) <- zip (map (makeRelative agdaSucceedDir) agdaFiles) results]

  putStrLn $ replicate 60 '='
  printf "Total:  %d\n" total
  printf "OK:     %d (%.1f%%)\n" ok (100.0 * fromIntegral ok / fromIntegral total :: Double)
  printf "FAIL:   %d (%.1f%%)\n" (length fails) (100.0 * fromIntegral (length fails) / fromIntegral total :: Double)
  putStrLn $ replicate 60 '='

  unless (null fails) $ do
    putStrLn "\n=== FAILURES ==="
    forM_ (take 20 fails) $ \(f, e) -> putStrLn $ "\n--- " ++ f ++ " ---\n" ++ take 400 e
    if length fails > 20 then putStrLn $ "... and " ++ show (length fails - 20) ++ " more" else pure ()
    putStrLn $ "\n" ++ show (length fails) ++ " failures total."

  if null fails then putStrLn "\nAll tests passed." else error $ show (length fails) ++ " test(s) failed"

-- | 递归收集目录中所有 .agda 文件
collectAgdaFiles :: FilePath -> IO [FilePath]
collectAgdaFiles dir = go dir
  where
    go d = do
      ents <- listDirectory d
      concat <$> mapM (\e -> do
        let p = d </> e
        isDir <- doesDirectoryExist p
        if isDir then go p
        else pure [p | takeExtension p == ".agda"]
        ) ents

-- | 递归复制目录树
copyDirRecursive :: FilePath -> FilePath -> IO ()
copyDirRecursive src dst = do
  createDirectoryIfMissing True dst
  ents <- listDirectory src
  forM_ ents $ \e -> do
    let sp = src </> e
        dp = dst </> e
    isDir <- doesDirectoryExist sp
    if isDir then copyDirRecursive sp dp
    else copyFile sp dp

-- | 相对路径
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative base fp = if takeDirectory fp == base then takeFileName fp
                       else makeRelative base (takeDirectory fp) </> takeFileName fp

testOne :: FilePath -> FilePath -> FilePath -> IO (Either String ())
testOne tmpDir baseDir relPath = do
  let originalPath = baseDir </> relPath
      targetPath   = tmpDir </> relPath
  content <- TIO.readFile originalPath
  result <- parseAgdaFile originalPath content
  case result of
    Left err -> pure $ Left ("[PARSE] " ++ take 300 err)
    Right agdaFile -> do
      let src = emitFile agdaFile
          modName = T.unpack (fileModule agdaFile)
          -- 点号 → 路径分隔符 (DeadCodePatSyn.Lib → DeadCodePatSyn/Lib.agda)
          targetRel = T.unpack (T.replace "." "/" (fileModule agdaFile)) <> ".agda"
          target = tmpDir </> targetRel
      createDirectoryIfMissing True (takeDirectory target)
      TIO.writeFile target src
      (exit, stdout, _stderr) <- readProcessWithExitCode "agda"
        ["--include-path=" <> tmpDir, "--include-path=" <> agdaTestDir, target] ""
      case exit of
        ExitSuccess -> pure $ Right ()
        ExitFailure _ ->
          pure $ Left ("[AGDA] " ++ relPath ++ "\n" ++ take 400 stdout)
