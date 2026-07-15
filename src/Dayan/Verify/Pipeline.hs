-- src/Dayan/Verify/Pipeline.hs — 跨层验证管线
--   .dy → Parse → AgdaFile → Emit → .agda → agda verify → report

{-# LANGUAGE OverloadedStrings #-}
module Dayan.Verify.Pipeline where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import Dayan.ProofGen.AST (AgdaModuleName(..))
import Dayan.ProofGen.Emit (emitFile)
import Dayan.Parse.Dy (parseDy)
import Dayan.Adapter.Agda (writeAgdaFile)

data VerifyResult = VerifyOk | VerifyFail [Text]
  deriving (Show, Eq)

-- | 完整管线: .dy 文本 → agda 验证
--   Returns (module name, generated .agda, verification result)
runPipeline :: Text -> IO (Text, Text, VerifyResult)
runPipeline dySource = do
  case parseDy dySource of
    Left errs -> pure (T.pack (show errs), "", VerifyFail [T.pack (show errs)])
    Right (modName, agdaFile) -> do
      let agdaSrc = emitFile agdaFile
      result <- withSystemTempDirectory "dayan-verify" $ \tmp -> do
        let modParts = T.split (== '.') (agdaModuleText modName)
            modDir = T.unpack (T.intercalate "/" (init modParts))
            modFile = T.unpack (last modParts) <> ".agda"
            agdaDir = tmp </> modDir
        createDirectoryIfMissing True agdaDir
        let agdaPath = agdaDir </> modFile
        TIO.writeFile agdaPath agdaSrc
        (exit, stdout, stderr) <- readProcessWithExitCode "agda"
          ["--include-path=" <> tmp, agdaPath] ""
        let output = T.pack (stdout <> stderr)
        pure $ case exit of
          ExitSuccess -> VerifyOk
          ExitFailure _ -> VerifyFail [output]
      pure (agdaModuleText modName, agdaSrc, result)

-- | 统计报告
report :: Text -> Text -> VerifyResult -> Text
report modName _agdaSrc VerifyOk =
  "✅ " <> modName <> " — Agda verification PASSED"
report modName _agdaSrc (VerifyFail errs) =
  "❌ " <> modName <> " — Agda verification FAILED\n" <>
  T.intercalate "\n" (take 5 errs)
