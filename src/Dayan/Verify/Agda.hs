{-# LANGUAGE OverloadedStrings #-}
module Dayan.Verify.Agda where
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dayan.ProofGen.AST (AgdaFile(..))
import Dayan.ProofGen.Emit (emitFile)

data VerifyResult = VerifyOK | VerifyError Text deriving (Show, Eq)

verify :: AgdaFile -> IO VerifyResult
verify af = withSystemTempDirectory "dayan" $ \tmp -> do
  let parts = T.splitOn "." (fileModule af)
      subDir = T.unpack (T.intercalate "/" (init parts))
      baseName = T.unpack (last parts) ++ ".agda"
      fullDir = tmp ++ "/" ++ subDir
      relPath = T.unpack (T.intercalate "/" parts) ++ ".agda"
  _ <- readProcessWithExitCode "mkdir" ["-p", fullDir] ""
  TIO.writeFile (fullDir ++ "/" ++ baseName) (emitFile af)
  (exit, out, _) <- readProcessWithExitCode "bash" ["-c", "cd " ++ tmp ++ " && agda " ++ relPath] ""
  return $ case exit of ExitSuccess -> VerifyOK; _ -> VerifyError (T.pack (take 200 out))
