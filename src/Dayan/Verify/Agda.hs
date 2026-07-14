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
  let agdaPath = tmp ++ "/" ++ T.unpack (fileModule af) ++ ".agda"
  TIO.writeFile agdaPath (emitFile af)
  (exit, _, err) <- readProcessWithExitCode "agda" [agdaPath] ""
  return $ case exit of
    ExitSuccess   -> VerifyOK
    ExitFailure _ -> VerifyError (T.pack (take 300 err))
