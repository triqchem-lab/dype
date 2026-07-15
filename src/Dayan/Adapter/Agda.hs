-- | Dayan.Adapter.Agda — 大衍→Agda 编译管线
--
-- Clang/LLVM 模型: 大衍前端生成 Agda AST → Agda 后端验证
--   管线: .dy → Parse(Dy) → Emit(AgdaFile) → write .agda → agda verify

{-# LANGUAGE OverloadedStrings #-}
module Dayan.Adapter.Agda
  ( dyToAgda
  , dyToAgdaFile
  , writeAgdaFile
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dayan.ProofGen.AST (AgdaFile(..), AgdaModuleName(..))
import Dayan.ProofGen.Emit (emitFile)
import Dayan.Parse.Dy (parseDy)

-- | .dy 文本 → Agda AST (AgdaFile)
dyToAgda :: Text -> Either String AgdaFile
dyToAgda input = case parseDy input of
  Left errs -> Left (show errs)
  Right (_, f) -> Right f

-- | .dy 文件 → AgdaFile (文件路径版本)
dyToAgdaFile :: FilePath -> IO (Either String AgdaFile)
dyToAgdaFile fp = do
  txt <- TIO.readFile fp
  pure (dyToAgda txt)

-- | AgdaFile → 写出 .agda 文件
writeAgdaFile :: FilePath -> AgdaFile -> IO ()
writeAgdaFile fp f = TIO.writeFile fp (emitFile f)
