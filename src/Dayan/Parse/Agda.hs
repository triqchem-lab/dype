-- | Dayan.Parse.Agda — Agda 语法层封装
--
-- 使用 Agda.Syntax.Parser 解析 .agda 文件 (100% 语法覆盖)。
-- 全量透传: DPassThrough 保存原始源文本。
-- 匿名模块 (module _ where) 用原始文件名回退。

{-# LANGUAGE OverloadedStrings #-}
module Dayan.Parse.Agda
  ( parseAgdaFile
  , AgdaCompatIssue(..)
  , classifyFailure
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeBaseName, replaceExtension)

import Dayan.ProofGen.AST

import Agda.Syntax.Parser (parseFile, moduleParser, runPMIO)
import Agda.Syntax.Concrete (Module(Mod), Pragma(OptionsPragma))
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Position (mkRangeFile)
import Agda.Utils.FileName (mkAbsolute)

-- | 剥离 UTF-8 BOM (U+FEFF)
stripBOM :: Text -> Text
stripBOM t = case T.uncons t of
  Just ('\xFEFF', rest) -> rest
  _ -> t

----------------------------------------------------------------------
-- 解析入口
----------------------------------------------------------------------

-- | 解析 .agda 文件文本，返回 dype AST。
-- 原始文件名用于匿名模块 (module _ where) 的文件名回退。
parseAgdaFile :: FilePath -> Text -> IO (Either String AgdaFile)
parseAgdaFile fp content = do
  let src = T.unpack (stripBOM content)
      absPath = mkAbsolute "/tmp/dype-input.agda"
      rfile = mkRangeFile absPath Nothing
  result <- runPMIO $ parseFile False moduleParser rfile src
  case fst result of
    Left err -> pure $ Left (show err)
    Right ((md, _attrs), _ft) ->
      pure $ Right $ moduleToAgdaFile fp content mod

----------------------------------------------------------------------
-- Module → AgdaFile (全量透传)
----------------------------------------------------------------------

-- | 将 Agda Module 映射为 dype AgdaFile。
-- 模块名优先用解析结果；匿名/不匹配时用原始文件名。
-- 文件路径保留原始相对路径 (如 DeadCodePatSyn/Lib.agda)。
moduleToAgdaFile :: FilePath -> Text -> Module -> AgdaFile
moduleToAgdaFile fp content (Mod modName pragmas _decls) =
  let parsedName = T.pack (prettyShow modName)
      baseName   = T.pack (takeBaseName (replaceExtension fp ".agda"))
      -- 点号分隔的模块名还原为子目录路径 (DeadCodePatSyn.Lib → DeadCodePatSyn/Lib.agda)
      moduleName = if parsedName == "_" || T.null parsedName
                   then baseName
                   else parsedName
      -- 保留 pragma 文本
      optsText = T.unlines [pragmaText p | p <- pragmas]
  in AgdaFile
    { fileOpts = optsText
    , fileModule = moduleName
    , fileDecls = [DPassThrough content]
    }

pragmaText :: Pragma -> Text
pragmaText (OptionsPragma _ opts) = "{-# OPTIONS " <> T.pack (unwords opts) <> " #-}"
pragmaText _ = ""

----------------------------------------------------------------------
-- 失败分类
----------------------------------------------------------------------

data AgdaCompatIssue
  = TheoryDiff
  | ProofIssue
  | Engineering
  deriving (Show, Eq)

classifyFailure :: Text -> AgdaCompatIssue
classifyFailure err
  | any (`T.isInfixOf` err) engMarkers    = Engineering
  | any (`T.isInfixOf` err) theoryMarkers = TheoryDiff
  | any (`T.isInfixOf` err) proofMarkers  = ProofIssue
  | otherwise                             = Engineering
  where
    -- 工程标记优先: 这些是配置/环境问题, 不是理论差异
    engMarkers =
      [ "ExeNotTrusted", "SafeFlagPragma"
      , "LibTooFarDown", "LibraryError"
      , "trusted executables"
      ]
    theoryMarkers =
      [ "Set₁", "Set₂", "Setω"
      , "cubical", "Partial", "hcomp", "transp"
      , "coinductive", "∞"
      , "sized", "Size<"
      , "Prop", "Propℓ"
      ]
    proofMarkers =
      [ "termination", "Termination"
      , "positivity", "Positivity"
      , "not a constructor"
      , "Cannot resolve", "unsolved"
      , "Should be a function"
      , "No such module"
      ]
