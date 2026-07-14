-- | Dayan.Parse.Dy — .dy 文件解析器
--
-- 解析大衍引擎的 .dy 源文件格式:
--   - module 声明
--   - postulate 声明  
--   - 定义: name : type; name clauses = body
--   - 注释: -- ...
--
-- 输出: Dayan.ProofGen.AST 的 AgdaFile
--
-- 这是 Phase 4 的前端基石，独立于 vendor/agda-src

{-# LANGUAGE OverloadedStrings #-}
module Dayan.Parse.Dy where

import Data.Text (Text)
import qualified Data.Text as T
import Dayan.ProofGen.AST

----------------------------------------------------------------------
-- 1. 解析入口
----------------------------------------------------------------------

-- | 解析 .dy 文件内容为 AgdaFile
parseDy :: Text -> Either String AgdaFile
parseDy input =
  case T.lines input of
    []  -> Left "empty file"
    ls  -> case parseOpts (head ls) of
             Just opts -> Right $ AgdaFile opts "TODO" (parseDecls (tail ls))
             Nothing   -> Right $ AgdaFile "" "TODO" (parseDecls ls)

-- | 解析 OPTIONS 行
parseOpts :: Text -> Maybe Text
parseOpts line
  | "{-#" `T.isPrefixOf` line && "#-}" `T.isSuffixOf` line
  = Just line
  | otherwise = Nothing

----------------------------------------------------------------------
-- 2. 声明解析
----------------------------------------------------------------------

parseDecls :: [Text] -> [Decl]
parseDecls [] = []
parseDecls (l:ls)
  | "--" `T.isPrefixOf` l = DComment (T.drop 2 l) : parseDecls ls
  | "postulate" `T.isPrefixOf` l = parsePostulate l : parseDecls ls
  | "module" `T.isPrefixOf` l = parseDecls ls  -- skip for now
  | "where" `T.isPrefixOf` l = parseDecls ls
  | T.null l = parseDecls ls
  | ":" `T.isInfixOf` l = parseDef l ls
  | otherwise = parseDecls ls

-- | 解析 postulate
parsePostulate :: Text -> Decl
parsePostulate line =
  let parts = T.words line
      name = if length parts > 1 then parts !! 1 else "?"
  in DPostulate name TNat

-- | 解析定义: name : type\n name pats = body
parseDef :: Text -> [Text] -> [Decl]
parseDef typeLine rest =
  let (name, _tyText) = splitColon typeLine; ty = TDef "Set"
  in case rest of
       [] -> [DDef name ty [Clause [] Refl]]
       (bodyLine:more) ->
         let body = if "= refl" `T.isSuffixOf` bodyLine then Refl
                    else Hole
         in DDef name ty [Clause [] body] : parseDecls more

----------------------------------------------------------------------
-- 3. 辅助
----------------------------------------------------------------------

splitColon :: Text -> (Text, Text)
splitColon t =
  case T.breakOn ":" t of
    (name, rest) -> (T.strip name, T.strip (T.drop 1 rest))
