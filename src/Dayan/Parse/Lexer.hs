-- | Dayan.Parse.Lexer — .dy 字符级词法分析器
--
-- 替代 T.words 的简单分行, 处理:
--   (_≡_; refl) → TokLParen, TokName "_≡_", TokSemi, TokName "refl", TokRParen
--   {-# OPTIONS --rewriting #-} → TokPragma
--   -- comment → TokComment
--   → → TokArrow

{-# LANGUAGE OverloadedStrings #-}
module Dayan.Parse.Lexer where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlpha, isDigit, isSpace, isSymbol)

data Token
  = TokModule | TokWhere | TokOpen | TokImport | TokUsing | TokPostulate
  | TokData | TokRewrite | TokPragma Text
  | TokSet | TokNat | TokFin | TokVec | TokRefl | TokHole
  | TokArrow | TokColon | TokEqual | TokLParen | TokRParen
  | TokDColon | TokVBar | TokSemi | TokUnderscore
  | TokName Text | TokNum Integer
  | TokComment Text
  deriving (Show, Eq)

lexDy :: Text -> [Token]
lexDy = topLevel . T.unpack
  where
    topLevel [] = []
    topLevel ('{':'-':'#':cs) =
      let (p, rest) = scanPragma cs
      in TokPragma (T.pack ("{-#" <> p <> "#-}")) : topLevel rest
    topLevel ('-':'-':cs) = let (c, rest) = scanLine cs in TokComment (T.pack c) : topLevel rest
    topLevel cs = case scanToken cs of
      (t, rest) | isEmpty t -> topLevel rest
      (t, rest) -> t : topLevel rest

    isEmpty (TokName "") = True
    isEmpty (TokName " ") = True
    isEmpty _ = False

    scanToken [] = (TokName "", [])
    scanToken ('(':'_':cs) = (TokLParen, '_':cs)  -- 留 _ 给下次递归
    scanToken ('(' : cs) = (TokLParen, cs)
    scanToken (')' : cs) = (TokRParen, cs)
    scanToken (':' : ':' : cs) = (TokDColon, cs)
    scanToken (':' : cs) = (TokColon, cs)
    scanToken ('=' : cs) = (TokEqual, cs)
    scanToken ('→' : cs) = (TokArrow, cs)
    scanToken ('|' : cs) = (TokVBar, cs)
    scanToken (';' : cs) = (TokSemi, cs)
    scanToken ('{' : '!' : '!' : '}' : cs) = (TokHole, cs)  -- {!!}
    scanToken ('_' : cs)
      | null name = (TokUnderscore, rest)
      | otherwise = (TokName (T.pack ('_':name)), rest)
      where (name, rest) = scanName cs
    scanToken (c : cs)
      | isSpace c  = (TokName " ", cs)  -- dummy, will be filtered by topLevel
      | isAlpha c || c == '_' = let (name, rest) = scanName (c:cs)
          in (keyword (T.pack name), rest)
      | isDigit c = let (n, rest) = scanNum (c:cs)
          in (TokNum n, rest)
      | otherwise = scanToken cs

-- | 扫描 pragma: {-＃ ... ＃-}
scanPragma :: String -> (String, String)
scanPragma = go []
  where go acc ('#':'-':'}':cs) = (reverse acc, cs)
        go acc (c:cs) = go (c:acc) cs
        go acc [] = (reverse acc, [])

-- | 扫描行注释: -- ... \n
scanLine :: String -> (String, String)
scanLine = go []
  where go acc ('\n':cs) = (reverse acc, cs)
        go acc (c:cs) = go (c:acc) cs
        go acc [] = (reverse acc, [])

-- | 扫描标识符: 字母/数字/下划线/'
scanName :: String -> (String, String)
scanName = go []
  where go acc (c:cs) | isNameChar c = go (c:acc) cs
        go acc cs = (reverse acc, cs)

isNameChar :: Char -> Bool
isNameChar c = isAlpha c || isDigit c || elem c ['_', '\'', '.', '-', '?', '!'] || isSymbol c

-- | 扫描数字
scanNum :: String -> (Integer, String)
scanNum = go 0
  where go acc (c:cs) | isDigit c = go (acc * 10 + fromIntegral (fromEnum c - fromEnum '0')) cs
        go acc cs = (acc, cs)

-- | 扫描字符串 (引号内)
scanString :: String -> (String, String)
scanString = go []
  where go acc ('"':cs) = (reverse acc, cs)
        go acc (c:cs) = go (c:acc) cs
        go acc [] = (reverse acc, [])

-- | 关键词匹配
keyword :: Text -> Token
keyword "module"    = TokModule
keyword "where"     = TokWhere
keyword "open"      = TokOpen
keyword "import"    = TokImport
keyword "using"     = TokUsing
keyword "postulate" = TokPostulate
keyword "data"      = TokData
keyword "rewrite"   = TokRewrite
keyword "Set"       = TokSet
keyword "ℕ"         = TokNat
keyword "Nat"       = TokNat
keyword "Fin"       = TokFin
keyword "Vec"       = TokVec
keyword "refl"      = TokRefl
keyword name         = TokName name
