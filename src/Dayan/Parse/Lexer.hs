-- | Dayan.Parse.Lexer — .dy 词法分析器
--
-- 简化版 Agda lexer, 支持 .dy 子集:
--   关键字: module, where, open, import, using, postulate, data, rewrite
--   标点:  ::, :, =, →, (, ), {!, !}, ∷, ;
--   字面量: 自然数
--   标识符: 字母开头的名字

{-# LANGUAGE OverloadedStrings #-}
module Dayan.Parse.Lexer where

import Data.Text (Text)
import qualified Data.Text as T

data Token
  = TokModule | TokWhere | TokOpen | TokImport | TokUsing | TokPostulate
  | TokData | TokRewrite | TokPragma Text
  | TokSet | TokNat | TokFin | TokVec | TokRefl | TokHole
  | TokArrow | TokColon | TokEqual | TokLParen | TokRParen
  | TokDColon | TokVBar | TokSemi
  | TokName Text | TokNum Integer
  | TokComment Text | TokWS
  deriving (Show, Eq)

lexDy :: Text -> [Token]
lexDy = filter (not . isWS) . go . T.words
  where
    isWS TokWS = True; isWS _ = False
    go [] = []
    go ("module":ws)       = TokModule : go ws
    go ("where":ws)        = TokWhere : go ws
    go ("open":ws)         = TokOpen : go ws
    go ("import":ws)       = TokImport : go ws
    go ("using":ws)        = TokUsing : go ws
    go ("postulate":ws)    = TokPostulate : go ws
    go ("data":ws)         = TokData : go ws
    go ("rewrite":ws)      = TokRewrite : go ws
    go ("Set":ws)          = TokSet : go ws
    go ("ℕ":ws)            = TokNat : go ws
    go ("Fin":ws)          = TokFin : go ws
    go ("Vec":ws)          = TokVec : go ws
    go ("refl":ws)         = TokRefl : go ws
    go ("{!!}":ws)         = TokHole : go ws
    go ("→":ws)            = TokArrow : go ws
    go (":":ws)            = TokColon : go ws
    go ("=":ws)            = TokEqual : go ws
    go ("(":ws)            = TokLParen : go ws
    go (")":ws)            = TokRParen : go ws
    go ("::":ws)           = TokDColon : go ws
    go ("|":ws)            = TokVBar : go ws
    go (";":ws)            = TokSemi : go ws
    go (w:ws)
      | "--" `T.isPrefixOf` w = TokComment (T.drop 2 w) : go ws
      | "{-#" `T.isPrefixOf` w = case parsePragma w ws of
          (p, rest) -> TokPragma p : go rest
      | T.all isDigitOrPrime w = TokNum (read (T.unpack w)) : go ws
      | otherwise = TokName w : go ws
    parsePragma w ws =
      if "#-}" `T.isSuffixOf` w
      then (w, ws)
      else case ws of
        (w2:w3) -> parsePragma (w <> " " <> w2) w3
        [] -> (w, [])

isDigitOrPrime :: Char -> Bool
isDigitOrPrime c = c `elem` ("0123456789'" :: String)
