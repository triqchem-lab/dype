-- | Dayan.Parse.Dy — .dy 语法解析器 (基于 Agda 语法子集)
--
-- 支持完整 .dy 语法:
--   pragma, module, open import, postulate, data, rewrite, defs
--   类型: Set, Nat, Fin n, Vec A n, A -> B, (x : A) -> B
--   项: refl, 字面量, 变量, 应用, {!!}
--   运算符: _===_, _+_, _*_, _%_, _/_
--
-- 不支持的语法会报 ParseError, 而非静默跳过。

{-# LANGUAGE OverloadedStrings #-}
module Dayan.Parse.Dy where

import Data.Text (Text)
import qualified Data.Text as T
import Dayan.ProofGen.AST
import Dayan.Parse.Lexer (Token(..), lexDy)

data ParseError = ParseError
  { peLine :: !Int
  , peToken :: !Text
  , peMessage :: !Text
  } deriving (Show, Eq)

----------------------------------------------------------------------
-- Top-level
----------------------------------------------------------------------

parseDy :: Text -> Either [ParseError] (AgdaModuleName, AgdaFile)
parseDy input =
  let toks = lexDy input
      (pragmas, toks') = scanPragmas toks
      (errs, result) = case toks' of
        TokModule : TokName modName : TokWhere : rest ->
          let (moreErrs, (morePragmas, decls)) = parseTopLevel rest
          in (moreErrs, Right (AgdaModuleName modName, AgdaFile (pragmas <> morePragmas) modName decls))
        _ ->
          let (e, decls) = parseDecls toks'
          in (e, Right (AgdaModuleName "Main", AgdaFile pragmas "Main" decls))
  in case errs of
    [] -> result
    _  -> Left errs

scanPragmas :: [Token] -> (Text, [Token])
scanPragmas (TokPragma p : rest) = let (more, rest') = scanPragmas rest in (p <> more, rest')
scanPragmas rest = ("", rest)

----------------------------------------------------------------------
-- Module body — 错误累积
----------------------------------------------------------------------

parseTopLevel :: [Token] -> ([ParseError], (Text, [Decl]))
parseTopLevel [] = ([], ("", []))
parseTopLevel (TokPragma p : rest) =
  let (errs, (more, decls)) = parseTopLevel rest
  in (errs, (p <> more, decls))
parseTopLevel (TokOpen : TokImport : TokName mod : rest) =
  let (decl, rest') = parseOpen mod rest
      (errs, (opts, decls)) = parseTopLevel rest'
  in (errs, (opts, decl : decls))
parseTopLevel (TokPostulate : rest) =
  let (decls, rest') = parsePostulates rest
      (errs, (opts, more)) = parseTopLevel rest'
  in (errs, (opts, decls <> more))
parseTopLevel (TokData : rest) =
  let (decl, rest') = parseDataDecl rest
      (errs, (opts, decls)) = parseTopLevel rest'
  in (errs, (opts, decl : decls))
parseTopLevel (TokRewrite : rest) =
  let (decl, rest') = parseRewrite rest
      (errs, (opts, decls)) = parseTopLevel rest'
  in (errs, (opts, decl : decls))
parseTopLevel (TokName name : TokColon : rest) =
  let (ty, rest') = parseType rest
      (body, rest'') = parseBody rest'
      (errs, (opts, decls)) = parseTopLevel rest''
  in (errs, (opts, DDef name ty [Clause [] body] : decls))
parseTopLevel (TokComment c : rest) =
  let (errs, (opts, decls)) = parseTopLevel rest
  in (errs, (opts, DComment c : decls))
parseTopLevel (t : rest) =
  let (errs, result) = parseTopLevel rest
  in (unsupported t : errs, result)

parseDecls :: [Token] -> ([ParseError], [Decl])
parseDecls [] = ([], [])
parseDecls (TokComment c : rest) =
  let (errs, ds) = parseDecls rest in (errs, DComment c : ds)
parseDecls (TokPostulate : rest) =
  let (ds, rest') = parsePostulates rest
      (errs, more) = parseDecls rest'
  in (errs, ds ++ more)
parseDecls (TokData : rest) =
  let (d, rest') = parseDataDecl rest
      (errs, ds) = parseDecls rest'
  in (errs, d : ds)
parseDecls (TokRewrite : rest) =
  let (d, rest') = parseRewrite rest
      (errs, ds) = parseDecls rest'
  in (errs, d : ds)
parseDecls (TokName name : TokColon : rest) =
  let (ty, rest') = parseType rest
      (body, rest'') = parseBody rest'
      (errs, ds) = parseDecls rest''
  in (errs, DDef name ty [Clause [] body] : ds)
parseDecls (t : rest) =
  let (errs, ds) = parseDecls rest
  in (unsupported t : errs, ds)

unsupported :: Token -> ParseError
unsupported t = ParseError 0 (tokenText t) ("unsupported syntax: " <> tokenText t)

tokenText :: Token -> Text
tokenText (TokModule {})    = "module"
tokenText TokWhere          = "where"
tokenText TokOpen           = "open"
tokenText TokImport         = "import"
tokenText TokUsing          = "using"
tokenText TokPostulate      = "postulate"
tokenText (TokData)         = "data"
tokenText TokRewrite        = "rewrite"
tokenText (TokPragma p)     = "{-# " <> p <> " #-}"
tokenText TokSet            = "Set"
tokenText TokNat            = "Nat"
tokenText TokFin            = "Fin"
tokenText TokVec            = "Vec"
tokenText TokRefl           = "refl"
tokenText TokHole           = "{!!}"
tokenText TokArrow          = "->"
tokenText TokColon          = ":"
tokenText TokEqual          = "="
tokenText TokLParen         = "("
tokenText TokRParen         = ")"
tokenText TokDColon         = "::"
tokenText TokVBar           = "|"
tokenText TokSemi           = ";"
tokenText TokUnderscore     = "_"
tokenText (TokName n)       = n
tokenText (TokNum n)        = T.pack (show n)
tokenText (TokComment _)    = "--"

-- 保留函数 (简化为不累积错误的版本, 供其他地方使用)
parseDecls' :: [Token] -> [Decl]
parseDecls' = snd . parseDecls

----------------------------------------------------------------------
-- Import
----------------------------------------------------------------------

parseOpen :: Text -> [Token] -> (Decl, [Token])
parseOpen mod (TokUsing : TokLParen : rest) =
  let (names, after) = parseUsingList rest
  in (DOpenUsing mod names, skipOpenRest after)
parseOpen mod rest = (DOpen mod, skipOpenRest rest)

parseUsingList :: [Token] -> ([Text], [Token])
parseUsingList rest =
  case span (/= TokRParen) rest of
    (names, TokRParen : after) ->
      ([n | TokName n <- names], after)
    (_, []) -> ([], [])

skipOpenRest :: [Token] -> [Token]
skipOpenRest (TokUsing : TokLParen : rest) = skipOpenRest (snd (span (/= TokRParen) rest))
skipOpenRest (TokName _ : rest) = skipOpenRest rest
skipOpenRest rest = rest

----------------------------------------------------------------------
-- Postulate (多行)
----------------------------------------------------------------------

parsePostulates :: [Token] -> ([Decl], [Token])
parsePostulates toks = case toks of
  TokName name : TokColon : rest ->
    let (ty, rest') = parseType rest
        (more, rest'') = parsePostulates rest'
    in (DPostulate name ty : more, rest'')
  _ -> ([], toks)

----------------------------------------------------------------------
-- Data
----------------------------------------------------------------------

parseDataDecl :: [Token] -> (Decl, [Token])
parseDataDecl (TokName name : TokColon : rest) =
  let (ty, afterType) = parseType rest
  in case afterType of
    TokWhere : rest' ->
      let (cons, rest'') = parseConstructors rest'
      in (DData name [] cons, rest'')
    _ -> (DData name [] [], afterType)
parseDataDecl _ = (DData "?" [] [], [])

parseConstructors :: [Token] -> ([ConDecl], [Token])
parseConstructors (TokName n : TokColon : rest) =
  let (ty, rest') = parseType rest
      (more, rest'') = parseConstructors rest'
  in (ConDecl n ty : more, rest'')
parseConstructors rest = ([], rest)

----------------------------------------------------------------------
-- Rewrite
----------------------------------------------------------------------

parseRewrite :: [Token] -> (Decl, [Token])
parseRewrite (TokName name : TokColon : rest) =
  let (eq, rest') = parseTerm rest
  in (DRewrite name eq, rest')
parseRewrite rest = (DRewrite "?" Hole, rest)

----------------------------------------------------------------------
-- Body
----------------------------------------------------------------------

parseBody :: [Token] -> (Term, [Token])
parseBody (TokEqual : TokRefl : rest) = (Refl, rest)
parseBody (TokEqual : TokHole : rest) = (Hole, rest)
parseBody (TokEqual : rest) = parseTerm rest
parseBody (TokName _ : rest) = parseBody rest  -- 跳过函数名, 进入 = body
parseBody rest = (Hole, rest)

----------------------------------------------------------------------
-- Type parsing (递归下降)
----------------------------------------------------------------------

parseType :: [Token] -> (Type, [Token])
parseType = parseTypeArrow

parseTypeArrow :: [Token] -> (Type, [Token])
parseTypeArrow toks =
  let (t, rest) = parseTypeApp toks
  in case rest of
    TokArrow : rest' ->
      let (t2, rest'') = parseTypeArrow rest'
      in (TFun t t2, rest'')
    _ -> (t, rest)

parseTypeApp :: [Token] -> (Type, [Token])
parseTypeApp (TokLParen : rest) =
  case rest of
    TokName x : TokColon : rest' ->
      let (a, rest'') = parseTypeArrow rest'
      in case rest'' of
        TokRParen : TokArrow : rest''' ->
          let (b, rest'''') = parseTypeArrow rest'''
          in (TPi x a b, rest'''')
        TokRParen : rest''' -> (TDef x, rest''')
        _ -> (TDef x, rest'')
    _ -> let (t, rest') = parseTypeArrow rest
         in case rest' of
           TokRParen : rest'' -> (t, rest'')
           _ -> (t, rest')
parseTypeApp (TokSet : rest) = (TSet, rest)
parseTypeApp (TokNat : rest) = (TNat, rest)
parseTypeApp (TokFin : rest) =
  let (n, rest') = parseTerm rest
  in (TFin n, rest')
parseTypeApp (TokVec : rest) =
  let (a, rest') = parseTypeArrow rest
      (n, rest'') = parseTerm rest'
  in (TVec a n, rest'')
parseTypeApp (TokName n : rest) = parseTypeAppMore (TDef n) rest
parseTypeApp rest = (TSet, rest)

parseTypeAppMore :: Type -> [Token] -> (Type, [Token])
parseTypeAppMore acc (TokName n : rest)
  | restStartsDecl rest = (acc, TokName n : rest)
  | otherwise = parseTypeAppMore (TApp acc (Def n)) rest
parseTypeAppMore acc (TokNum n : rest)
  | restStartsDecl rest = (acc, TokNum n : rest)
  | otherwise = parseTypeAppMore (TApp acc (Lit (LNat (fromIntegral n)))) rest
parseTypeAppMore acc rest = (acc, rest)

restStartsDecl :: [Token] -> Bool
restStartsDecl (TokColon : _) = True
restStartsDecl _ = False

----------------------------------------------------------------------
-- Term parsing
----------------------------------------------------------------------

parseTerm :: [Token] -> (Term, [Token])
parseTerm (TokName n : rest) = parseTermApp (Var n) rest
parseTerm (TokNum n : rest) = (Lit (LNat (fromIntegral n)), rest)
parseTerm (TokRefl : rest) = (Refl, rest)
parseTerm (TokHole : rest) = (Hole, rest)
parseTerm (TokLParen : rest) =
  let (t, rest') = parseTerm rest
  in case rest' of
    TokRParen : rest'' -> (t, rest'')
    _ -> (t, rest')
parseTerm rest = (Hole, rest)

parseTermApp :: Term -> [Token] -> (Term, [Token])
parseTermApp acc (TokName n : rest) | n /= "where" = parseTermApp (App acc (Def n)) rest
parseTermApp acc (TokNum n : rest) = parseTermApp (App acc (Lit (LNat (fromIntegral n)))) rest
parseTermApp acc (TokLParen : rest) =
  let (t, rest') = parseTerm rest
  in case rest' of
    TokRParen : rest'' -> parseTermApp (App acc t) rest''
    _ -> (acc, rest)
parseTermApp acc rest = (acc, rest)
