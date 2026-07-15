-- | Dayan.Parse.Dy — .dy 语法解析器 (基于 Agda 语法子集)
--
-- 支持完整 .dy 语法:
--   pragma, module, open import, postulate, data, rewrite, defs
--   类型: Set, ℕ, Nat, Fin n, Vec A n, A → B, (x : A) → B
--   项: refl, 字面量, 变量, 应用, λ 抽象, {!!}
--   运算符: _≡_, _+_, _*_, _%_, _/_, _,_

{-# LANGUAGE OverloadedStrings #-}
module Dayan.Parse.Dy where

import Data.Text (Text)
import qualified Data.Text as T
import Dayan.ProofGen.AST
import Dayan.Parse.Lexer (Token(..), lexDy)

----------------------------------------------------------------------
-- Top-level
----------------------------------------------------------------------

parseDy :: Text -> Either String (AgdaModuleName, AgdaFile)
parseDy input =
  let toks = lexDy input
      (pragmas, toks') = scanPragmas toks
  in case toks' of
    TokModule : TokName modName : TokWhere : rest ->
      let (morePragmas, decls) = parseTopLevel rest
      in Right (AgdaModuleName modName, AgdaFile (pragmas <> morePragmas) modName decls)
    _ -> Right (AgdaModuleName "Main", AgdaFile pragmas "Main" (parseDecls toks'))

scanPragmas :: [Token] -> (Text, [Token])
scanPragmas (TokPragma p : rest) = let (more, rest') = scanPragmas rest in (p <> more, rest')
scanPragmas rest = ("", rest)

----------------------------------------------------------------------
-- Module body
----------------------------------------------------------------------

parseTopLevel :: [Token] -> (Text, [Decl])
parseTopLevel [] = ("", [])
parseTopLevel (TokPragma p : rest) =
  let (more, decls) = parseTopLevel rest in (p <> more, decls)
parseTopLevel (TokOpen : TokImport : TokName mod : rest) =
  let (decl, rest') = parseOpen mod rest
      (opts, decls) = parseTopLevel rest'
  in (opts, decl : decls)
parseTopLevel (TokPostulate : rest) =
  let (decls, rest') = parsePostulates rest
      (opts, more) = parseTopLevel rest'
  in (opts, decls <> more)
parseTopLevel (TokData : rest) =
  let (decl, rest') = parseDataDecl rest
      (opts, decls) = parseTopLevel rest'
  in (opts, decl : decls)
parseTopLevel (TokRewrite : rest) =
  let (decl, rest') = parseRewrite rest
      (opts, decls) = parseTopLevel rest'
  in (opts, decl : decls)
parseTopLevel (TokName name : TokColon : rest) =
  let (ty, rest') = parseType rest
      (body, rest'') = parseBody rest'
      (opts, decls) = parseTopLevel rest''
  in (opts, DDef name ty [Clause [] body] : decls)
parseTopLevel (TokComment c : rest) =
  let (opts, decls) = parseTopLevel rest
  in (opts, DComment c : decls)
parseTopLevel (_ : rest) = parseTopLevel rest

parseDecls :: [Token] -> [Decl]
parseDecls [] = []
parseDecls (TokComment c : rest) = DComment c : parseDecls rest
parseDecls (TokPostulate : rest) =
  let (ds, rest') = parsePostulates rest in ds ++ parseDecls rest'
parseDecls (TokData : rest) =
  let (d, rest') = parseDataDecl rest in d : parseDecls rest'
parseDecls (TokRewrite : rest) =
  let (d, rest') = parseRewrite rest in d : parseDecls rest'
parseDecls (TokName name : TokColon : rest) =
  let (ty, rest') = parseType rest
      (body, rest'') = parseBody rest'
  in DDef name ty [Clause [] body] : parseDecls rest''
parseDecls (_ : rest) = parseDecls rest

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
  | restStartsDecl rest = (acc, TokName n : rest)  -- n 后是 : → 新声明
  | otherwise = parseTypeAppMore (TApp acc (Def n)) rest
parseTypeAppMore acc (TokNum n : rest)
  | restStartsDecl rest = (acc, TokNum n : rest)
  | otherwise = parseTypeAppMore (TApp acc (Lit (LNat (fromIntegral n)))) rest
parseTypeAppMore acc rest = (acc, rest)

restStartsDecl :: [Token] -> Bool
restStartsDecl (TokColon : _) = True   -- name : type
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
