-- | Dayan.Parse.Dy — .dy 文件解析器
--
-- 语法子集:
--   module Name where
--   {-# OPTIONS --rewriting #-}
--   open import Module using (names)
--   postulate name : Type
--   name : Type
--   name = refl
--   data Name : Set where cons : Type

{-# LANGUAGE OverloadedStrings #-}
module Dayan.Parse.Dy where

import Data.Text (Text)
import qualified Data.Text as T
import Dayan.ProofGen.AST
import Dayan.Parse.Lexer (Token(..), lexDy)

parseDy :: Text -> Either String (AgdaModuleName, AgdaFile)
parseDy input =
  let toks = lexDy input
      (opts, toks') = consumePragmas toks
  in case toks' of
    TokModule : TokName modName : TokWhere : rest ->
      let (moreOpts, decls) = parseTopLevel rest
      in Right (AgdaModuleName modName, AgdaFile (opts <> moreOpts) modName decls)
    _ -> Right (AgdaModuleName "Main", AgdaFile opts "Main" (parseDecls toks'))

-- | 消耗前置 pragma 和空白, 返回 (opts, remaining tokens)
consumePragmas :: [Token] -> (Text, [Token])
consumePragmas (TokPragma p : rest) =
  let (more, rest') = consumePragmas rest
  in (p <> more, rest')
consumePragmas rest = ("", rest)

parseTopLevel :: [Token] -> (Text, [Decl])
parseTopLevel (TokPragma opts : rest) =
  let (moreOpts, decls) = parseTopLevel rest
  in (opts <> moreOpts, decls)
parseTopLevel (TokOpen : TokImport : TokName mod : rest) =
  let decl = parseOpenImport mod rest
      rest' = skipOpenRest rest
      (_, decls) = parseTopLevel rest'
  in ("", decl : decls)
parseTopLevel (TokPostulate : rest) =
  let (decl, rest') = parsePostulate rest
      (_, decls) = parseTopLevel rest'
  in ("", decl : decls)
parseTopLevel (TokData : rest) =
  let (decl, rest') = parseData rest
      (_, decls) = parseTopLevel rest'
  in ("", decl : decls)
parseTopLevel (TokRewrite : rest) =
  let (decl, rest') = parseRewrite rest
      (_, decls) = parseTopLevel rest'
  in ("", decl : decls)
parseTopLevel (TokName name : TokColon : rest) =
  let (ty, rest') = parseType rest
      (body, rest'') = parseBody rest'
      decl = DDef name ty [Clause [] body]
      (_, decls) = parseTopLevel rest''
  in ("", decl : decls)
parseTopLevel (_ : rest) = parseTopLevel rest
parseTopLevel [] = ("", [])

dropImport :: [Token] -> [Token]
dropImport [] = []
dropImport (TokSemi : rest) = rest
dropImport (_ : rest) = dropImport rest

-- | 跳过 open import 的 using/hiding/renaming 子句
skipOpenRest :: [Token] -> [Token]
skipOpenRest (TokUsing : TokLParen : rest) = skipToClose rest
skipOpenRest (TokName _ : rest) = skipOpenRest rest
skipOpenRest rest = rest

skipToClose :: [Token] -> [Token]
skipToClose (TokRParen : rest) = rest
skipToClose (_ : rest) = skipToClose rest
skipToClose [] = []

parseOpenImport :: Text -> [Token] -> Decl
parseOpenImport mod (TokUsing : TokLParen : rest) =
  let names = takeWhile (/= TokRParen) rest
  in DOpenUsing mod [ n | TokName n <- names ]
parseOpenImport mod _ = DOpen mod

-- 便捷: token列表 → 声明列表解析
parseDecls :: [Token] -> [Decl]
parseDecls [] = []
parseDecls (TokComment c : rest) = DComment c : parseDecls rest
parseDecls (TokPostulate : rest) =
  let (d, rest') = parsePostulate rest
  in d : parseDecls rest'
parseDecls (TokData : rest) =
  let (d, rest') = parseData rest
  in d : parseDecls rest'
parseDecls (TokRewrite : rest) =
  let (d, rest') = parseRewrite rest
  in d : parseDecls rest'
parseDecls (TokName name : TokColon : rest) =
  let (ty, rest') = parseType rest
      (body, rest'') = parseBody rest'
  in DDef name ty [Clause [] body] : parseDecls rest''
parseDecls (_ : rest) = parseDecls rest

----------------------------------------------------------------------
-- Postulate
----------------------------------------------------------------------

parsePostulate :: [Token] -> (Decl, [Token])
parsePostulate (TokName name : TokColon : rest) =
  let (ty, rest') = parseType rest
  in (DPostulate name ty, rest')
parsePostulate rest = (DPostulate "?" TNat, rest)

----------------------------------------------------------------------
-- Data
----------------------------------------------------------------------

parseData :: [Token] -> (Decl, [Token])
parseData (TokName name : TokColon : rest) =
  let (ty, afterTy) = parseType rest
      (cons, rest') = parseConstructors afterTy
  in (DData name [] cons, rest')
parseData rest = (DData "?" [] [], rest)

parseConstructors :: [Token] -> ([ConDecl], [Token])
parseConstructors (TokWhere : rest) = go rest
  where
    go (TokName n : TokColon : rest') =
      let (ty, rest'') = parseType rest'
          (more, rest''') = go rest''
      in (ConDecl n ty : more, rest''')
    go rest' = ([], rest')
parseConstructors rest = ([], rest)

----------------------------------------------------------------------
-- Rewrite
----------------------------------------------------------------------

parseRewrite :: [Token] -> (Decl, [Token])
parseRewrite (TokName name : rest) =
  let (eq, rest') = parseRewriteEq rest
  in (DRewrite name eq, rest')
parseRewrite rest = (DRewrite "?" Hole, rest)

parseRewriteEq :: [Token] -> (Term, [Token])
parseRewriteEq (TokColon : rest) =
  let (t, rest') = parseTerm rest
  in (t, rest')
parseRewriteEq rest = (Hole, rest)

----------------------------------------------------------------------
-- Body
----------------------------------------------------------------------

parseBody :: [Token] -> (Term, [Token])
parseBody (TokEqual : TokRefl : rest) = (Refl, rest)
parseBody (TokEqual : TokHole : rest) = (Hole, rest)
parseBody (TokEqual : rest) =
  let (t, rest') = parseTerm rest
  in (t, rest')
parseBody rest = (Hole, rest)

----------------------------------------------------------------------
-- Type parsing
----------------------------------------------------------------------

parseType :: [Token] -> (Type, [Token])
parseType (TokSet : rest) = (TSet, rest)
parseType (TokNat : rest) = (TNat, rest)
parseType (TokFin : TokNum n : rest) = (TFin (Lit (LNat (fromIntegral n))), rest)
parseType (TokFin : rest) = (TFin (Lit (LNat 0)), rest)
parseType (TokVec : rest) =
  let (a, rest') = parseType rest
      (n, rest'') = parseTerm rest'
  in (TVec a n, rest'')
parseType (TokName n : rest) =
  parseTypeApp (TDef n) rest

parseTypeApp :: Type -> [Token] -> (Type, [Token])
parseTypeApp acc (TokName n : rest) = parseTypeApp (TApp acc (Def n)) rest
parseTypeApp acc (TokNum n : rest) = parseTypeApp (TApp acc (Lit (LNat (fromIntegral n)))) rest
parseTypeApp acc (TokLParen : rest) =
  let (t, rest') = parseType rest
  in case rest' of
    TokRParen : rest'' -> parseTypeApp (TApp acc (Lit (LNat 0))) rest''  -- simplified
    _ -> (acc, rest)
parseTypeApp acc rest = (acc, rest)

----------------------------------------------------------------------
-- Term parsing (simplified)
----------------------------------------------------------------------

parseTerm :: [Token] -> (Term, [Token])
parseTerm (TokName n : rest) = parseTermApp (Var n) rest
parseTerm (TokNum n : rest) = (Lit (LNat (fromIntegral n)), rest)
parseTerm (TokRefl : rest) = (Refl, rest)
parseTerm (TokHole : rest) = (Hole, rest)
parseTerm rest = (Hole, rest)

parseTermApp :: Term -> [Token] -> (Term, [Token])
parseTermApp acc (TokName n : rest) = parseTermApp (App acc (Def n)) rest
parseTermApp acc (TokNum n : rest) = parseTermApp (App acc (Lit (LNat (fromIntegral n)))) rest
parseTermApp acc rest = (acc, rest)
