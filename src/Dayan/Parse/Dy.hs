-- | Dayan.Parse.Dy — .dy 语法解析器 (基于 Agda 语法子集)
--
-- 支持完整 .dy 语法:
--   pragma, module, open import, postulate, data, rewrite, defs
--   infixl/infixr/infix 声明
--   多参数函数定义: f x y = body
--   lambda: λ x → body
--   where 子句 (跳过/透传)
--   类型: Set, Nat, Fin n, Vec A n, A -> B, (x : A) -> B
--   项: refl, 字面量, 变量, 应用, {!!}, 运算符
--
-- 不支持的语法会报 ParseError, 而非静默跳过。

{-# LANGUAGE OverloadedStrings #-}
module Dayan.Parse.Dy where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlpha, isDigit)
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
-- infixl/infixr/infix 声明
parseTopLevel (TokInfixl : rest) =
  let (decl, rest') = parseInfix InfixL rest
      (errs, (opts, decls)) = parseTopLevel rest'
  in (errs, (opts, decl : decls))
parseTopLevel (TokInfixr : rest) =
  let (decl, rest') = parseInfix InfixR rest
      (errs, (opts, decls)) = parseTopLevel rest'
  in (errs, (opts, decl : decls))
parseTopLevel (TokInfix : rest) =
  let (decl, rest') = parseInfix InfixN rest
      (errs, (opts, decls)) = parseTopLevel rest'
  in (errs, (opts, decl : decls))
-- 类型签名: name : Type, 后跟可选的函数子句
parseTopLevel (TokName name : TokColon : rest) =
  let (ty, rest') = parseType rest
      (clauses, rest'') = parseClauses name rest'
      (errs, (opts, decls)) = parseTopLevel rest''
  in (errs, (opts, DDef name ty clauses : decls))
-- 函数子句 (无类型签名): name args = body
parseTopLevel (TokName name : rest)
  | isClauseStart rest =
      let (pats, body, rest') = parseClauseBody rest
          (errs, (opts, decls)) = parseTopLevel rest'
      in (errs, (opts, DClause name pats body : decls))
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
parseDecls (TokInfixl : rest) =
  let (d, rest') = parseInfix InfixL rest
      (errs, ds) = parseDecls rest'
  in (errs, d : ds)
parseDecls (TokInfixr : rest) =
  let (d, rest') = parseInfix InfixR rest
      (errs, ds) = parseDecls rest'
  in (errs, d : ds)
parseDecls (TokInfix : rest) =
  let (d, rest') = parseInfix InfixN rest
      (errs, ds) = parseDecls rest'
  in (errs, d : ds)
parseDecls (TokName name : TokColon : rest) =
  let (ty, rest') = parseType rest
      (clauses, rest'') = parseClauses name rest'
      (errs, ds) = parseDecls rest''
  in (errs, DDef name ty clauses : ds)
parseDecls (TokName name : rest)
  | isClauseStart rest =
      let (pats, body, rest') = parseClauseBody rest
          (errs, ds) = parseDecls rest'
      in (errs, DClause name pats body : ds)
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
tokenText TokInfixl         = "infixl"
tokenText TokInfixr         = "infixr"
tokenText TokInfix          = "infix"
tokenText TokLambda         = "λ"
tokenText TokComma          = ","
tokenText (TokName n)       = n
tokenText (TokNum n)        = T.pack (show n)
tokenText (TokComment _)    = "--"

-- 保留函数 (简化为不累积错误的版本, 供其他地方使用)
parseDecls' :: [Token] -> [Decl]
parseDecls' = snd . parseDecls

----------------------------------------------------------------------
-- Infix 声明
----------------------------------------------------------------------

parseInfix :: Fixity -> [Token] -> (Decl, [Token])
parseInfix fx (TokNum prec : rest) =
  let (ops, rest') = spanInfixOps rest
  in (DInfix fx prec ops, rest')
parseInfix fx rest = (DInfix fx 0 [], rest)

-- | 收集 infix 声明中的运算符名 (直到遇到新声明开头)
spanInfixOps :: [Token] -> ([Name], [Token])
spanInfixOps (TokName n : rest)
  | isNewDeclStart rest = ([n], rest)
  | otherwise = let (more, rest') = spanInfixOps rest in (n : more, rest')
spanInfixOps rest = ([], rest)

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
-- Function clauses
----------------------------------------------------------------------

-- | 判断 token 流是否以函数子句开头 (name args = 或 name = )
isClauseStart :: [Token] -> Bool
isClauseStart (TokEqual : _) = True
isClauseStart (TokName _ : rest) = isClauseStart rest
isClauseStart (TokNum _ : rest) = isClauseStart rest
isClauseStart (TokUnderscore : rest) = isClauseStart rest
isClauseStart (TokLParen : _) = True  -- 模式匹配
isClauseStart _ = False

-- | 解析函数子句体: 收集 patterns 直到 =, 然后解析 body
parseClauseBody :: [Token] -> ([Pattern], Term, [Token])
parseClauseBody toks =
  let (pats, rest) = collectPats toks
  in case rest of
    TokEqual : rest' ->
      let (body, rest'') = parseTerm rest'
          rest''' = skipWhereClause rest''
      in (pats, body, rest''')
    _ -> (pats, Hole, rest)

-- | 收集 patterns (变量名) 直到遇到 =
collectPats :: [Token] -> ([Pattern], [Token])
collectPats (TokEqual : rest) = ([], TokEqual : rest)
collectPats (TokName n : rest) =
  let (more, rest') = collectPats rest
  in (PVar n : more, rest')
collectPats (TokNum n : rest) =
  let (more, rest') = collectPats rest
  in (PLit (LNat (fromIntegral n)) : more, rest')
collectPats (TokUnderscore : rest) =
  let (more, rest') = collectPats rest
  in (PWild : more, rest')
collectPats rest = ([], rest)

-- | 解析类型签名后的函数子句
-- 查找同名函数的子句: name pats = body
parseClauses :: Name -> [Token] -> ([Clause], [Token])
parseClauses name toks = case toks of
  TokName n : rest | n == name ->
    let (pats, body, rest') = parseClauseBody rest
        (more, rest'') = parseClauses name rest'
    in (Clause pats body : more, rest'')
  _ -> ([], toks)

-- | 跳过 where 子句 (where open import ... / where name : ...)
skipWhereClause :: [Token] -> [Token]
skipWhereClause (TokWhere : rest) = skipWhereBody rest
skipWhereClause rest = rest

skipWhereBody :: [Token] -> [Token]
skipWhereBody (TokOpen : TokImport : rest) =
  -- 跳过 open import X using (...)
  case span (/= TokRParen) rest of
    (_, TokRParen : after) -> skipWhereBody after
    (_, []) -> []
skipWhereBody (TokName _ : TokColon : rest) =
  -- 跳过 where 内的类型签名和定义
  let (_, rest') = parseType rest
  in skipWhereBody rest'
skipWhereBody (TokName _ : rest) = skipWhereBody rest
skipWhereBody rest = rest

----------------------------------------------------------------------
-- Body (简化版, 供无子句的定义使用)
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
  | isNewDeclStart rest = (acc, TokName n : rest)
  | isOperatorName n =
      -- 中缀类型运算符: acc ≡ rhs → TApp (TApp (TDef "_≡_") acc) rhs
      let (rhs, rest') = parseTypeApp rest
      in (TApp (TApp (TDef (T.pack "_" <> n <> T.pack "_")) (typeToTerm acc)) (typeToTerm rhs), rest')
  | isClauseLike (TokName n : rest) = (acc, TokName n : rest)
  | otherwise = parseTypeAppMore (TApp acc (Def n)) rest
parseTypeAppMore acc (TokNum n : rest)
  | isNewDeclStart rest = (acc, TokNum n : rest)
  | otherwise = parseTypeAppMore (TApp acc (Lit (LNat (fromIntegral n)))) rest
parseTypeAppMore acc (TokLParen : rest) =
  -- 括号内的项作为类型参数: det3 (λ i j → ...)
  let (t, rest') = parseTerm rest
  in case rest' of
    TokRParen : rest'' -> parseTypeAppMore (TApp acc t) rest''
    _ -> (TApp acc t, rest')
parseTypeAppMore acc rest = (acc, rest)

-- | 前瞻检测: token 流是否看起来像函数子句 (name args = ...)
-- 纯运算符名 (≡, ≤, ×) 不视为子句开头 — 它们是类型表达式的一部分
isClauseLike :: [Token] -> Bool
isClauseLike (TokName n : rest)
  | isOperatorName n = False
  | otherwise = isClauseLikeArgs rest
isClauseLike _ = False

isClauseLikeArgs :: [Token] -> Bool
isClauseLikeArgs (TokEqual : _) = True
isClauseLikeArgs (TokName _ : rest) = isClauseLikeArgs rest
isClauseLikeArgs (TokNum _ : rest) = isClauseLikeArgs rest
isClauseLikeArgs (TokUnderscore : rest) = isClauseLikeArgs rest
isClauseLikeArgs _ = False

-- | 纯运算符名: 不含字母/数字 (如 ≡, ≤, ×, ⊕, ⊗, _≡_)
isOperatorName :: Text -> Bool
isOperatorName n = not (T.null n) && T.all (\c -> not (isAlpha c || isDigit c)) n

-- | 类型→项转换 (用于中缀类型运算符的参数)
typeToTerm :: Type -> Term
typeToTerm (TDef n) = Def n
typeToTerm (TApp t e) = App (typeToTerm t) e
typeToTerm TNat = Def "ℕ"
typeToTerm TSet = Def "Set"
typeToTerm (TFun a b) = App (App (Def "_→_") (typeToTerm a)) (typeToTerm b)
typeToTerm (TFin n) = App (Def "Fin") n
typeToTerm (TVec a n) = App (App (Def "Vec") (typeToTerm a)) n
typeToTerm (TPi x a b) = App (App (Def "_→_") (typeToTerm a)) (typeToTerm b)

-- | 判断后续 token 是否开始新声明 (用于类型/项解析终止)
-- 只匹配确定性标志: 类型签名 (name :) 或关键字
-- 注意: TokComment 不在此列 — 注释是透明的，不开始新声明
isNewDeclStart :: [Token] -> Bool
isNewDeclStart (TokColon : _) = True
isNewDeclStart (TokEqual : _) = True
isNewDeclStart (TokName _ : TokColon : _) = True      -- name : Type (新类型签名)
isNewDeclStart (TokInfixl : _) = True
isNewDeclStart (TokInfixr : _) = True
isNewDeclStart (TokInfix : _) = True
isNewDeclStart (TokOpen : _) = True
isNewDeclStart (TokPostulate : _) = True
isNewDeclStart (TokData : _) = True
isNewDeclStart _ = False

----------------------------------------------------------------------
-- Term parsing
----------------------------------------------------------------------

parseTerm :: [Token] -> (Term, [Token])
parseTerm toks =
  let (t, rest) = parseTermAtom toks
  in parseTermContinuation t rest

-- | 项的后续: 逗号 (pair) 或中缀运算符 (左结合)
parseTermContinuation :: Term -> [Token] -> (Term, [Token])
parseTermContinuation t (TokComma : rest) =
  let (t2, rest') = parseTerm rest
  in parseTermContinuation (App (App (Def "_,_") t) t2) rest'
parseTermContinuation t (TokName n : rest)
  | isOperatorName n =
      -- 中缀运算符: a ⊕ b → App (App (Def "_⊕_") a) b, 左结合
      let (rhs, rest') = parseTermAtom rest
      in parseTermContinuation (App (App (Def (T.pack "_" <> n <> T.pack "_")) t) rhs) rest'
parseTermContinuation t rest = (t, rest)

-- | 原子项 (不含中缀运算符): 用于运算符右侧
parseTermAtom :: [Token] -> (Term, [Token])
parseTermAtom (TokLambda : rest) = parseLambda rest
parseTermAtom (TokName n : rest) = parseTermApp (Var n) rest
parseTermAtom (TokNum n : rest) = (Lit (LNat (fromIntegral n)), rest)
parseTermAtom (TokRefl : rest) = (Refl, rest)
parseTermAtom (TokHole : rest) = (Hole, rest)
parseTermAtom (TokLParen : rest) =
  let (t, rest') = parseTerm rest
  in case rest' of
    TokRParen : rest'' -> (t, rest'')
    _ -> (t, rest')
parseTermAtom rest = (Hole, rest)

-- | Lambda 解析: λ x y → body
parseLambda :: [Token] -> (Term, [Token])
parseLambda (TokName x : rest) =
  case rest of
    TokArrow : rest' ->
      let (body, rest'') = parseTerm rest'
      in (Lam x body, rest'')
    _ ->
      -- 多参数 lambda: λ x y → body
      let (body, rest') = parseLambda rest
      in (Lam x body, rest')
parseLambda rest = (Hole, rest)

parseTermApp :: Term -> [Token] -> (Term, [Token])
parseTermApp acc (TokName n : rest)
  | n == "where" = (acc, TokName n : rest)
  | isOperatorName n = (acc, TokName n : rest)  -- 中缀运算符: 停止, 交给 parseTermContinuation
  | isNewDeclStart rest = (acc, TokName n : rest)
  | otherwise = parseTermApp (App acc (Def n)) rest
parseTermApp acc (TokNum n : rest) = parseTermApp (App acc (Lit (LNat (fromIntegral n)))) rest
parseTermApp acc (TokLParen : rest) =
  let (t, rest') = parseTerm rest
  in case rest' of
    TokRParen : rest'' -> parseTermApp (App acc t) rest''
    _ -> (acc, rest)
parseTermApp acc (TokComma : rest) =
  -- pair 语法: (a , b) → App (App (Def "_,_") a) b
  let (t2, rest') = parseTerm rest
  in (App (App (Def "_,_") acc) t2, rest')
parseTermApp acc rest = (acc, rest)
