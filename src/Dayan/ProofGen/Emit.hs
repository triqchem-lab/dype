{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Dayan.ProofGen.Emit where
import Data.Text (Text)
import qualified Data.Text as T
import Dayan.ProofGen.AST

emitFile :: AgdaFile -> Text
emitFile (AgdaFile _opts _modName [DPassThrough t]) = t  -- 全量透传: 直接输出原始源文本
emitFile (AgdaFile opts modName decls) =
  let imported = collectImportedNames decls
      filtered = filter (not . isRedundantInfix imported) decls
      -- 检测 Sovereign 依赖: 需要 --cubical --guardedness
      needsSovereign = any isSovereignImport decls
      opts' = if needsSovereign && not ("--cubical" `T.isInfixOf` opts)
              then T.replace "#-}" "--cubical --guardedness #-}" opts
              else opts
  in T.unlines $ [opts', "module " <> modName <> " where", ""] ++ concatMap emitDecl filtered

-- | 检测是否导入 Sovereign 模块
isSovereignImport :: Decl -> Bool
isSovereignImport (DOpen m) = "Sovereign" `T.isInfixOf` m
isSovereignImport (DOpenUsing m _) = "Sovereign" `T.isInfixOf` m
isSovereignImport _ = False

-- | 收集所有 open import ... using (...) 引入的名称
collectImportedNames :: [Decl] -> [Name]
collectImportedNames = concatMap go
  where
    go (DOpenUsing _ ns) = ns
    go _ = []

-- | 过滤对 import 名称的 infix 声明 (Agda 不允许)
isRedundantInfix :: [Name] -> Decl -> Bool
isRedundantInfix imported (DInfix _ _ ops) = all (`elem` imported) ops
isRedundantInfix _ _ = False

emitDecl :: Decl -> [Text]
emitDecl = \case
  DModule n ds -> ("module " <> n <> " where") : map ("  " <>) (concatMap emitDecl ds)
  DOpen n -> ["open import " <> n]; DOpenUsing n ns -> ["open import " <> n <> " using (" <> T.intercalate "; " ns <> ")"]
  DImport n -> ["open import " <> n]; DPostulate n ty -> ["postulate", "  " <> n <> " : " <> emitType ty]
  DDef n ty cls -> map (\c -> n <> " : " <> emitType ty <> "\n" <> n <> " " <> emitClause c) cls
  DClause n pats body -> [n <> " " <> T.unwords (map emitPattern pats) <> " = " <> emitTerm body]
  DData n _ cons -> ("data " <> n <> " : Set where") : map (\c -> "  " <> emitConDecl c) cons
  DRecord n params fields ->
    ("record " <> n <> (if null params then "" else " " <> T.unwords params) <> " : Set where")
    : "  field" : map (\(fn, ft) -> "    " <> fn <> " : " <> emitType ft) fields
  DRewrite n eq -> ["{-# REWRITE " <> n <> " #-}\n" <> "postulate\n  " <> n <> " : " <> emitTerm eq]
  DInfix fx prec ops -> [emitFixity fx <> " " <> T.pack (show prec) <> " " <> T.unwords ops]
  DComment t -> ["-- " <> t]
  DPassThrough t -> [t]

emitFixity :: Fixity -> Text
emitFixity InfixL = "infixl"; emitFixity InfixR = "infixr"; emitFixity InfixN = "infix"

emitClause :: Clause -> Text
emitClause (Clause pats body) = T.intercalate " " (map emitPattern pats) <> " = " <> emitTerm body

emitConDecl :: ConDecl -> Text
emitConDecl (ConDecl n ty) = n <> " : " <> emitType ty

emitType :: Type -> Text
emitType = \case
  TSet -> "Set"; TNat -> "Nat"; TDef n -> n
  TFun a b -> emitTypeArg a <> " → " <> emitType b
  TPi x a b -> "(" <> x <> " : " <> emitType a <> ") → " <> emitType b
  TPiImplicit x a b -> "{" <> x <> " : " <> emitType a <> "} → " <> emitType b
  TFin n -> "Fin " <> emitTerm n; TVec a n -> "Vec " <> emitType a <> " " <> emitTerm n
  TApp (TApp (TDef "_≡_") a) b -> emitTerm a <> " ≡ " <> emitTerm b
  TApp (TApp (TDef "_≤_") a) b -> emitTerm a <> " ≤ " <> emitTerm b
  TApp t e -> emitType t <> " " <> emitTerm e

-- | 函数类型参数需要括号: (A → B) → C
emitTypeArg :: Type -> Text
emitTypeArg t@(TFun _ _) = "(" <> emitType t <> ")"
emitTypeArg t = emitType t

emitTerm :: Term -> Text
emitTerm = \case
  Var x -> x; Def f -> f; Refl -> "refl"; Hole -> "{!!}"; Lit l -> emitLit l
  Lam x e -> "λ " <> x <> " → " <> emitTerm e
  Sym p -> "sym " <> emitTerm p; Trans p q -> "trans " <> emitTerm p <> " " <> emitTerm q
  Cong f p -> "cong " <> emitTerm f <> " " <> emitTerm p
  Subst _ _ eq x -> "subst (λ _ → _) " <> emitTerm eq <> " " <> emitTerm x
  -- 中缀运算符: App (App (Def "_op_") a) b → (a op b)
  App (App (Def op) a) b | isInfixOp op ->
    "(" <> emitTerm a <> " " <> stripUnderscores op <> " " <> emitTerm b <> ")"
  App f a -> emitTerm f <> " " <> emitTermAtom a
  Ann e t -> "(" <> emitTerm e <> " : " <> emitType t <> ")"; Pi _ _ _ -> "{! Pi !}"

-- | 原子项渲染: 复合项 (App/Lam) 作为参数时需要括号
-- 中缀运算符已自带括号, 不再重复
emitTermAtom :: Term -> Text
emitTermAtom t@(App (App (Def op) _) _) | isInfixOp op = emitTerm t
emitTermAtom t@(App _ _) = "(" <> emitTerm t <> ")"
emitTermAtom t@(Lam _ _) = "(" <> emitTerm t <> ")"
emitTermAtom t = emitTerm t

-- | 判断是否为中缀运算符名 (_xxx_ 形式)
isInfixOp :: Text -> Bool
isInfixOp t = T.length t >= 3 && T.head t == '_' && T.last t == '_'

-- | 去掉首尾下划线: _≟_ → ≟, _⊕_ → ⊕
stripUnderscores :: Text -> Text
stripUnderscores t = T.drop 1 (T.dropEnd 1 t)

emitLit :: Lit -> Text
emitLit = \case; LNat n -> T.pack (show n); LZero -> "zero"; LSuc n -> "suc " <> emitTerm n

emitPattern :: Pattern -> Text
emitPattern = \case; PVar x -> x; PWild -> "_"; PLit l -> emitLit l; PCon c [] -> c; PCon c ps -> c <> " " <> T.unwords (map emitPattern ps)
