{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Dayan.ProofGen.Emit where
import Data.Text (Text); import qualified Data.Text as T; import Dayan.ProofGen.AST

emitFile (AgdaFile opts modName decls) = T.unlines $ [opts, "module " <> modName <> " where", ""] ++ concatMap emitDecl decls

emitDecl = \case
  DModule n ds -> ("module " <> n <> " where") : map ("  " <>) (concatMap emitDecl ds)
  DOpen n -> ["open " <> n]; DOpenUsing n ns -> ["open " <> n <> " using (" <> T.intercalate "; " ns <> ")"]
  DImport n -> ["import " <> n]; DPostulate n ty -> ["postulate", "  " <> n <> " : " <> emitType ty]
  DDef n ty cls -> map (\c -> n <> " : " <> emitType ty <> "\n" <> n <> " " <> emitClause c) cls
  DData n _ cons -> ("data " <> n <> " : Set where") : map (\c -> "  " <> emitConDecl c) cons
  DComment t -> ["-- " <> t]

emitClause (Clause pats body) = T.intercalate " " (map emitPattern pats) <> " = " <> emitTerm body
emitConDecl (ConDecl n ty) = n <> " : " <> emitType ty

emitType = \case
  TSet -> "Set"; TNat -> "Nat"; TDef n -> n; TFun a b -> emitType a <> " → " <> emitType b
  TPi x a b -> "(" <> x <> " : " <> emitType a <> ") → " <> emitType b
  TFin n -> "Fin " <> emitTerm n; TVec a n -> "Vec " <> emitType a <> " " <> emitTerm n
  TApp (TApp (TDef "_≡_") a) b -> emitTerm a <> " ≡ " <> emitTerm b
  TApp t e -> emitType t <> " " <> emitTerm e

emitTerm = \case
  Var x -> x; Def f -> f; Refl -> "refl"; Hole -> "{!!}"; Lit l -> emitLit l
  Lam x e -> "λ " <> x <> " → " <> emitTerm e
  Sym p -> "sym " <> emitTerm p; Trans p q -> "trans " <> emitTerm p <> " " <> emitTerm q
  Cong f p -> "cong " <> emitTerm f <> " " <> emitTerm p
  Subst _ _ eq x -> "subst (λ _ → _) " <> emitTerm eq <> " " <> emitTerm x
  App (App (Def "_,_") a) b -> "(" <> emitTerm a <> " , " <> emitTerm b <> ")"
  App f a -> emitTerm f <> " " <> emitTerm a
  Ann e t -> "(" <> emitTerm e <> " : " <> emitType t <> ")"; Pi _ _ _ -> "{! Pi !}"

emitLit = \case; LNat n -> T.pack (show n); LZero -> "zero"; LSuc n -> "suc " <> emitTerm n
emitPattern = \case; PVar x -> x; PWild -> "_"; PLit l -> emitLit l; PCon c [] -> c; PCon c ps -> c <> " " <> T.unwords (map emitPattern ps)
