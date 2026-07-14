{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Dayan.ProofGen.Emit where
import Data.Text (Text); import qualified Data.Text as T; import Dayan.ProofGen.AST

emitFile :: AgdaFile -> Text
emitFile (AgdaFile opts modName decls) = T.unlines $ [opts, "module " <> modName <> " where", ""] ++ concatMap emitDecl decls

emitDecl :: Decl -> [Text]
emitDecl = \case
  DModule name ds -> ("module " <> name <> " where") : map ("  " <>) (concatMap emitDecl ds)
  DOpen name -> ["open " <> name]
  DOpenUsing name ns -> ["open " <> name <> " using (" <> T.intercalate "; " ns <> ")"]
  DImport name -> ["import " <> name]
  DPostulate name ty -> ["postulate", "  " <> name <> " : " <> emitType ty]
  DDef name ty clauses -> (name <> " : " <> emitType ty) : map emitClause clauses
  DData name _ cons -> ("data " <> name <> " : Set where") : map (\c -> "  " <> emitConDecl c) cons
  DComment txt -> ["-- " <> txt]

emitClause (Clause pats body) = T.intercalate " " (map emitPattern pats) <> " = " <> emitTerm body
emitConDecl (ConDecl name ty) = name <> " : " <> emitType ty

emitType :: Type -> Text
emitType = \case
  TSet -> "Set"; TNat -> "Nat"; TDef n -> n
  TPi x a b -> "(" <> x <> " : " <> emitType a <> ") -> " <> emitType b
  TApp t e -> emitType t <> " " <> emitTerm e
  TFin n -> "Fin " <> emitTerm n
  TVec a n -> "Vec " <> emitType a <> " " <> emitTerm n
  TFun a b -> emitType a <> " -> " <> emitType b

emitTerm :: Term -> Text
emitTerm = \case
  Var x -> x; Def f -> f; Refl -> "refl"; Hole -> "{!!}"
  App f a -> emitTerm f <> " " <> parensIfApp a
  Lam x e -> "\\ " <> x <> " -> " <> emitTerm e
  Lit l -> emitLit l
  Sym p -> "sym (" <> emitTerm p <> ")"
  Trans p q -> "trans (" <> emitTerm p <> ") (" <> emitTerm q <> ")"
  Cong f p -> "cong (" <> emitTerm f <> ") (" <> emitTerm p <> ")"
  Subst _ _ eq x -> "subst _ (" <> emitTerm eq <> ") (" <> emitTerm x <> ")"
  Ann e t -> "(" <> emitTerm e <> " : " <> emitType t <> ")"
  Pi _ _ _ -> "{! Pi !}"

emitLit :: Lit -> Text
emitLit = \case
  LNat n -> T.pack (show n); LZero -> "zero"; LSuc n -> "suc " <> emitTerm n

emitPattern :: Pattern -> Text
emitPattern = \case
  PVar x -> x; PWild -> "_"; PLit l -> emitLit l
  PCon c [] -> c; PCon c ps -> c <> " " <> T.unwords (map emitPattern ps)

parens :: Text -> Text; parens t = "(" <> t <> ")"
parensIfApp :: Term -> Text
parensIfApp t@App{} = parens (emitTerm t); parensIfApp t = emitTerm t
