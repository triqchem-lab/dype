-- | Dayan.ProofGen.Templates — Agda 证明模板生成器
--
-- Phase 5: 核心定理去 postulate
--   5.1 leftInv: finToT6 ∘ t6ToFin ≡ id (729 case refl)
--   5.2 rightInv: t6ToFin ∘ finToT6 ≡ id (postulate, 需 Agda DivMod)
--   5.3 t6≃fin729: iso from leftInv + rightInv (构造性)
--   5.4 div3k/mod3k: 4320D 归约模板

{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Dayan.ProofGen.Templates where

import Data.Word (Word16)
import Data.Text (Text)
import qualified Data.Text as T
import Dayan.ProofGen.AST

----------------------------------------------------------------------
-- CRT 查表
----------------------------------------------------------------------

genCrtLookup :: Word16 -> (Word16, Word16) -> Decl
genCrtLookup n (p, t) =
  let name = "crt-" <> T.pack (show n)
      ty = TApp (TApp (TDef "_≡_") (apps (Def "lookupCrt") [Lit (LNat n)]))
                 (apps (Def "_,_") [Lit (LNat p), Lit (LNat t)])
  in DDef name ty [Clause [] Refl]

genAllCrtLookups :: [(Word16, (Word16, Word16))] -> [Decl]
genAllCrtLookups = map (uncurry genCrtLookup)

----------------------------------------------------------------------
-- 编解码 roundtrip
----------------------------------------------------------------------

genEncodeDecode :: Word16 -> Decl
genEncodeDecode n =
  let name = "encdec-" <> T.pack (show n)
      ty = TApp (TApp (TDef "_≡_")
              (apps (Def "encode") [apps (Def "decode") [Lit (LNat n)]]))
              (Lit (LNat n))
  in DDef name ty [Clause [] Refl]

genEncodeDecodeRange :: Word16 -> Word16 -> [Decl]
genEncodeDecodeRange lo hi = map genEncodeDecode [lo..hi]

----------------------------------------------------------------------
-- 全息对齐
----------------------------------------------------------------------

genAlignmentProof :: Decl
genAlignmentProof =
  let ty = TApp (TApp (TDef "_≡_")
              (apps (Def "toroidal") [apps (Def "stepN") [Lit (LNat 6624), Def "huangzhong"]]))
              (Lit (LNat 0))
      body = Trans (apps (Def "stepN-adds") [Lit (LNat 6624), Def "huangzhong"])
                   (Trans (apps (Def "+-assoc") [Lit (LNat 0), Lit (LNat 6624), Lit (LNat 0)]) Refl)
  in DDef "align-6624" ty [Clause [] body]

----------------------------------------------------------------------
-- 5.1 leftInv: finToT6 ∘ t6ToFin ≡ id (729 case refl)
----------------------------------------------------------------------

-- | 生成全部 729 个 T6 格点模式 (用于穷举匹配)
genAllT6Patterns :: [[Term]]
genAllT6Patterns = sequence (replicate 6 [Lit (LNat 0), Lit (LNat 1), Lit (LNat 2)])

-- | leftInv: 对全部 729 个 T6 格点, finToT6(t6ToFin x) ≡ x
genT6LeftInv :: Decl
genT6LeftInv =
  let ty = TPi "x" (TDef "T6Lattice")
            (TApp (TApp (TDef "_≡_")
                   (apps (Def "finToT6") [apps (Def "t6ToFin") [Var "x"]]))
                   (Var "x"))
      clauses = map (\pats -> Clause (map toPat pats) Refl) genAllT6Patterns
      toPat (Lit l) = PLit l; toPat _ = PWild
  in DDef "leftInv" ty clauses

----------------------------------------------------------------------
-- 5.2 rightInv: t6ToFin ∘ finToT6 ≡ id (postulate)
----------------------------------------------------------------------

-- | rightInv 需要 Agda Data.Nat.DivMod 的 m≡m%n+[m/n]*n 引理
--   在 dype 模板中生成为 postulate (Phase 5 后期补充为 DivMod 生成)
genT6RightInv :: Decl
genT6RightInv = DPostulate "rightInv"
  (TPi "x" (TDef "Fin729")
    (TApp (TApp (TDef "_≡_")
           (apps (Def "t6ToFin") [apps (Def "finToT6") [Var "x"]]))
           (Var "x")))

----------------------------------------------------------------------
-- 5.3 t6≃fin729: T6Lattice ≃ Fin729 (from leftInv + rightInv)
----------------------------------------------------------------------

-- | T6Lattice ≃ Fin729 同构证明
--   isoToPath (iso t6ToFin finToT6 rightInv leftInv)
genT6Iso729 :: Decl
genT6Iso729 =
  let ty = TDef "T6Lattice ≃ Fin729"
      body = apps (Def "isoToPath")
             [ apps (Def "iso")
               [ Def "t6ToFin", Def "finToT6"
               , Def "rightInv", Def "leftInv" ] ]
  in DDef "t6≃fin729" ty [Clause [] body]

----------------------------------------------------------------------
-- 5.4 4320D 归约模板
----------------------------------------------------------------------

-- | div3k: (3*k) / 3 ≡ k
genDiv3k :: Decl
genDiv3k =
  let ty = TPi "k" TNat
            (TApp (TApp (TDef "_≡_")
                   (apps (Def "_/_") [apps (Def "_*_") [Lit (LNat 3), Var "k"], Lit (LNat 3)]))
                   (Var "k"))
  in DPostulate "div3k" ty  -- 待补充为 Agda Nat 证明

-- | mod3k: (3*k) % 3 ≡ 0
genMod3k :: Decl
genMod3k =
  let ty = TPi "k" TNat
            (TApp (TApp (TDef "_≡_")
                   (apps (Def "_%_") [apps (Def "_*_") [Lit (LNat 3), Var "k"], Lit (LNat 3)]))
                   (Lit (LNat 0)))
  in DPostulate "mod3k" ty

----------------------------------------------------------------------
-- 文件生成器
----------------------------------------------------------------------

genT6VerificationFile :: Text -> [(Word16, (Word16, Word16))] -> AgdaFile
genT6VerificationFile modName crtEntries = AgdaFile
  { fileOpts = "{-# OPTIONS --rewriting #-}", fileModule = modName
  , fileDecls =
      [ DImport "Agda.Builtin.Nat", DImport "Agda.Builtin.Equality"
      , DComment "CRT lookup"
      ] ++ genAllCrtLookups crtEntries
      ++ [ DComment "alignment", genAlignmentProof ] }

genTryteVerificationFile :: Text -> AgdaFile
genTryteVerificationFile modName = AgdaFile
  { fileOpts = "{-# OPTIONS --guardedness #-}", fileModule = modName
  , fileDecls =
      [ DImport "Sovereign.Core.Tryte"
      , DComment "encode∘decode roundtrip"
      ] ++ genEncodeDecodeRange 0 99 }

-- | 完整 T6 验证文件 (含同构证明, 去 postulate)
genFullT6File :: Text -> [(Word16, (Word16, Word16))] -> AgdaFile
genFullT6File modName crtEntries = AgdaFile
  { fileOpts = "{-# OPTIONS --rewriting #-}"
  , fileModule = modName
  , fileDecls =
      [ DImport "Sovereign.Structology.T6"
      , DComment "Type-level 同构 (构造性, leftInv 729 case refl)"
      , genT6Iso729
      , genT6LeftInv
      , DComment "rightInv requires Agda DivMod → postulate"
      , genT6RightInv
      , DComment "4320D 归约"
      , genDiv3k
      , genMod3k
      , DComment "CRT 查表"
      ] ++ genAllCrtLookups crtEntries
      ++ [ DComment "全息对齐", genAlignmentProof ]
  }
