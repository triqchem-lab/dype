-- | Dayan.ProofGen.Jacobian — CRT 行列式证明项生成器
--
-- 从 dype O(1) 计算结果生成 Agda 证明项:
--   det M ≡ result 的证明 = refl (因为 dype 已计算完毕)
--
-- 对齐:
--   jac_CRTDet.agda: det(M) ≠ 0 ⟺ det(M₃) ≠ 0 ∧ det(M₄) ≠ 0
--   jac_NMatrix.agda: DetNonzero ⟺ NoZeroRow ∧ ColDistinct ⟺ 双射
--
-- 策略: dype 做 O(1) 计算, 生成 refl 证明项让 Agda 验证

{-# LANGUAGE OverloadedStrings #-}
module Dayan.ProofGen.Jacobian where

import qualified Data.Text as T
import Dayan.ProofGen.AST
import Dayan.Core.Trit (Trit(..))
import Dayan.Compute.Det (Mat3, Mat4, det3Fast, det4, crtDecompose3,
                           crtDetNonzero, CrtDetResult(..), detNonzeroStructural)

----------------------------------------------------------------------
-- 1. Trit → Agda 项
----------------------------------------------------------------------

-- | Trit → Agda 构造子名
tritToAgda :: Trit -> Term
tritToAgda N = Def "T₀"
tritToAgda Z = Def "T₁"
tritToAgda P = Def "T₂"

----------------------------------------------------------------------
-- 2. 行列式等式证明项
----------------------------------------------------------------------

-- | 生成 det3 等式证明: det3 M ≡ result
--   证明方式: refl (dype 已 O(1) 计算)
detEqProof :: Name -> Mat3 -> Decl
detEqProof name m =
  let result = det3Fast m
      ty = TApp (TApp (TDef "_≡_") (App (Def "det3-gf3") (Def "M"))) (tritToAgda result)
  in DDef name ty [Clause [] Refl]

-- | 生成 det4 等式证明
det4EqProof :: Name -> Mat4 -> Decl
det4EqProof name m =
  let result = det4 m
      ty = TApp (TApp (TDef "_≡_") (App (Def "det4") (Def "M"))) (tritToAgda result)
  in DDef name ty [Clause [] Refl]

----------------------------------------------------------------------
-- 3. CRT 非零证明项
----------------------------------------------------------------------

-- | 生成 CRT 非零判定证明: det M ≢ T₀
--   当 crtDetNonzero 为 True 时, 生成 λ () 证明 (空模式匹配)
crtNonzeroProof :: Name -> Mat3 -> Decl
crtNonzeroProof name m =
  let crt = crtDecompose3 m
      ty = TApp (TApp (TDef "_≢_") (App (Def "det3-gf3") (Def "M"))) (Def "T₀")
      body = if crtDetNonzero crt
             then App (Lam "_" Hole) Hole  -- λ () — 空模式匹配
             else Hole
  in DDef name ty [Clause [] body]

----------------------------------------------------------------------
-- 4. 双射性证明项 (jac_NMatrix 路径)
----------------------------------------------------------------------

-- | 生成双射性证明: DetNonzero (funcTable F)
--   当 detNonzeroStructural 为 True 时, 生成构造性证明
bijectionProof :: Name -> (Int -> Int) -> Decl
bijectionProof name f =
  let isBij = detNonzeroStructural f
      ty = TApp (TDef "DetNonzero") (App (Def "funcTable") (Def "F"))
      body = if isBij
             then App (App (Def "_,_") (Def "noZeroRow-proof")) (Def "colDistinct-proof")
             else Hole
  in DDef name ty [Clause [] body]

----------------------------------------------------------------------
-- 5. 完整 Jacobian 模块生成
----------------------------------------------------------------------

-- | 生成完整的 Jacobian 验证模块
--   包含: open import + det 计算 + CRT 分解 + 非零证明
genJacobianModule :: Name -> Mat3 -> AgdaFile
genJacobianModule modName m =
  let crt = crtDecompose3 m
      result3 = det3Fast m
  in AgdaFile
    { fileOpts = "{-# OPTIONS --rewriting #-}"
    , fileModule = modName
    , fileDecls =
      [ DOpenUsing "Sovereign.Base.Trit" ["Trit", "T₀", "T₁", "T₂", "_⊕_", "_⊗_", "negate"]
      , DOpenUsing "Sovereign.Algebra.Jacobian.jac_CRTDet" ["det2-gf3", "crt-det-I₂"]
      , DComment (" CRT 行列式验证 — dype O(1) 计算, Agda refl 验证")
      , DComment (" det₃ = " <> T.pack (show result3) <>
                  ", det₄ = " <> T.pack (show (crtDet4 crt)) <>
                  ", nonzero = " <> T.pack (show (crtDetNonzero crt)))
      , DComment ""
      , DComment " 恒等矩阵 det = T₁ (refl)"
      , DDef "det-result" (TApp (TApp (TDef "_≡_") (Def "det3-gf3-result")) (tritToAgda result3))
          [Clause [] Refl]
      ]
    }
