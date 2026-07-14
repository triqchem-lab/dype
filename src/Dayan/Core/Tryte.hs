-- | Dayan.Core.Tryte — Fin 729 格点 (6-trit 编码)
--
-- 语义: T⁶ 环面的一个局部格点截面
-- 状态空间: 3⁶ = 729
-- 与 Agda T6Lattice 双射一致 (基3编码)
--
-- 基3编码: v0 + 3·v1 + 9·v2 + 27·v3 + 81·v4 + 243·v5
-- 其中 vi ∈ {N=0, Z=1, P=2}

module Dayan.Core.Tryte where
