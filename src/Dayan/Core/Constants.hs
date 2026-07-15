-- | Dayan.Core.Constants — 大衍体系核心常数
--
-- 144: 极向缠绕数 (12 律 × 12 仲吕周期)
--  46: 环向缠绕数 (全局共振周期)
-- 6624: 相位对齐格点遍历数 = 144 × 46
--  LCM: 3¹¹ × 2¹⁶ = 11,609,505,792
-- 全息 π_H: 144/46 ≈ 3.1304...
--
-- 对齐 Agda: Sovereign.Base.Invariants.agda
--   POLAR_WINDING    = 144
--   TOROIDAL_WINDING = 46
--   SOVEREIGN_LCM    = 11609505792

module Dayan.Core.Constants where

import Data.Word (Word16, Word64)

----------------------------------------------------------------------
-- 1. 缠绕数
----------------------------------------------------------------------

-- | 极向缠绕数 = 144 (12 律 × 12 仲吕周期)
polarWinding :: Word16
polarWinding = 144

-- | 环向缠绕数 = 46 (全局共振周期)
toroidalWinding :: Word16
toroidalWinding = 46

-- | 极向半周期 = 72 (阴阳分界)
polarHalf :: Word16
polarHalf = 72

----------------------------------------------------------------------
-- 2. 格点与环面常数
----------------------------------------------------------------------

-- | T⁶ 格点总数 = 3⁶ = 729
t6Cardinality :: Word16
t6Cardinality = 729

-- | 全息格点遍历数 = 144 × 46 = 6624
holographicCardinality :: Int
holographicCardinality = 144 * 46

-- | 全息 π_H = 144/46 (不可通约比)
holographicPi :: Double
holographicPi = 144.0 / 46.0

-- | 离散曲率 κ = √(144² + 46²) = √(20736 + 2116) = √22852 ≈ 151.17
discreteCurvature :: Double
discreteCurvature = sqrt $ fromIntegral ((144*144 + 46*46) :: Int)

----------------------------------------------------------------------
-- 3. 群论常数
----------------------------------------------------------------------

-- | A4 交替群阶 = 12
a4GroupOrder :: Word16
a4GroupOrder = 12

-- | C3 循环群阶 = 3
c3GroupOrder :: Word16
c3GroupOrder = 3

-- | I_h 正二十面体群阶 = 120
ihGroupOrder :: Word16
ihGroupOrder = 120

-- | 正十二面体转动数 = 60
dodecahedralRotations :: Word16
dodecahedralRotations = 60

----------------------------------------------------------------------
-- 4. 算术常数
----------------------------------------------------------------------

-- | 主权LCM = 3¹¹ × 2¹⁶ = 11,609,505,792
sovereignLCM :: Word64
sovereignLCM = 11609505792

-- | 因子 3¹¹ = 177147
factor3pow11 :: Word64
factor3pow11 = 177147

-- | 因子 2¹⁶ = 65536
factor2pow16 :: Word64
factor2pow16 = 65536

-- | Bézout: 13×39 = 1 + 46×11  (即 13⁻¹ ≡ 39 mod 46)
bezout13x39 :: (Int, Int)
bezout13x39 = (13 * 39, 1 + 46 * 11)  -- 507 = 507

-- | CRT 同构: ℕ/6624 → ℕ/144 × ℕ/46
--   同构映射的 Bézout 系数
-- | 144 和 46 不互质 (gcd=2), 无标准 Bézout 逆元
--   使用 CRT 查表 (Dayan.Compute.CRT.reverseLookup) 替代模逆计算
crtInv144mod46 :: Word16
crtInv144mod46 = 0  -- 标记为不适用: gcd(144,46)=2, 查表模式使用 reverseLookup

crtInv46mod144 :: Word16
crtInv46mod144 = 0  -- 标记为不适用: gcd(144,46)=2

----------------------------------------------------------------------
-- 5. 幻方常数
----------------------------------------------------------------------

-- | 亚瑟幻方 4×4 幻和 = 34
magicSum4x4 :: Word16
magicSum4x4 = 34

-- | 幻方正交基维度 = 4 (4×4 幻方空间)
magicSquareDim :: Word16
magicSquareDim = 4

-- | 幻方全排列数 = 16! = 20922789888000
magicSquarePermutations :: Word64
magicSquarePermutations = 20922789888000

----------------------------------------------------------------------
-- 6. 十二律基元
----------------------------------------------------------------------

-- | 十二律 = 12
twelveTones :: Word16
twelveTones = 12

-- | 仲吕步 = 11 (第 12 步为仲吕点)
zhonglvStep :: Word16
zhonglvStep = 11
