-- | Dayan.Algebra.GF9 — GF(3²) 的 CRT 余数向量表示
--
-- 幻方正交拓扑基石: i² + 1² = 0²
--   M4 正交本征方向: v16⁺ (右手征) ⊥ v16⁻ (左手征)
--   crtLabel e16⁺ = crtLabel e16⁻ = 16 (CRT投影相同!)
--   差异在 C3 层: crt-to-trit e16⁺ = T₁ (右旋), e16⁻ = T₂ (左旋)
--   eigen-conjugate: e16⁺ ↔ e16⁻
--
-- GF(9) 的 9 个元素 → CRT 余数向量:
--   实部 a ∈ {0,1,2} → CRT 投影 (a % 144, a % 46)
--   虚部 b ∈ {0,1,2} → C3 手征: 0=中性, 1=右旋(T₁), 2=左旋(T₂)
--   Frobenius σ(a,b)=(a,-b): 手征翻转, CRT投影不变
--   共轭对 (a,1)↔(a,2) 共享同一 CRT 坐标
--
-- 对齐 Agda: Algebra/GF9.agda, QuantumBridge §24 (crtLabel/crt-to-trit)

module Dayan.Algebra.GF9 where

import Data.Word (Word8, Word16)
import Dayan.Core.Trit (Trit(..), toNat, neg)

----------------------------------------------------------------------
-- 1. GF(9) 核心类型
----------------------------------------------------------------------

-- | GF(9) 元素: a + b·α, 其中 α² = -1
data Gf9 = Gf9 { realPart :: !Trit, imagPart :: !Trit }
  deriving (Show, Eq, Ord)

-- | 全部 9 个 GF(9) 元素
allGf9 :: [Gf9]
allGf9 = [Gf9 a b | a <- [N, Z, P], b <- [N, Z, P]]

----------------------------------------------------------------------
-- 2. GF(9) 代数运算 (对齐 x²+1=0 域定义)
----------------------------------------------------------------------

-- | Frobenius 自同构: σ(a + bα) = a + (-b)·α = a - bα
frobenius :: Gf9 -> Gf9
frobenius (Gf9 a b) = Gf9 a (neg b)

-- | GF(9) 加法
addGf9 :: Gf9 -> Gf9 -> Gf9
addGf9 (Gf9 a b) (Gf9 c d) = Gf9 (tritAdd a c) (tritAdd b d)

-- | GF(9) 乘法: (a+bα)(c+dα) = (ac-bd)+(ad+bc)α, α²=-1
mulGf9 :: Gf9 -> Gf9 -> Gf9
mulGf9 (Gf9 a b) (Gf9 c d) =
  let ac = tritMul a c; bd = tritMul b d
      ad = tritMul a d; bc = tritMul b c
  in Gf9 (tritSub ac bd) (tritAdd ad bc)

tritAdd, tritMul, tritSub :: Trit -> Trit -> Trit
tritAdd a b = fromIntegralTrit ((fromIntegral (toNat a) + fromIntegral (toNat b)) `mod` 3)
tritMul a b = fromIntegralTrit ((fromIntegral (toNat a) * fromIntegral (toNat b)) `mod` 3)
tritSub a b = tritAdd a (neg b)

fromIntegralTrit :: Int -> Trit
fromIntegralTrit 0 = N; fromIntegralTrit 1 = Z; fromIntegralTrit _ = P

----------------------------------------------------------------------
-- 3. M4 幻方正交: ±16 的 CRT 投影相同, C3 层区分
----------------------------------------------------------------------

-- | C3 手征指示
--   0 = T₀ (中性, e0)
--   1 = T₁ (右旋, e16⁺: crt-to-trit e16⁺ = T₁)
--   2 = T₂ (左旋, e16⁻: crt-to-trit e16⁻ = T₂)
chirality :: Gf9 -> Word8
chirality (Gf9 _ b) = toNat b

-- | 元素索引 (0..8): a + 3·b 基3编码, 仅用于遍历
encodeGf9 :: Gf9 -> Word16
encodeGf9 (Gf9 a b) =
  fromIntegral (toNat a) + 3 * fromIntegral (toNat b)

-- | CRT 余数向量投影
--   仅用实部 a → 对齐 crtLabel e16⁺ ≡ crtLabel e16⁻ (refl)
--   Frobenius共轭对 (a,1)↔(a,2) 投影相同 — CRT层不可区分
gf9CrtProject :: Gf9 -> (Word8, Word8)
gf9CrtProject (Gf9 a _) =
  let n = fromIntegral (toNat a)  -- 仅实部: 0,1,2
  in (n, n)  -- polar = toroidal = a

-- | 9 个 GF(9) 元素 → CRT 坐标表
gf9CrtTable :: [(Gf9, (Word8, Word8))]
gf9CrtTable = [(x, gf9CrtProject x) | x <- allGf9]

----------------------------------------------------------------------
-- 4. 验证
----------------------------------------------------------------------

-- | Frobenius 共轭对 CRT 投影相同 (对齐 crtLabel e16⁺ ≡ e16⁻)
--   共轭对仅在 C3 手征层区分, CRT 层不可区
gf9ConjugateCrtSame :: Bool
gf9ConjugateCrtSame =
  all (\x -> gf9CrtProject x == gf9CrtProject (frobenius x)) allGf9

-- | C3 手征映射: 共轭对翻转手征
--   (a,1) → (a,2): chirality 1→2 (右→左)
--   (a,2) → (a,1): chirality 2→1 (左→右)
--   (a,0) → (a,0): 自共轭 (中性)
gf9ChiralityFlip :: Bool
gf9ChiralityFlip =
  all (\x ->
    let c = chirality x; fc = chirality (frobenius x)
    in (c == 0 && fc == 0) ||          -- 中性固定
       (c == 1 && fc == 2) ||          -- 右→左
       (c == 2 && fc == 1)             -- 左→右
  ) allGf9

-- | Frobenius 平方 = 恒等
frobeniusInvolutive :: Gf9 -> Bool
frobeniusInvolutive x = frobenius (frobenius x) == x

-- | Frobenius 保加法同态
frobeniusAddHom :: Gf9 -> Gf9 -> Bool
frobeniusAddHom x y =
  frobenius (addGf9 x y) == addGf9 (frobenius x) (frobenius y)

-- | Frobenius 保乘法同态
frobeniusMulHom :: Gf9 -> Gf9 -> Bool
frobeniusMulHom x y =
  frobenius (mulGf9 x y) == mulGf9 (frobenius x) (frobenius y)
