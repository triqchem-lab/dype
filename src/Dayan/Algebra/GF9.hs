-- | Dayan.Algebra.GF9 — GF(3²) 的 CRT 余数向量表示
--
-- 幻方正交拓扑基石: i² + 1² = 0²
--   M4 正交本征方向: v16⁺ (右手征) ⊥ v16⁻ (左手征)
--   16² ≡ 40 ≡ (2√10)² (mod 216)
--   16%3 = 1 (= C3 正向), -16%3 = 2 (= C3 反向)
--
-- GF(9) 的 9 个元素映射到 CRT 域 (Z/144 × Z/46) 的 9 个坐标:
--   a + b·α → (a·M₊ + b·M₋) 投影到 (polar, toroidal)
--   其中 M₊ = 16 (右手征方向), M₋ = 补方向
--   Frobenius σ: b → -b → ±16 手征交换
--
-- 对齐 Agda: Algebra/GF9.agda (代数定义), QuantumBridge §24 (GF9⁶→4D CRT)

module Dayan.Algebra.GF9 where

import Data.Word (Word8, Word16)
import Dayan.Core.Trit (Trit(..), toNat, neg)

----------------------------------------------------------------------
-- 1. GF(9) 核心类型
----------------------------------------------------------------------

-- | GF(9) 元素: a + b·α, 其中 α² = -1
--   CRT 域表示: y 对 (Trit, Trit) 共有 9 个元素
data Gf9 = Gf9 { realPart :: !Trit, imagPart :: !Trit }
  deriving (Show, Eq, Ord)

-- | 全部 9 个 GF(9) 元素
allGf9 :: [Gf9]
allGf9 = [Gf9 a b | a <- [N, Z, P], b <- [N, Z, P]]

----------------------------------------------------------------------
-- 2. GF(9) 代数运算 (对齐 x²+1=0 域定义)
----------------------------------------------------------------------

-- | Frobenius 自同构: σ(a + bα) = a + (-b)·α = a - bα
--   Gal(GF(9)/GF(3)) ≅ C₂, σ² = id
frobenius :: Gf9 -> Gf9
frobenius (Gf9 a b) = Gf9 a (neg b)

-- | GF(9) 加法: (a+bα) + (c+dα) = (a⊕c) + (b⊕d)α
addGf9 :: Gf9 -> Gf9 -> Gf9
addGf9 (Gf9 a b) (Gf9 c d) = Gf9 (tritAdd a c) (tritAdd b d)

-- | GF(9) 乘法: (a+bα)(c+dα) = (ac-bd) + (ad+bc)α, α²=-1
mulGf9 :: Gf9 -> Gf9 -> Gf9
mulGf9 (Gf9 a b) (Gf9 c d) =
  let ac = tritMul a c; bd = tritMul b d
      ad = tritMul a d; bc = tritMul b c
  in Gf9 (tritSub ac bd) (tritAdd ad bc)

-- 内部: GF(3) 辅助运算
tritAdd, tritMul, tritSub :: Trit -> Trit -> Trit
tritAdd a b = fromIntegralTrit ((fromIntegral (toNat a) + fromIntegral (toNat b)) `mod` 3)
tritMul a b = fromIntegralTrit ((fromIntegral (toNat a) * fromIntegral (toNat b)) `mod` 3)
tritSub a b = tritAdd a (neg b)

fromIntegralTrit :: Int -> Trit
fromIntegralTrit 0 = N; fromIntegralTrit 1 = Z; fromIntegralTrit _ = P

----------------------------------------------------------------------
-- 3. M4 幻方正交常量
----------------------------------------------------------------------

-- | 右手征投影权: 16 (M4 正手征本征值 CRT 投影)
--   16%3 = 1 ≡ C3 正向, 16² ≡ 40 ≡ (2√10)² (mod 216)
chiralWeight :: Word16
chiralWeight = 16

-- | GF(3) 基编码权: 实部权重 1, 虚部权重 chiralWeight
--   9个元素 → 9个不同的 CRT 下标值
encodeGf9 :: Gf9 -> Word16
encodeGf9 (Gf9 a b) =
  fromIntegral (toNat a) + chiralWeight * fromIntegral (toNat b)

----------------------------------------------------------------------
-- 4. CRT 余数向量投影
----------------------------------------------------------------------

-- | GF(9) 元素 → CRT 余数向量 (polar, toroidal)
--   polar  = encodeGf9(x) mod 144
--   toroidal = encodeGf9(x) mod 46
--   Frobenius σ: b → -b → 虚部权重改变 → ±16 手征交换
gf9CrtProject :: Gf9 -> (Word8, Word8)
gf9CrtProject x =
  let n = encodeGf9 x
  in (fromIntegral (n `rem` 144), fromIntegral (n `rem` 46))

-- | 9 个 GF(9) 元素 → 9 个 CRT 坐标 (验证内射性)
gf9CrtTable :: [(Gf9, (Word8, Word8))]
gf9CrtTable = [(x, gf9CrtProject x) | x <- allGf9]

-- | 验证: CRT 投影是单射 (9 元素 → 9 不同坐标)
gf9CrtInjective :: Bool
gf9CrtInjective =
  let coords = map (gf9CrtProject) allGf9
  in length (nubOrd coords) == 9

-- 内部: Ord 去重
nubOrd :: Ord a => [a] -> [a]
nubOrd = go []
  where go _ [] = []
        go seen (x:xs)
          | x `elem` seen = go seen xs
          | otherwise = x : go (x:seen) xs

----------------------------------------------------------------------
-- 5. Frobenius 共轭 ↔ M4 ±16 手征
----------------------------------------------------------------------

-- | Frobenius 共轭的 CRT 变换:
--   σ(a,b) = (a, -b) → CRT 投影中虚部方向取反
--   对应 M4 中 +16 ↔ -16 的本征值交换
frobeniusCrt :: (Word8, Word8) -> (Word8, Word8)
frobeniusCrt (p, t) = gf9CrtProject (frobenius (crtToGf9 (p, t)))

-- | CRT 坐标 → GF(9) 元素 (查表逆向, 仅用于验证)
crtToGf9 :: (Word8, Word8) -> Gf9
crtToGf9 coord =
  case [(x, c) | (x, c) <- gf9CrtTable, c == coord] of
    ((x,_):_) -> x
    _         -> Gf9 N N  -- 无效坐标, 回退原點

----------------------------------------------------------------------
-- 6. 幻方正交拓扑验证
----------------------------------------------------------------------

-- | M4 正交基石: i² + 1² = 0²
--   i = α (GF(9) 生成元), 1 = T₁ (单位元)
--   在 GF(3) 中: α² = -1 = 2, 1² = 1
--   2 + 1 = 3 ≡ 0 ✓
--   在 CRT 域: α 的 CRT 投影 (16%144,16%46) = (16,16)
--   1 的 CRT 投影 = (1,1)
--   内积 ⟨(16,16), (1,1)⟩? 不 — 这不是向量內积。
--
--   正确的 M4 正交含义: v16⁺ ⊥ v16⁻
--   在 GF(9) 中: 元素 x 和 σ(x) 构成共轭对
--   CRT 投影中: gf9CrtProject(x) 和 gf9CrtProject(σ(x)) 关于 16-轴 对称
magicSquareOrthogonality :: Gf9 -> Bool
magicSquareOrthogonality x =
  let (p1, t1) = gf9CrtProject x
      (p2, t2) = gf9CrtProject (frobenius x)
  in (p1 - p2) `rem` 144 == 0 && (t1 - t2) `rem` 46 == 0
  -- 如果实部 a ≠ 0, 共轭对投影不同 (非平凡手征)
  -- 如果 a = 0, 共轭 = 自身 (固定点)

-- | 验证: Frobenius 平方 = 恒等 (C₂ 群)
frobeniusInvolutive :: Gf9 -> Bool
frobeniusInvolutive x = frobenius (frobenius x) == x

-- | 验证: Frobenius 是域自同构 (保加法和乘法)
frobeniusAddHom :: Gf9 -> Gf9 -> Bool
frobeniusAddHom x y =
  frobenius (addGf9 x y) == addGf9 (frobenius x) (frobenius y)

frobeniusMulHom :: Gf9 -> Gf9 -> Bool
frobeniusMulHom x y =
  frobenius (mulGf9 x y) == mulGf9 (frobenius x) (frobenius y)
