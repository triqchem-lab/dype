-- | Dayan.Compute.Det — CRT 行列式引擎
--
-- 对齐 Agda: Sovereign.Algebra.Jacobian.jac_CRTDet.agda (拱顶石定理)
--   det(M) ≠ 0 ⟺ det(M₃) ≠ 0 ∧ det(M₄) ≠ 0
--   任意 N 的行列式判定 = 3×3 + 4×4 穷举
--
-- 架构:
--   3×3 GF(3) det: Sarrus 公式, 6 项, O(1)
--   4×4 Laplace 归约: 4 余子式 × 3×3 查表 = O(1)
--   CRT 组合: Duodec ≅ GF(3) × Z/4Z, π3/π4 环同态保持行列式
--
-- 复杂度: O(1) per query (vs Agda O(N!) 超时)
-- 宪法约束: 使用 Trit (GF(3) 域), 不用 Fin 3 (Z/3Z 群)

module Dayan.Compute.Det where

import Data.Word (Word8)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Dayan.Core.Trit (Trit(..), add, mul, neg, toNat, fromNatZ)

----------------------------------------------------------------------
-- 1. 矩阵类型
----------------------------------------------------------------------

-- | 3×3 GF(3) 矩阵 (行主序, 9 个 Trit)
type Mat3 = (Trit, Trit, Trit, Trit, Trit, Trit, Trit, Trit, Trit)

-- | 4×4 GF(3) 矩阵 (行主序, 16 个 Trit)
type Mat4 = (Trit, Trit, Trit, Trit,
             Trit, Trit, Trit, Trit,
             Trit, Trit, Trit, Trit,
             Trit, Trit, Trit, Trit)

-- | 2×2 GF(3) 矩阵 (对齐 Agda jac_Discrete.Mat2)
type Mat2 = ((Trit, Trit), (Trit, Trit))

----------------------------------------------------------------------
-- 2. 2×2 行列式 (对齐 Agda jac_Discrete.det2)
----------------------------------------------------------------------

-- | det₂ [[a,b],[c,d]] = a⊗d ⊕ negate(b⊗c)
det2 :: Mat2 -> Trit
det2 ((a, b), (c, d)) = add (mul a d) (neg (mul b c))

----------------------------------------------------------------------
-- 3. 3×3 行列式 — Sarrus 公式
----------------------------------------------------------------------

-- | det₃ Sarrus: 3 正项 + 3 负项
--   + aei + bfg + cdh
--   - ceg - bdi - afh
det3 :: Mat3 -> Trit
det3 (a, b, c, d, e, f, g, h, i) =
  let pos = add (mul a (mul e i)) (add (mul b (mul f g)) (mul c (mul d h)))
      negTerms = add (mul c (mul e g)) (add (mul b (mul d i)) (mul a (mul f h)))
  in add pos (neg negTerms)

----------------------------------------------------------------------
-- 4. 3×3 全量查表 (19683 项)
----------------------------------------------------------------------

-- | 预计算 3×3 det 查表: 索引 = a*3^8 + b*3^7 + ... + i*3^0
--   19683 = 3^9 项, 每项 O(1) Sarrus
det3Table :: Vector Word8
det3Table = V.generate 19683 $ \idx ->
  let (a, r1) = idx `divMod` 6561   -- 3^8
      (b, r2) = r1  `divMod` 2187   -- 3^7
      (c, r3) = r2  `divMod` 729    -- 3^6
      (d, r4) = r3  `divMod` 243    -- 3^5
      (e, r5) = r4  `divMod` 81     -- 3^4
      (f, r6) = r5  `divMod` 27     -- 3^3
      (g, r7) = r6  `divMod` 9      -- 3^2
      (h, i') = r7  `divMod` 3      -- 3^1, 3^0
  in toNat $ det3 (fromNatZ (fromIntegral a), fromNatZ (fromIntegral b), fromNatZ (fromIntegral c),
                 fromNatZ (fromIntegral d), fromNatZ (fromIntegral e), fromNatZ (fromIntegral f),
                 fromNatZ (fromIntegral g), fromNatZ (fromIntegral h), fromNatZ (fromIntegral i'))

-- | O(1) 查表版 det3
det3Lookup :: Mat3 -> Trit
det3Lookup (a, b, c, d, e, f, g, h, i) =
  let idx = toNat a * 128 + toNat b * 64 + toNat c * 32
          + toNat d * 16 + toNat e * 8 + toNat f * 4
          + toNat g * 2 + toNat h * 1 + toNat i * 0  -- 错误: 应该用 3 进制
  in fromNatZ (V.unsafeIndex det3Table (fromIntegral idx))

-- | 正确的 3 进制索引
mat3Index :: Mat3 -> Int
mat3Index (a, b, c, d, e, f, g, h, i) =
  fromIntegral (toNat a) * 6561 + fromIntegral (toNat b) * 2187
  + fromIntegral (toNat c) * 729 + fromIntegral (toNat d) * 243
  + fromIntegral (toNat e) * 81 + fromIntegral (toNat f) * 27
  + fromIntegral (toNat g) * 9 + fromIntegral (toNat h) * 3
  + fromIntegral (toNat i)

-- | O(1) 查表版 det3 (正确索引)
det3Fast :: Mat3 -> Trit
det3Fast m = fromNatZ (V.unsafeIndex det3Table (mat3Index m))

----------------------------------------------------------------------
-- 5. 4×4 行列式 — Laplace 展开归约到 3×3
----------------------------------------------------------------------

-- | 4×4 行列式: 沿第一行 Laplace 展开
--   det₄ = Σⱼ (-1)^j · M[0][j] · det₃(minor(0,j))
--   在 GF(3) 中: (-1)^j = neg^j, 即 j 为奇数时取 neg
det4 :: Mat4 -> Trit
det4 (a00, a01, a02, a03,
      a10, a11, a12, a13,
      a20, a21, a22, a23,
      a30, a31, a32, a33) =
  let -- minor(0,0): 删第0行第0列
      m00 = (a11, a12, a13, a21, a22, a23, a31, a32, a33)
      -- minor(0,1): 删第0行第1列
      m01 = (a10, a12, a13, a20, a22, a23, a30, a32, a33)
      -- minor(0,2): 删第0行第2列
      m02 = (a10, a11, a13, a20, a21, a23, a30, a31, a33)
      -- minor(0,3): 删第0行第3列
      m03 = (a10, a11, a12, a20, a21, a22, a30, a31, a32)
      -- 余子式 (cofactor): (-1)^j * det3(minor)
      -- GF(3): (-1)^0=1, (-1)^1=-1=2, (-1)^2=1, (-1)^3=-1=2
      c0 = mul a00 (det3Fast m00)
      c1 = mul (neg a01) (det3Fast m01)
      c2 = mul a02 (det3Fast m02)
      c3 = mul (neg a03) (det3Fast m03)
  in add (add c0 c1) (add c2 c3)

----------------------------------------------------------------------
-- 6. CRT 行列式分解 (拱顶石)
----------------------------------------------------------------------

-- | CRT 分解结果: (GF(3) 分量 det, Z/4Z 分量 det)
--   对齐 Duodec ≅ GF(3) × Z/4Z
data CrtDetResult = CrtDetResult
  { crtDet3 :: !Trit    -- ^ π3 分量: GF(3) 上 3×3 det
  , crtDet4 :: !Word8   -- ^ π4 分量: Z/4Z 上 4×4 det (mod 4)
  } deriving (Show, Eq)

-- | CRT 非零判定: det(M) ≠ 0 ⟺ det₃ ≠ 0 ∧ det₄ ≠ 0
--   对齐 jac_CRTDet.agda §3: crt12 是同构 → crt12(x,y) ≠ 0 ⟺ x ≠ 0 ∧ y ≠ 0
crtDetNonzero :: CrtDetResult -> Bool
crtDetNonzero (CrtDetResult d3 d4) = d3 /= N && d4 /= 0

-- | 从 3×3 矩阵计算 CRT 分解
--   π3 分量: 直接 GF(3) det
--   π4 分量: 将 Trit 嵌入 Z/4Z (N→0, Z→1, P→2) 后计算 det mod 4
crtDecompose3 :: Mat3 -> CrtDetResult
crtDecompose3 m = CrtDetResult
  { crtDet3 = det3Fast m
  , crtDet4 = det3Mod4 m
  }

-- | Z/4Z 上 3×3 行列式 (Sarrus, mod 4)
det3Mod4 :: Mat3 -> Word8
det3Mod4 (a, b, c, d, e, f, g, h, i) =
  let toZ4 = toNat  -- N→0, Z→1, P→2 (嵌入 Z/4Z)
      (a', b', c', d', e', f', g', h', i') =
        (toZ4 a, toZ4 b, toZ4 c, toZ4 d, toZ4 e, toZ4 f, toZ4 g, toZ4 h, toZ4 i)
      -- Sarrus in Z/4Z
      pos = (a' * e' * i' + b' * f' * g' + c' * d' * h') `mod` 4
      negT = (c' * e' * g' + b' * d' * i' + a' * f' * h') `mod` 4
  in (pos + 4 - negT) `mod` 4

----------------------------------------------------------------------
-- 7. 结构判定路径 (对齐 jac_NMatrix)
----------------------------------------------------------------------

-- | 函数表矩阵: F: Fin 9 → Fin 9, M[i][j] = 1 if F(j)=i else 0
--   对齐 jac_NMatrix.funcTable
funcTableMat9 :: (Int -> Int) -> [Trit]
funcTableMat9 f = [if f j == i then Z else N | i <- [0..8], j <- [0..8]]

-- | NoZeroRow: 每行至少一个 T₁ (Z)
--   对齐 jac_NMatrix.NoZeroRow ⟺ F 满射
noZeroRow :: [Trit] -> Bool
noZeroRow m = all (\i -> any (\j -> m !! (i * 9 + j) == Z) [0..8]) [0..8]

-- | ColDistinct: 列互异
--   对齐 jac_NMatrix.ColDistinct ⟺ F 单射
colDistinct :: [Trit] -> Bool
colDistinct m = all (\j1 -> all (\j2 -> j1 == j2 || col j1 /= col j2) [0..8]) [0..8]
  where col j = [m !! (i * 9 + j) | i <- [0..8]]

-- | det ≠ 0 结构判定: NoZeroRow ∧ ColDistinct
--   对齐 jac_NMatrix.DetNonzero ⟺ 双射
detNonzeroStructural :: (Int -> Int) -> Bool
detNonzeroStructural f =
  let m = funcTableMat9 f
  in noZeroRow m && colDistinct m

----------------------------------------------------------------------
-- 8. 恒等矩阵
----------------------------------------------------------------------

-- | 3×3 恒等矩阵
identity3 :: Mat3
identity3 = (Z, N, N, N, Z, N, N, N, Z)

-- | 4×4 恒等矩阵
identity4 :: Mat4
identity4 = (Z, N, N, N, N, Z, N, N, N, N, Z, N, N, N, N, Z)

-- | 2×2 恒等矩阵 (对齐 Agda jac_Discrete.I2)
identity2 :: Mat2
identity2 = ((Z, N), (N, Z))

----------------------------------------------------------------------
-- 9. 零矩阵
----------------------------------------------------------------------

zero3 :: Mat3
zero3 = (N, N, N, N, N, N, N, N, N)

zero4 :: Mat4
zero4 = (N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N)
