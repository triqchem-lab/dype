-- | Dayan.Core.Tryte — Fin 729 格点 (6-trit 编码)
--
-- 语义: T⁶ 环面的一个局部格点截面
-- 状态空间: 3⁶ = 729
-- 与 Agda T6Lattice 双射一致 (基3编码)
--
-- 基3编码: v0 + 3·v1 + 9·v2 + 27·v3 + 81·v4 + 243·v5
-- 其中 vi ∈ {N=0, Z=1, P=2}
--
-- 对应 Agda:
--   encode: T6Lattice → ℕ  (v5∷v4∷v3∷v2∷v1∷v0 → ℕ)
--   decode: ℕ → T6Lattice  (ℕ → 6层 %3 + /3 剥离)

module Dayan.Core.Tryte where

import Data.Word (Word16, Word8)
import Data.Bits (shiftR, (.&.))
import Data.Maybe (isJust)

import Dayan.Core.Trit (Trit(..), toNat, fromNat)

----------------------------------------------------------------------
-- 1. 核心类型
----------------------------------------------------------------------

-- | Tryte — 6-trit 格点, 编码为 0..728 的 Word16
--
-- 内部编码: 小端基3 (v0 在 LSB)
--   index = v0 + 3*v1 + 9*v2 + 27*v3 + 81*v4 + 243*v5
--
-- 不变式: 0 ≤ index < 729
newtype Tryte = Tryte { unTryte :: Word16 }
  deriving (Show, Eq, Ord)

----------------------------------------------------------------------
-- 2. 常数
----------------------------------------------------------------------

-- | 格点总数 = 3⁶
tryteCardinality :: Word16
tryteCardinality = 729

-- | 最大有效索引
tryteMaxIndex :: Word16
tryteMaxIndex = 728

-- | 基3权重: [3⁰, 3¹, 3², 3³, 3⁴, 3⁵]
base3Weights :: [Word16]
base3Weights = [1, 3, 9, 27, 81, 243]

----------------------------------------------------------------------
-- 3. 构造与验证
----------------------------------------------------------------------

-- | 从 Word16 构造 Tryte (危险: 不验证范围)
mkTryte :: Word16 -> Tryte
mkTryte = Tryte

-- | 安全构造: 验证 0 ≤ n < 729
mkTryteSafe :: Word16 -> Maybe Tryte
mkTryteSafe n
  | n < tryteCardinality = Just (Tryte n)
  | otherwise            = Nothing

-- | 最小格点 (全 N: v0..v5 = N)
minTryte :: Tryte
minTryte = Tryte 0

-- | 最大格点 (全 P: v0..v5 = P)
maxTryte :: Tryte
maxTryte = Tryte 728

-- | 平衡点 (全 Z: v0..v5 = Z) → 1+3+9+27+81+243 = 364
balanceTryte :: Tryte
balanceTryte = Tryte 364

-- | 验证 Tryte 索引是否在有效范围内
isValid :: Tryte -> Bool
isValid (Tryte n) = n < tryteCardinality

----------------------------------------------------------------------
-- 4. 基3 编解码
----------------------------------------------------------------------

-- | 将 6 个 Trit 编码为 Tryte
--
-- > encode [v0, v1, v2, v3, v4, v5]
-- >   = v0 + 3·v1 + 9·v2 + 27·v3 + 81·v4 + 243·v5
--
-- 其中 vi 的 ℕ 编码为 N→0, Z→1, P→2
encode :: [Trit] -> Maybe Tryte
encode ts
  | length ts == 6 = Just . Tryte . go 1 $ map toNat ts
  | otherwise      = Nothing
  where
    go _ []     = 0
    go w (n:ns) = w * fromIntegral n + go (w*3) ns

-- | 将 Tryte 解码为 6 个 Trit 列表 [v0, v1, v2, v3, v4, v5]
--
-- 算法: 6 层 %3 + /3 剥离 (4320D 范式)
--   v0 = n % 3, n₁ = n / 3
--   v1 = n₁ % 3, n₂ = n₁ / 3
--   ...
--   v5 = n₅ % 3
decode :: Tryte -> [Trit]
decode (Tryte n) = go n 6
  where
    go _ 0 = []
    go m k = case fromNat (fromIntegral (m `mod` 3)) of
               Just t  -> t : go (m `div` 3) (k - 1)
               Nothing -> Z : go (m `div` 3) (k - 1)  -- 不应到达

-- | 解码为 Word8 列表 (N→0, Z→1, P→2)
decodeRaw :: Tryte -> [Word8]
decodeRaw (Tryte n) = go n 6
  where
    go _ 0 = []
    go m k = fromIntegral (m `mod` 3) : go (m `div` 3) (k - 1)

----------------------------------------------------------------------
-- 5. Trit 级访问
----------------------------------------------------------------------

-- | 读取第 i 个 Trit (0 ≤ i < 6)
--   i=0: LSB (v0), i=5: MSB (v5)
tritAt :: Tryte -> Int -> Maybe Trit
tritAt (Tryte n) i
  | i < 0 || i >= 6 = Nothing
  | otherwise       = fromNat . fromIntegral $ (n `div` (3 ^ i)) `mod` 3

-- | 设置第 i 个 Trit (0 ≤ i < 6)
setTrit :: Tryte -> Int -> Trit -> Maybe Tryte
setTrit (Tryte n) i t
  | i < 0 || i >= 6 = Nothing
  | otherwise       = Just . Tryte $
      let pow = 3 ^ i
          old = (n `div` pow) `mod` 3
          new = fromIntegral (toNat t)
      in n - old * pow + new * pow

-- | 获取所有 Trit 的 ℕ 编码列表
toNatList :: Tryte -> [Word8]
toNatList = decodeRaw

----------------------------------------------------------------------
-- 6. 枚举与遍历
----------------------------------------------------------------------

-- | 全部 729 个 Tryte
allTrytes :: [Tryte]
allTrytes = map Tryte [0..728]

-- | 遍历所有格点并应用函数
forEachTryte :: (Tryte -> IO ()) -> IO ()
forEachTryte f = mapM_ f allTrytes

-- | 在格点上查找满足谓词的第一个点
findTryte :: (Tryte -> Bool) -> Maybe Tryte
findTryte p = case filter p allTrytes of
                []    -> Nothing
                (t:_) -> Just t
