-- | Dayan.Compute.ModArith — 快速模算术 (mod 3/12/46/144)
--
-- 4320D 范式核心: %3 + /3 剥离操作
-- 对齐 Agda: T6.agda [4320D-migration]
--   [m+kn]%n≡m%n  — 模一步归约
--   div3-add      — 除3加法

module Dayan.Compute.ModArith where

import Data.Word (Word8, Word16)

----------------------------------------------------------------------
-- 1. mod 3 (GF(3) 基本操作)
----------------------------------------------------------------------

-- | mod 3 (O(1), 对应 Agda 的 _% 3)
mod3 :: Word16 -> Word8
mod3 n = fromIntegral (n `mod` 3)

-- | div 3 (O(1), 对应 Agda 的 _/ℕ3)
div3 :: Word16 -> Word16
div3 n = n `div` 3

-- | 3*k 的模运算: (3*k) % 3 = 0 (对应 Agda 的 mod3k rewrite)
mod3k :: Word16 -> Word8
mod3k _ = 0

-- | 3*k 的除运算: (3*k)/3 = k (对应 Agda 的 div3k rewrite)
div3k :: Word16 -> Word16
div3k k = k  -- 因为 (3*k)/3 = k

-- | 基3 6层剥离 (4320D 范式)
--   输入: ℕ 值 → 输出: [v0, v1, v2, v3, v4, v5] (每位 ∈ {0,1,2})
unpack3 :: Word16 -> [Word8]
unpack3 0 = replicate 6 0
unpack3 n = go n 6
  where
    go _ 0 = []
    go m k = fromIntegral (m `mod` 3) : go (m `div` 3) (k - 1)

-- | 基3 6层打包
--   [v0, v1, v2, v3, v4, v5] → v0 + 3·v1 + 9·v2 + 27·v3 + 81·v4 + 243·v5
pack3 :: [Word8] -> Word16
pack3 = go 1
  where
    go _ []     = 0
    go w (n:ns) = w * fromIntegral n + go (w * 3) ns

----------------------------------------------------------------------
-- 2. mod 12 (十二律周期)
----------------------------------------------------------------------

-- | mod 12: 十二律内定位
mod12 :: Word16 -> Word8
mod12 n = fromIntegral (n `mod` 12)

-- | 是否为仲吕点 (step 11, 即第 12 步)
isZhonglv :: Word8 -> Bool
isZhonglv n = n == 11

----------------------------------------------------------------------
-- 3. mod 46 (环向共振周期)
----------------------------------------------------------------------

-- | mod 46
mod46 :: Word16 -> Word8
mod46 n = fromIntegral (n `mod` 46)

-- | 是否在被 46 整除 (全息对齐线)
isMultipleOf46 :: Word16 -> Bool
isMultipleOf46 n = n `mod` 46 == 0

----------------------------------------------------------------------
-- 4. mod 144 (极向缠绕数)
----------------------------------------------------------------------

-- | mod 144
mod144 :: Word16 -> Word8
mod144 n = fromIntegral (n `mod` 144)

----------------------------------------------------------------------
-- 5. 4320D 剥离公式 (对齐 Agda [4320D-migration])
----------------------------------------------------------------------

-- | [m+kn]%n ≡ m%n — 模一步归约
--   对齐 Agda: Data.Nat.DivMod.[m+kn]%n≡m%n
modAddReduce :: Word16 -> Word16 -> Word16 -> Word16
modAddReduce m k n = (m + k * n) `mod` n  -- = m `mod` n

-- | div3 加法公式: (m + 3*k) / 3 = m/3 + k
--   对齐 Agda: div3-add
div3Add :: Word16 -> Word16 -> Word16
div3Add m k = (m `div` 3) + k
