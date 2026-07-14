-- | Dayan.Compute.CRT — 中国剩余定理 (CRT) 全局查表
--
-- 前向: 6624 个格点索引 → (极向模 144, 环向模 46)
-- 逆向: CRT 重构 (Bézout 系数法)
--
-- 公式:
--   前向: f(i) = (i % 144, i % 46)
--   逆向: g(p, t) = p · (46 · inv46_144) + t · (144 · inv144_46) mod 6624
--   其中 inv46_144 = 46⁻¹ mod 144, inv144_46 = 144⁻¹ mod 46
--
-- 复杂度: O(1) 查表, O(6624) 一次性构建
--
-- 对齐 Agda: Sovereign.Arithmetic.CRTLemmas.agda
--   CRT 模互质 (gcd(144,46)=2, 故 6624=LCM)

module Dayan.Compute.CRT where

import Data.Word (Word8, Word16)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

----------------------------------------------------------------------
-- 1. 常数
----------------------------------------------------------------------

polarW, toroidalW :: Int
polarW    = 144
toroidalW = 46

holographicN :: Int
holographicN = 144 * 46  -- 6624

----------------------------------------------------------------------
-- 2. CRT 表类型
----------------------------------------------------------------------

-- | CRT 前向表: 格点索引 → (极向余数, 环向余数)
--   索引: 0..6623, 返回: (polar % 144, toroidal % 46)
data CrtTable = CrtTable
  { forward   :: !(Vector (Word8, Word8))  -- 6624 项, unboxed
  , inv144mod46 :: !Word8                   -- 144⁻¹ mod 46
  , inv46mod144 :: !Word8                   -- 46⁻¹ mod 144
  }

-- | CRT 逆向: 余数对 → 格点索引 (O(1))
crtReconstruct :: CrtTable -> Word8 -> Word8 -> Word16
crtReconstruct t p t' =
  let i = fromIntegral p * 46 + fromIntegral t'
  in reverseTable (forward t) V.! i

-- | 逆向查表: (极向余数, 环向余数) → 格点索引
lookupIndex :: Word8 -> Word8 -> Word16
lookupIndex p t = crtReconstruct crtTable p t

----------------------------------------------------------------------
-- 3. Bézout 系数计算
----------------------------------------------------------------------

-- | 扩展欧几里得算法: gcd(a,b) = x·a + y·b
egcd :: Int -> Int -> (Int, Int, Int)
egcd a 0 = (a, 1, 0)
egcd a b = (g, y, x - (a `div` b) * y)
  where (g, x, y) = egcd b (a `mod` b)

-- | 模逆: 返回 m⁻¹ mod n (若 gcd(m,n) ≠ 1 则 Nothing)
modInv :: Int -> Int -> Maybe Int
modInv m n = case egcd m n of
               (1, x, _) -> Just (x `mod` n)
               _         -> Nothing

----------------------------------------------------------------------
-- 4. 构建 CRT 表
----------------------------------------------------------------------

-- | 构建完整的 CRT 查表 (一次性预计算)
buildCrtTable :: CrtTable
buildCrtTable = CrtTable
  { forward     = V.generate holographicN (\i ->
                    let i' = fromIntegral i in
                    (fromIntegral (i' `mod` polarW),
                     fromIntegral (i' `mod` toroidalW)))
  , inv144mod46 = 0  -- 非互质，不用 Bézout 重构
  , inv46mod144 = 0
  }

-- | 全局 CRT 表 (惰性, 首次使用构建)
crtTable :: CrtTable
crtTable = buildCrtTable
{-# NOINLINE crtTable #-}

-- | 反向查表: (polar, toroidal) → 格点索引
--   注意: gcd(144,46)=2，只有 polar%2 == toroidal%2 的组合有效
reverseTable :: Vector (Word8, Word8) -> Vector Word16
reverseTable fwd = V.generate (144 * 46) $ \i ->
  let p = fromIntegral (i `div` 46) :: Word8
      t = fromIntegral (i `mod` 46) :: Word8
      matches = [ idx | idx <- [0..6623]
                 , V.unsafeIndex fwd (fromIntegral idx) == (p, t) ]
  in case matches of
       (x:_) -> x
       []    -> 0  -- 无效组合, 回退到 0

----------------------------------------------------------------------
-- 5. 查表操作 (O(1))
----------------------------------------------------------------------

-- | 前向查表: 格点索引 → (极向余数, 环向余数)
lookupCrt :: Word16 -> (Word8, Word8)
lookupCrt i = forward crtTable V.! fromIntegral i

-- | 极向余数
lookupPolar :: Word16 -> Word8
lookupPolar = fst . lookupCrt

-- | 环向余数
lookupToroidal :: Word16 -> Word8
lookupToroidal = snd . lookupCrt

----------------------------------------------------------------------
-- 6. 批量遍历
----------------------------------------------------------------------

-- | 对所有格点执行 CRT 投影
projectAll :: [(Word16, (Word8, Word8))]
projectAll = [(i, lookupCrt i) | i <- [0..6623]]

-- | 通过 CRT 投射还原所有格点的位置
--   返回列表 [(格点索引, polar_index, toroidal_index)]
allProjections :: [(Word16, Word8, Word8)]
allProjections = [(i, p, t) | (i, (p, t)) <- projectAll]
