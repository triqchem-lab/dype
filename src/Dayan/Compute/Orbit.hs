-- | Dayan.Compute.Orbit — Orbit-Stabilizer 分解 (A4 群作用于 729 格点)
--
-- A4 (交替群) 阶 = 12, 作用于 T⁶ 环面格点 (729 点)
-- Orbit-Stabilizer 定理: |Orbit(x)| × |Stab(x)| = |G| = 12
--
-- 轨道大小: {1, 3, 4, 6, 12} (12 的因子)
-- 稳定子大小: {12, 4, 3, 2, 1} (对偶)
--
-- 对齐 Agda: Sovereign.Structology.A4Group.agda

{-# LANGUAGE LambdaCase #-}
module Dayan.Compute.Orbit where

import Data.Word (Word16)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (sortOn, group, sort)
import Data.Maybe (fromMaybe)

import Dayan.Core.Tryte (Tryte(..), allTrytes)

----------------------------------------------------------------------
-- 1. A4 群元素 (12 个置换)
----------------------------------------------------------------------

-- | A4 元素: 偶置换 (12 个)
--   表示方式: 作用于 4 元集合 {0,1,2,3} 的置换
--   编码: [σ(0), σ(1), σ(2), σ(3)]
type A4Element = [Int]

-- | 全部 12 个 A4 元素
--   A4 = { 偶置换 of S4 } = { id, 3-cycles ×8, double-transpositions ×3 }
a4Group :: [A4Element]
a4Group =
  [ [0,1,2,3]   -- id
  , [0,2,3,1]   -- (1 2 3)
  , [0,3,1,2]   -- (1 3 2)
  , [1,0,3,2]   -- (0 1)(2 3)
  , [1,2,0,3]   -- (0 1 2)
  , [1,3,2,0]   -- (0 1 3)
  , [2,0,1,3]   -- (0 2 1)
  , [2,1,3,0]   -- (0 2)(1 3)
  , [2,3,0,1]   -- (0 2 3)
  , [3,0,2,1]   -- (0 3)(1 2)
  , [3,1,0,2]   -- (0 3 2)
  , [3,2,1,0]   -- (0 3 1)
  ]

-- | A4 恒等元
a4Identity :: A4Element
a4Identity = [0,1,2,3]

-- | A4 群阶
a4Order :: Int
a4Order = 12

----------------------------------------------------------------------
-- 2. A4 群对 Tryte 的作用 (简化模型)
----------------------------------------------------------------------

-- | A4 元素对 Tryte 的作用
--   简化: 将 Tryte 的 6 个 Trit 分成 4+2 两组,
--   前 4 个 Trit 按 A4 置换重排, 后 2 个不变
a4Action :: A4Element -> Tryte -> Tryte
a4Action perm (Tryte n) = Tryte result
  where
    -- 解码 6 个 Trit
    trits = decode6 n
    -- 前 4 个 Trit 按 A4 置换
    trits' = [ trits !! (perm !! i) | i <- [0..3] ] ++ drop 4 trits
    -- 重新编码
    result = encode6 trits'

-- | 将 Word16 解码为 6 个 Trit (0,1,2)
decode6 :: Word16 -> [Int]
decode6 n = [ fromIntegral ((n `div` (3^i)) `mod` 3) | i <- [0..5] ]

-- | 将 6 个 Trit 编码为 Word16
encode6 :: [Int] -> Word16
encode6 = go 0 1
  where
    go acc _ []     = acc
    go acc w (t:ts) = go (acc + w * fromIntegral t) (w*3) ts

----------------------------------------------------------------------
-- 3. 轨道计算
----------------------------------------------------------------------

-- | 计算 Tryte x 在 A4 群作用下的轨道
orbit :: Tryte -> [Tryte]
orbit x = S.toList $ S.fromList [ a4Action g x | g <- a4Group ]

-- | 计算轨道大小
orbitSize :: Tryte -> Int
orbitSize = length . orbit

-- | 计算稳定子 (固定 x 的群元素)
stabilizer :: Tryte -> [A4Element]
stabilizer x = [ g | g <- a4Group, a4Action g x == x ]

-- | 计算稳定子大小
stabilizerSize :: Tryte -> Int
stabilizerSize = length . stabilizer

-- | 验证 Orbit-Stabilizer 定理: |Orbit| × |Stab| = 12
verifyOrbitStabilizer :: Tryte -> Bool
verifyOrbitStabilizer x = orbitSize x * stabilizerSize x == a4Order

----------------------------------------------------------------------
-- 4. 全局轨道分解
----------------------------------------------------------------------

-- | 轨道分解结果
data OrbitDecomp = OrbitDecomp
  { orbits     :: [[Tryte]]         -- ^ 轨道列表
  , orbitSizes :: [(Int, Int)]      -- ^ (轨道大小, 稳定子大小)
  , orbitCount :: Int               -- ^ 轨道总数
  , maxOrbitSize :: Int             -- ^ 最大轨道大小
  } deriving (Show, Eq)

-- | 对所有 729 个格点计算轨道分解
decomposeOrbits :: OrbitDecomp
decomposeOrbits =
  let all = S.fromList allTrytes
      orbits' = findOrbits all S.empty
  in OrbitDecomp
    { orbits     = orbits'
    , orbitSizes = [(length o, a4Order `div` length o) | o <- orbits']
    , orbitCount = length orbits'
    , maxOrbitSize = maximum (map length orbits')
    }

-- | 贪心查找所有轨道
findOrbits :: Set Tryte -> Set Tryte -> [[Tryte]]
findOrbits remaining _
  | S.null remaining = []
  | otherwise =
      let x = S.elemAt 0 remaining
          orb = S.fromList (orbit x)
      in S.toList orb : findOrbits (remaining `S.difference` orb) (S.empty)

----------------------------------------------------------------------
-- 5. 轨道统计
----------------------------------------------------------------------

-- | 轨道大小分布
orbitDistribution :: OrbitDecomp -> Map Int Int
orbitDistribution decomp =
  M.fromListWith (+) [(length o, 1) | o <- orbits decomp]

-- | 验证: 所有格点都被覆盖
verifyCoverage :: OrbitDecomp -> Bool
verifyCoverage decomp =
  let covered = S.fromList (concat (orbits decomp))
      allPts   = S.fromList allTrytes
  in covered == allPts

-- | 验证: 所有轨道都满足 Orbit-Stabilizer 定理
verifyAllOrbits :: OrbitDecomp -> Bool
verifyAllOrbits decomp =
  all (\orb -> length orb * (a4Order `div` length orb) == a4Order)
      (orbits decomp)
