-- | Dayan.Compute.Cascade — 极限环级联 (6624 步格点巡游)
--
-- 极限环: 极向 144 步 + 环向 46 步的不可通约级联
-- 对齐点: 6624 = 144 × 46 步后相位归零
-- 不闭合: 级联永不停止, 只在对齐点坍缩
--
-- 对齐 Agda: XuanwuAbsorption cycleN-adds
--   仲吕相位同步每 12 步注入 Δφ

module Dayan.Compute.Cascade where

import Data.Word (Word16)
import Data.Maybe (fromMaybe, listToMaybe)
import Dayan.Compute.CRT (lookupPolar, lookupToroidal)
import Dayan.Core.Torus (TorusPoint(..))

----------------------------------------------------------------------
-- 1. 步进定义
----------------------------------------------------------------------

-- | 单步级联: 极向 +1 (mod 144), 环向 +1 (mod 46)
cascadeStep :: Word16 -> Word16
cascadeStep idx = (idx + 1) `mod` 6624

-- | 从格点索引提取 TorusPoint
indexToPoint :: Word16 -> TorusPoint
indexToPoint idx = TorusPoint
  { polar    = fromIntegral (lookupPolar idx)
  , toroidal = lookupToroidal idx
  }

-- | 从 TorusPoint 恢复格点索引 (通过 CRT)
pointToIndex :: TorusPoint -> Word16
pointToIndex (TorusPoint p t) =
  -- 线性搜索: p + k*144, k∈[0..45], 找 toroidal ≡ t (mod 46)
  fromMaybe 0 (listToMaybe [ idx | k <- [0..45]
         , let idx = fromIntegral p + k * 144
         , idx < 6624
         , lookupToroidal idx == t ])

----------------------------------------------------------------------
-- 2. 极限环轨迹
----------------------------------------------------------------------

-- | 从起始索引开始, 6624 步的完整极限环轨迹
--   返回: [start, step(start), step²(start), ..., step⁶⁶²³(start)]
cascade :: Word16 -> [Word16]
cascade start = take 6624 $ iterate cascadeStep start

-- | 从黄钟初始态 (polar=0, toroidal=0) 开始的轨迹
huangzhongCascade :: [Word16]
huangzhongCascade = cascade 0

-- | 轨迹的 TorusPoint 表示
cascadePoints :: Word16 -> [TorusPoint]
cascadePoints = map indexToPoint . cascade

-- | 黄钟轨迹的 TorusPoint 表示
huangzhongPoints :: [TorusPoint]
huangzhongPoints = cascadePoints 0

----------------------------------------------------------------------
-- 3. 仲吕相位同步 (每 12 步注入)
----------------------------------------------------------------------

-- | 仲吕相位同步: 在 polar=11 (第12步) 注入 Δφ
--   同步操作: polar 重置为 0, toroidal 保持但注入全局相位
--   对齐 Agda: Closure.zhonglvPhaseSyncOp
zhonglvSync :: Word16 -> Word16
zhonglvSync idx = idx + 144  -- 在 144 基线上注入额外歧义
-- 注意: 这只是一个简化模型, 真正的仲吕同步涉及 CRT 谱投影

-- | 带仲吕同步的步进: 每 12 步检查是否需要同步
cascadeWithZhonglv :: Word16 -> [Word16]
cascadeWithZhonglv = go (0 :: Int)
  where
    go steps idx
      | steps >= 6624 = []
      | lookupPolar idx == 11 && steps `mod` 12 == 11
      = let next = zhonglvSync idx
        in idx : go (steps + 1) next
      | otherwise
      = let next = cascadeStep idx
        in idx : go (steps + 1) next

----------------------------------------------------------------------
-- 4. 相位对齐检测
----------------------------------------------------------------------

-- | 是否在相位对齐点上 (polar=0 且 toroidal=0)
isAligned :: Word16 -> Bool
isAligned idx = lookupPolar idx == 0 && lookupToroidal idx == 0

-- | 找到下一个对齐点 (从当前索引开始)
nextAlignment :: Word16 -> Word16
nextAlignment idx = fromMaybe idx (listToMaybe $
  dropWhile (not . isAligned) $ iterate cascadeStep idx)

-- | 两个对齐点之间的步数应为 6624
alignmentPeriod :: Bool
alignmentPeriod =
  let a0 = nextAlignment 0
      a1 = nextAlignment (a0 + 1)
  in a1 - a0 == 6624  -- 从 a0 到 a1 正好是 6624 步
