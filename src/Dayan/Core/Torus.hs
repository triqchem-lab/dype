-- | Dayan.Core.Torus — 离散环面 T⁶ 坐标
--
-- 极向缠绕 (Polar Winding):   Fin 144 = 12 律 × 12 仲吕周期
-- 环向缠绕 (Toroidal Winding): Fin 46  = 全局共振周期
-- 全息投影: 144/46 不可通约 → 极限环 ν = ∥·∥₂/√3
--
-- 格点总数: 144 × 46 = 6624 (相位对齐遍历步数)
--
-- 对应 Agda: Sovereign.Structology.Winding
--   PolarWinding = Fin 144
--   ToroidalWinding = Fin 46

module Dayan.Core.Torus where

import Data.Word (Word16, Word8)


----------------------------------------------------------------------
-- 1. 核心类型
----------------------------------------------------------------------

-- | 离散环面 T⁶ 上的一个截面点
--
-- 极向分量: 局部时间流逝 (12 律 × 12 周期 = 144 步)
-- 环向分量: 全局空间共振 (46 周期)
data TorusPoint = TorusPoint
  { polar    :: !Word16   -- ^ 极向坐标 (Fin 144: 0..143)
  , toroidal :: !Word8    -- ^ 环向坐标 (Fin 46:  0..45)
  } deriving (Show, Eq, Ord)

----------------------------------------------------------------------
-- 2. 常数
----------------------------------------------------------------------

-- | 极向缠绕数 = 144
polarWinding :: Word16
polarWinding = 144

-- | 环向缠绕数 = 46
toroidalWinding :: Word8
toroidalWinding = 46

-- | 全息格点总数 = 144 × 46 = 6624
holographicCardinality :: Int
holographicCardinality = 144 * 46  -- = 6624

-- | [DEPRECATED] 不可通约比 144/46 (使用 Constants.holographicRatio)
{-# DEPRECATED holographicRatio "使用 Constants.holographicRatio (纯整数对)" #-}
holographicRatio :: Double
holographicRatio = 144.0 / 46.0

----------------------------------------------------------------------
-- 3. 构造与验证
----------------------------------------------------------------------

-- | 构造环面点
mkTorusPoint :: Word16 -> Word8 -> Maybe TorusPoint
mkTorusPoint p t
  | p < polarWinding && t < toroidalWinding = Just (TorusPoint p t)
  | otherwise                               = Nothing

-- | 构造环面点 (不验证)
mkTorusPointUnsafe :: Word16 -> Word8 -> TorusPoint
mkTorusPointUnsafe = TorusPoint

-- | 黄钟初始态: polar=0, toroidal=0
huangzhong :: TorusPoint
huangzhong = TorusPoint 0 0

-- | 验证环面点坐标是否在有效范围内
isValid :: TorusPoint -> Bool
isValid (TorusPoint p t) = p < polarWinding && t < toroidalWinding

----------------------------------------------------------------------
-- 4. 步进 (step)
----------------------------------------------------------------------

-- | 极向步进: 向前推进一步 (mod 144)
--
-- 对应 Agda: stepPolar (Fin 12 → Fin 12 的环绕)
-- 语义: 12 步一个完整律周期, 12×12=144 步完成完整极向循环
stepPolar :: Word16 -> Word16
stepPolar p = (p + 1) `mod` polarWinding

-- | 环向步进: 向前推进一步 (mod 46)
stepToroidal :: Word8 -> Word8
stepToroidal t = (t + 1) `mod` toroidalWinding

-- | 完整步进: 极向 + 环向同时推进
step :: TorusPoint -> TorusPoint
step (TorusPoint p t) = TorusPoint (stepPolar p) (stepToroidal t)

-- | 步进 n 次
stepN :: Int -> TorusPoint -> TorusPoint
stepN 0 tp = tp
stepN n tp = stepN (n - 1) (step tp)

-- | 完整极向周期 (144 步)
stepFullPolar :: TorusPoint -> TorusPoint
stepFullPolar = stepN (fromIntegral polarWinding)

-- | 完整环向周期 (46 步)
stepFullToroidal :: TorusPoint -> TorusPoint
stepFullToroidal = stepN (fromIntegral toroidalWinding)

----------------------------------------------------------------------
-- 5. 相位信息
----------------------------------------------------------------------

-- | 离散极向相位: Word16 值 [0, 143] 直接作为相位索引
--   替代连续弧度的纯离散表示
discretePolarPhase :: TorusPoint -> Word16
discretePolarPhase = polar

-- | 离散环向相位: Word8 值 [0, 45] 直接作为相位索引
discreteToroidalPhase :: TorusPoint -> Word8
discreteToroidalPhase = toroidal

-- | [DEPRECATED: 连续统污染] 极向相位角 (弧度): p / 144 × 2π
--   使用 discretePolarPhase 替代
{-# DEPRECATED polarPhase "使用 discretePolarPhase (纯离散 Word16 相位索引)" #-}
polarPhase :: TorusPoint -> Double
polarPhase tp = fromIntegral (polar tp) / 144.0 * 2 * pi

-- | [DEPRECATED: 连续统污染] 环向相位角: t / 46 × 2π
--   使用 discreteToroidalPhase 替代
{-# DEPRECATED toroidalPhase "使用 discreteToroidalPhase (纯离散 Word8 相位索引)" #-}
toroidalPhase :: TorusPoint -> Double
toroidalPhase tp = fromIntegral (toroidal tp) / 46.0 * 2 * pi

-- | [DEPRECATED: 连续统污染] 组合相位差 (极向 - 环向)
--   使用 discretePolarPhase / discreteToroidalPhase 替代
{-# DEPRECATED phaseDifference "使用 discretePolarPhase/discreteToroidalPhase" #-}
phaseDifference :: TorusPoint -> Double
phaseDifference tp = polarPhase tp - toroidalPhase tp

-- | 是否在极限环对齐点上 (相位差 ≡ 0 mod 2π 的对齐边界)
isAligned :: TorusPoint -> Bool
isAligned tp = polar tp == 0 && toroidal tp == 0

----------------------------------------------------------------------
-- 6. 遍历
----------------------------------------------------------------------

-- | 全部 6624 个环面格点 (按极向→环向顺序)
allPoints :: [TorusPoint]
allPoints = [ TorusPoint p t | p <- [0..143], t <- [0..45] ]

-- | 极限环轨迹: 从初始点开始, 步进直到返回起点
--   返回 [start, step(start), step(step(start)), ...]
--   在 6624 步处回到 start (相位对齐)
trajectory :: TorusPoint -> [TorusPoint]
trajectory start = take holographicCardinality $ iterate step start

-- | 极向缠绕数 - 环向缠绕数的不可通约验证:
--   gcd(144, 46) = 2, 所以 6624 步完成最小公倍周期
gcdPolarToroidal :: Int
gcdPolarToroidal = 2  -- gcd(144, 46) = 2
