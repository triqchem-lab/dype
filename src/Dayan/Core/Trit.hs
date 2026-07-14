-- | Dayan.Core.Trit — GF(3) 三进制基本单元
--
-- 编码与 Agda GF3.Trit 一致:
--   N (T0) = 0  →  吸收态 / 负极
--   Z (T1) = 1  →  平衡态 / 零极
--   P (T2) = 2  →  表达态 / 正极
--
-- 哲学约束 (宪法层级):
--   GF(3) 的合法身份 = 模 3 整数算术 + 驻波叠加表
--   禁止: "GF(3) 是有限域" / "三进制计算机" /
--         将模 3 算术直接用于主权状态机演化
--
-- 来源: /data/work/functional-programming/agda 参考实现
-- 对齐: Agda GF3.Trit (Sovereign.Base.Trit.agda)

module Dayan.Core.Trit where

import Data.Ix (Ix)
import Data.Word (Word8)

----------------------------------------------------------------------
-- 1. 核心类型定义
----------------------------------------------------------------------

-- | GF(3) 三进制 Trit
--
-- 三个状态的语义映射 (与 Agda 对齐):
--   N (吸收态)   →  -1  (负极性, 抵消叠加)
--   Z (平衡态)   →   0  (中性, 不参与叠加)
--   P (表达态)   →  +1  (正极性, 加强叠加)
--
-- 整数编码:
--   N → 0, Z → 1, P → 2  — 匹配 Agda GF3.Trit 的 toℕ 编码
data Trit
  = N  -- ^ 吸收态 (Absorption) — 对应 Agda 的 T0
  | Z  -- ^ 平衡态 (Balance)    — 对应 Agda 的 T1
  | P  -- ^ 表达态 (Expression) — 对应 Agda 的 T2
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Ix)

----------------------------------------------------------------------
-- 2. ℕ 编解码
----------------------------------------------------------------------

-- | Trit → 自然数编码 (N→0, Z→1, P→2)
toNat :: Trit -> Word8
toNat N = 0
toNat Z = 1
toNat P = 2

-- | 自然数编码 → Trit
--   0→N, 1→Z, 2→P, 其他→Nothing
fromNat :: Word8 -> Maybe Trit
fromNat 0 = Just N
fromNat 1 = Just Z
fromNat 2 = Just P
fromNat _ = Nothing

-- | 带默认值的解码 (越界返回 Z)
fromNatZ :: Word8 -> Trit
fromNatZ 0 = N
fromNatZ 1 = Z
fromNatZ _ = P  -- 2 及以上都视为 P (宽容模式)

-- | Trit → 有符号整数 (N→ -1, Z→ 0, P→ +1)
toInt :: Trit -> Int
toInt N = -1
toInt Z =  0
toInt P =  1

-- | 有符号整数 → Trit (-1→N, 0→Z, 1→P, 其他→Nothing)
fromInt :: Int -> Maybe Trit
fromInt (-1) = Just N
fromInt 0    = Just Z
fromInt 1    = Just P
fromInt _    = Nothing

----------------------------------------------------------------------
-- 3. 模 3 算术运算
----------------------------------------------------------------------

-- | 模 3 加法 (N=0, Z=1, P=2)
add :: Trit -> Trit -> Trit
add N N = N   -- (0+0) % 3 = 0
add N Z = Z   -- (0+1) % 3 = 1
add N P = P   -- (0+2) % 3 = 2
add Z N = Z   -- (1+0) % 3 = 1
add Z Z = P   -- (1+1) % 3 = 2
add Z P = N   -- (1+2) % 3 = 0
add P N = P   -- (2+0) % 3 = 2
add P Z = N   -- (2+1) % 3 = 0
add P P = Z   -- (2+2) % 3 = 1

-- | 模 3 乘法 (N=0, Z=1, P=2)
mul :: Trit -> Trit -> Trit
mul N _ = N   -- 0 * x = 0
mul Z x = x   -- 1 * x = x
mul P N = N   -- 2 * 0 = 0
mul P Z = P   -- 2 * 1 = 2
mul P P = Z   -- 2 * 2 = 1

-- | 模 3 取负 (加法逆元)
neg :: Trit -> Trit
neg N = N   -- neg 0 = 0
neg Z = P   -- neg 1 = 2
neg P = Z   -- neg 2 = 1

----------------------------------------------------------------------
-- 4. 驻波叠加表 (Standing Wave Superposition)
----------------------------------------------------------------------

-- | 驻波叠加 — GF(3) 的"物理层"运算
--
--   不同于纯模运算 add，驻波叠加编码了波动力学的
--   相消干涉与相长干涉:
--
--   N ⊕ Z = N  — 吸收态遇平衡态: 平衡态被吸收
--   Z ⊕ P = P  — 平衡态遇表达态: 平衡态被推开
--   N ⊕ P = Z  — 吸收态遇表达态: 对消归零
--
--   叠加表:
--     ⊕ | N  Z  P
--    ---+--------
--     N | N  N  Z
--     Z | N  Z  P
--     P | Z  P  P
superpose :: Trit -> Trit -> Trit
superpose N N = N
superpose N Z = N
superpose N P = Z
superpose Z N = N
superpose Z Z = Z
superpose Z P = P
superpose P N = Z
superpose P Z = P
superpose P P = P

-- | 驻波对消检测: N ⊕ P → Z (吸收-表达对消)
isCanceled :: Trit -> Trit -> Bool
isCanceled N P = True
isCanceled P N = True
isCanceled _ _ = False

-- | 是否为吸收态
isAbsorb :: Trit -> Bool
isAbsorb N = True
isAbsorb _ = False

-- | 是否为平衡态
isBalance :: Trit -> Bool
isBalance Z = True
isBalance _ = False

-- | 是否为表达态
isExpress :: Trit -> Bool
isExpress P = True
isExpress _ = False
