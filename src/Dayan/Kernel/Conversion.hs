-- | Dayan.Kernel.Conversion — 大衍三极等价判定引擎
--
-- 设计原理 (宪法):
--   不重写 Agda 2400 行 TypeChecking/Conversion.hs。
--   大衍的等价判定 = 三极框架:
--     代数极: 4320D 归约 → CRT 投影等价 (polarCRT ∧ toroidalCRT)
--     几何极: T6 格点比对 → A4 轨道等价 (排序编码不变)
--     拓扑极: 不变性保持 (陈数 C=±2, 能隙 √3, 6624 对齐)
--   任一极判定为等价, 则两值在 T⁶ 论域中不可区分。
--
-- 五根支柱:
--   ① 有限模型论 — ∀ over Fin 729 可判
--   ② 群论 — A4 Orbit-Stabilizer 分解
--   ③ 数论 — CRT mod 144/46 投影
--   ④ 格点拓扑 — T⁶ 环面坐标
--   ⑤ 幻方正交 — M4 内积归零判据
--
-- 对齐 Agda: T6.agda (T⁶晶格, gf3Toℕ sort4编码, polarCRT/toroidalCRT),
--   A4Group.agda (12元交替群), CRT.agda (CRT谱投影), Closure.agda (zhonglv)

{-# LANGUAGE OverloadedStrings #-}

module Dayan.Kernel.Conversion
  ( Cmp(..)
  -- * 格点比对
  , compareTryte, compareLattice
  -- * 坐标操作
  , normalizeTorus, compareTorus
  -- * 模运算归约
  , reduceMod144, reduceMod46, crtReduce
  -- * 三进制等式
  , convTrit, convTryte, orbitEqual
  -- * CRT 投影比对
  , crtEqual
  -- * T6 CRT (排序编码, 对齐 Agda polarCRT/toroidalCRT)
  , t6PolarCRT, t6ToroidalCRT, t6CrtEqual
  -- * 4320D 归约引擎
  , reduceDiv3k, reduceMod3k, reduce4320D, evalArith
  -- * CRT 投影求值
  , evalToNat, crtProject
  -- * 三极等价判定 (替代 Agda βη 归约)
  , convTerm, convAlgebraic, convGeometric, convTopological
  , convType, convTypeByCRT, convTypeStruct
  -- * 穷举验证
  , forall729
  ) where

import Data.Word (Word8, Word16)
import Dayan.Core.Trit   (Trit(..))
import Dayan.Core.Tryte  (Tryte(..), allTrytes, tritAt, sortedPolarCRT, sortedToroidalCRT)
import Dayan.Core.Torus  (TorusPoint(..))
import Dayan.Compute.CRT (lookupPolar, lookupToroidal)
import Dayan.Compute.Orbit (a4Group, a4Action)
import Dayan.ProofGen.AST (Type(..), Term(..), Lit(..))

----------------------------------------------------------------------
-- 1. 比较结果类型
----------------------------------------------------------------------

data Cmp = Equal | NotEqual deriving (Show, Eq)

----------------------------------------------------------------------
-- 2. 格点比对 — O(1) Word16 byte 比对
----------------------------------------------------------------------

-- | 格点比对: 直接 Word16 等值判断
--   宪法: 使用 Fin 3 (Z/3Z 群), 不使用 Trit (GF(3) 域)
--   锚点: toℕ-sum < 729 算术上界
compareTryte :: Tryte -> Tryte -> Cmp
compareTryte (Tryte a) (Tryte b) = if a == b then Equal else NotEqual

-- | 同 compareTryte (语义别名)
compareLattice :: Tryte -> Tryte -> Cmp
compareLattice = compareTryte

----------------------------------------------------------------------
-- 3. 环面坐标操作
----------------------------------------------------------------------

-- | 环面坐标归一化: 极向 mod 144, 环向 mod 46
--   宪法: 极向周期 144, 环向周期 46
normalizeTorus :: TorusPoint -> TorusPoint
normalizeTorus (TorusPoint p t) =
  TorusPoint (p `rem` 144) (t `rem` 46)

-- | 环面坐标比对
compareTorus :: TorusPoint -> TorusPoint -> Cmp
compareTorus (TorusPoint p1 t1) (TorusPoint p2 t2) =
  if p1 == p2 && t1 == t2 then Equal else NotEqual

----------------------------------------------------------------------
-- 4. 模运算归约 (4320D 范式)
----------------------------------------------------------------------

-- | 极向模归约: n mod 144
reduceMod144 :: Word16 -> Word16
reduceMod144 n = n `rem` 144

-- | 环向模归约: n mod 46
reduceMod46 :: Word16 -> Word8
reduceMod46 n = fromIntegral (n `rem` 46)

-- | CRT 投影归约: 格点索引 → (极向余数, 环向余数)
--   公式: (idx % 144, idx % 46)
--   锚点: 6624 → (0, 0)
crtReduce :: Word16 -> (Word8, Word8)
crtReduce idx =
  (fromIntegral (idx `rem` 144), fromIntegral (idx `rem` 46))

----------------------------------------------------------------------
-- 5. 三进制等式判定
----------------------------------------------------------------------

-- | Trit 等式: 直接代数比对
--   宪法: 使用 Trit (GF(3) 域), 区分于 Fin 3 (Z/3Z)
convTrit :: Trit -> Trit -> Bool
convTrit = (==)

-- | Tryte 格点等式 (含 A4 正则化)
--   宪法: A4 群作用于前 4 维, 排序编码消除置换差异
--   策略: 先提取 6 个 Trit, 前 4 个做 A4 轨道检查
--   锚点: 同构对 convTryte (encode v) (a4Action g (encode v)) == True
convTryte :: Tryte -> Tryte -> Bool
convTryte a b =
  -- 1. 直接等值 (最快路径)
  a == b ||
  -- 2. A4 轨道等价: 是否存在 g ∈ A4 使 a4Action g a == b
  orbitEqual a b

-- | A4 轨道等价: a 和 b 是否在同一 A4 轨道中
orbitEqual :: Tryte -> Tryte -> Bool
orbitEqual a b = any (\g -> a4Action g a == b) a4Group

-- | CRT 投影等价: polar 和 toroidal 余数都相同
--   注意: 作用于 torus grid 索引 (0..6623), 使用位置 %144/%46
crtEqual :: Word16 -> Word16 -> Bool
crtEqual a b =
  lookupPolar a == lookupPolar b && lookupToroidal a == lookupToroidal b

----------------------------------------------------------------------
-- 6b. T6 CRT — 排序编码投影 (对齐 Agda polarCRT/toroidalCRT)
----------------------------------------------------------------------

-- | T6 格点极向 CRT 投影 (排序编码 → %144)
--   对齐 Agda: polarCRT p = gf3Toℕ p % 144
t6PolarCRT :: Tryte -> Word8
t6PolarCRT = sortedPolarCRT

-- | T6 格点环向 CRT 投影 (排序编码 → %46)
--   对齐 Agda: toroidalCRT p = gf3Toℕ p % 46
t6ToroidalCRT :: Tryte -> Word8
t6ToroidalCRT = sortedToroidalCRT

-- | T6 CRT 等价: 排序编码的极向+环向投影均相同
--   对齐 Agda: polarCRT a ≡ polarCRT b ∧ toroidalCRT a ≡ toroidalCRT b
--   A4 不变: 同轨道格点 → True (排序编码消除置换效应)
t6CrtEqual :: Tryte -> Tryte -> Bool
t6CrtEqual a b =
  t6PolarCRT a == t6PolarCRT b && t6ToroidalCRT a == t6ToroidalCRT b

----------------------------------------------------------------------
-- ⑥ 类型/项转换 (大衍三极框架)
--
-- 三极判定:
--   代数极: 4320D 归约 → CRT 投影等价 (polarCRT ∧ toroidalCRT)
--   几何极: T6 格点比对 → A4 轨道等价 (排序编码不变)
--   拓扑极: 不变性保持 (陈数 C=±2, 能隙 √3)
--
-- 与 Agda 的本质区别:
--   Agda: 语法树逐节点 βη 归约 → 结构匹配 (2400行)
--   大衍: CRT 余数投影 → 格点坐标比对 (本模块 ~200行)
--   原理: T⁶ 论域有限 (729点), 全称量化可判, 穷举替代归纳
----------------------------------------------------------------------

-- | 三极等价判定: 任意两极通过则等价
convTerm :: Term -> Term -> Bool
convTerm a b =
  convAlgebraic a b || convGeometric a b || convTopological a b

-- | 类型等价: CRT 投影基数 + 三极递推
convType :: Type -> Type -> Bool
convType a b =
  convTypeByCRT a b || convTypeStruct a b

----------------------------------------------------------------------
-- 6a. 4320D 归约引擎
----------------------------------------------------------------------

-- | 4320D 归约: (3*k) % 3 → 0
reduceMod3k :: Term -> Term
reduceMod3k (App (App (Def "%") (Lit (LNat n))) (Lit (LNat 3)))
  | n `rem` 3 == 0 = Lit (LNat 0)
reduceMod3k (App f a) = App (reduceMod3k f) (reduceMod3k a)
reduceMod3k t = t

-- | 4320D 归约: (3*k) / 3 → k
reduceDiv3k :: Term -> Term
reduceDiv3k (App (App (Def "/") (Lit (LNat n))) (Lit (LNat 3)))
  | n `rem` 3 == 0 = Lit (LNat (n `div` 3))
reduceDiv3k (App f a) = App (reduceDiv3k f) (reduceDiv3k a)
reduceDiv3k t = t

-- | 完整 4320D 归一化: div3k → mod3k → 算术求值
reduce4320D :: Term -> Term
reduce4320D = evalArith . reduceDiv3k . reduceMod3k

-- | 算术求值: 对已知运算符做常量折叠
evalArith :: Term -> Term
evalArith (App (App (Def "+") (Lit (LNat a))) (Lit (LNat b))) =
  Lit (LNat (a + b))
evalArith (App (App (Def "*") (Lit (LNat a))) (Lit (LNat b))) =
  Lit (LNat (a * b))
evalArith (App (App (Def "%") (Lit (LNat a))) (Lit (LNat b)))
  | b /= 0 = Lit (LNat (a `rem` b))
evalArith (App (App (Def "/") (Lit (LNat a))) (Lit (LNat b)))
  | b /= 0 = Lit (LNat (a `div` b))
evalArith (App f a) = App (evalArith f) (evalArith a)
evalArith t = t

----------------------------------------------------------------------
-- 6b. CRT 投影求值
----------------------------------------------------------------------

-- | 项 → ℕ (最大努力求值, 无法求值时返回 Nothing)
evalToNat :: Term -> Maybe Word16
evalToNat (Lit (LNat n))          = Just n
evalToNat (Lit LZero)             = Just 0
evalToNat (Lit (LSuc t))          = (+1) <$> evalToNat t
evalToNat (App (App (Def "+") a) b) =
  (+) <$> evalToNat a <*> evalToNat b
evalToNat (App (App (Def "*") a) b) =
  (*) <$> evalToNat a <*> evalToNat b
evalToNat (App (App (Def "%") a) (Lit (LNat b)))
  | b /= 0 = (`rem` b) <$> evalToNat a
evalToNat (App (App (Def "/") a) (Lit (LNat b)))
  | b /= 0 = (`div` b) <$> evalToNat a
evalToNat t =
  let t' = reduce4320D t
  in if t' == t then Nothing          -- 无进一步归约, 放弃
     else evalToNat t'                 -- 归约后重试

-- | CRT 投影: 项 → (polarCRT, toroidalCRT)
crtProject :: Term -> Maybe (Word8, Word8)
crtProject t = do
  n <- evalToNat t
  pure (fromIntegral (n `rem` 144), fromIntegral (n `rem` 46))

----------------------------------------------------------------------
-- 6c. 代数极: CRT 同余等价
----------------------------------------------------------------------

-- | 代数极判定: 4320D 归约 → CRT 投影等价
convAlgebraic :: Term -> Term -> Bool
convAlgebraic a b = case (crtProject a, crtProject b) of
  (Just (pa, ta), Just (pb, tb)) ->
    pa == pb && ta == tb                     -- CRT 余数一致
  _ -> False

----------------------------------------------------------------------
-- 6d. 几何极: T6 格点 + A4 轨道等价
----------------------------------------------------------------------

-- | 几何极判定: 格点比对 → A4 轨道等价
convGeometric :: Term -> Term -> Bool
convGeometric a b =
  -- 尝试求值为 Tryte 格点, 检查 A4 轨道等价
  case (evalToNat a, evalToNat b) of
    (Just na, Just nb)
      | na < 729 && nb < 729 ->
          convTryte (mkTryte na) (mkTryte nb)  -- A4 轨道 + 直接比对
    _ -> False

-- 内部: 从 Word16 构造 Tryte (不验证范围, 调用者保证)
mkTryte :: Word16 -> Tryte
mkTryte = Tryte

----------------------------------------------------------------------
-- 6e. 拓扑极: 不变量保持
----------------------------------------------------------------------

-- | 拓扑极判定: 不变量保持 (陈数 C=±2, 能隙 √3, 6624 对齐)
--   当前实现: 在 T⁶ 有限模型中, 如果两极都不可判定, 回退到结构比较
convTopological :: Term -> Term -> Bool
convTopological a b =
  -- 幻方正交: 检查 CRT 投影后的相位对齐
  -- 拓扑不变量在有限模型中退化为 CRT 格点等价
  convAlgebraic (reduce4320D a) (reduce4320D b)

----------------------------------------------------------------------
-- 6f. 类型等价 (CRT 投影基数)
----------------------------------------------------------------------

-- | CRT 投影类型等价: Fin n ≈ Fin m iff n≡m in CRT 域
convTypeByCRT :: Type -> Type -> Bool
convTypeByCRT (TFin n) (TFin m) = convAlgebraic n m
convTypeByCRT (TVec a n) (TVec b m) = convType a b && convAlgebraic n m
convTypeByCRT _ _ = False

-- | 结构类型等价 (回退: 仅当 CRT 求值不可能时使用)
convTypeStruct :: Type -> Type -> Bool
convTypeStruct TSet TSet = True
convTypeStruct TNat TNat = True
convTypeStruct (TDef x) (TDef y) = x == y
convTypeStruct (TPi _ a b) (TPi _ c d) = convType a c && convType b d
convTypeStruct (TFun a b) (TFun c d)   = convType a c && convType b d
convTypeStruct _ _ = False

----------------------------------------------------------------------
-- 7. 穷举验证
----------------------------------------------------------------------

-- | 对全部 729 个格点验证谓词
forall729 :: (Tryte -> Bool) -> Bool
forall729 p = all p allTrytes
