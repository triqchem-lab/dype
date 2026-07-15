-- | Dayan.Kernel.Conversion — 格点比对引擎 (大衍等价性判定)
--
-- 设计原理 (宪法):
--   不重写 Agda 2400 行 TypeChecking/Conversion.hs。
--   大衍的 "类型归约" = 格点坐标比对, "等式判定" = CRT 余数比对。
--   有限模型论: 全称量词可判, 穷举替代归纳。
--
-- 五根支柱:
--   ① 有限模型论 — ∀ over Fin 729 可判
--   ② 群论 — A4 Orbit-Stabilizer 分解
--   ③ 数论 — CRT mod 144/46 投影
--   ④ 格点拓扑 — T⁶ 环面坐标
--   ⑤ 幻方正交 — M4 内积归零判据
--
-- 对齐 Agda: T6.agda (T⁶晶格), A4Group.agda (12元交替群),
--   CRT.agda (CRT谱投影), MagicSquareM4.agda (幻方正交场)

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
  -- * 类型/项转换
  , convType, convTerm
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
-- 6. 类型/项转换 (格点比对替代 βη 归约)
----------------------------------------------------------------------

-- | 类型等价判定
--   有限模型论: 对 Fin n → 值域比对, T⁶ → Tryte 比对
--   结构类型: TSet = TSet, TPi conv, TVec conv, TApp conv
convType :: Type -> Type -> Bool
convType TSet TSet = True
convType TNat TNat = True
convType (TFin a) (TFin b)   = convTerm a b
convType (TVec a n) (TVec b m) = convType a b && convTerm n m
convType (TPi _ a b) (TPi _ c d) = convType a c && convType b d
convType (TFun a b) (TFun c d)   = convType a c && convType b d
convType (TApp f a) (TApp g b)   = convType f g && convTerm a b
convType (TDef x) (TDef y)       = x == y
convType _ _ = False

-- | 项等价判定
--   格点比对: Lit → Word16 等值, Refl → True
--   构造子: App 结构递归, Lam α-等价简化
convTerm :: Term -> Term -> Bool
-- 字面量
convTerm (Lit (LNat a)) (Lit (LNat b)) = a == b
convTerm (Lit LZero)    (Lit LZero)    = True
convTerm (Lit (LSuc a)) (Lit (LSuc b)) = convTerm a b
-- 命题
convTerm Refl Refl = True
convTerm Hole Hole = True
-- 变量/定义: 名等值
convTerm (Var x) (Var y) = x == y
convTerm (Def f) (Def g) = f == g
-- 应用: 结构递归
convTerm (App f a) (App g b) = convTerm f g && convTerm a b
-- λ: α-等价简化 (忽略绑定名, 比较体)
convTerm (Lam _ a) (Lam _ b) = convTerm a b
-- Pi: 依赖积简化
convTerm (Pi _ ta a) (Pi _ tb b) = convType ta tb && convTerm a b
-- 对称/传递/同余: 结构比对
convTerm (Sym a) (Sym b)         = convTerm a b
convTerm (Trans a b) (Trans c d) = convTerm a c && convTerm b d
convTerm (Cong a b) (Cong c d)   = convTerm a c && convTerm b d
-- 类型标注
convTerm (Ann a ta) (Ann b tb) = convTerm a b && convType ta tb
-- 回退: 结构不等
convTerm _ _ = False

----------------------------------------------------------------------
-- 7. 穷举验证
----------------------------------------------------------------------

-- | 对全部 729 个格点验证谓词
forall729 :: (Tryte -> Bool) -> Bool
forall729 p = all p allTrytes
