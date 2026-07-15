# dype 开发执行手册 v1.0

> 基于 wiki v6.5 知识图谱 + 09-roadmap.md 原子任务分解
> 每个任务包含: 输入/输出类型签名, 宪法约束, 测试锚点, 实现路径

---

## 宪法级前置约束

以下 8 条来自 wiki 的刚性约束，**任何开发不可违反**：

| # | 约束 | 来源 |
|---|------|------|
| 1 | 不可约多项式必须用 **x²+1**，禁止 x²+x+1 | 01-algebraic-pole |
| 2 | 禁止拆分 144 为 12×12/120+24 | 08-constants |
| 3 | 禁止约分 144/46，全息π保持有理分式 | 10-holographic-pi |
| 4 | GF(3) 域 (Trit) ≠ Z/3Z 群 (Fin 3)，必须显式标注 | 13-domain-separation |
| 5 | T⁶ 晶格处理中禁止出现 `_⊕_` / `_⊗_` | 02-geometric-pole |
| 6 | 必须启用 `--rewriting` + div3k/mod3k 归约 | 02-geometric-pole |
| 7 | 禁止 GF(2) 范式 (InversionDepthReached 风险) | 02-geometric-pole |
| 8 | 禁止跨六层栈引用 (dype 层4 必须自包含) | 05-code-theory-mapping |

---

## Phase 3: Conversion 格点比对 (M1, 目标 ~230 行, 1周)

### 设计原理

Conversion 不是重写 Agda 的 2400 行 `TypeChecking/Conversion.hs`，而是用格点字节比对替代语法树逐节点匹配。Clang/LLVM 模型: 大衍只做前端计算，Agda 做后端验证。

### 任务 3.1: `compareLattice` O(1) byte 比对

```haskell
-- 文件: src/Dayan/Kernel/Conversion.hs
-- 类型: Tryte → Tryte → Bool
-- 宪法: 使用 Fin 3 (Z/3Z 群), 不用 Trit (GF(3) 域)
-- 锚点: toℕ-sum < 729 算术上界

compareLattice :: Tryte -> Tryte -> Bool
compareLattice (Tryte a) (Tryte b) = a == b
```

**实现**: 复用 `Tryte` 的 `Eq` 实例 (Word16 直接比对)。  
**测试**: 729 全量格点自反性 `forall729 (\t -> compareLattice t t == True)`

### 任务 3.2: `normalizeTorus` 环面坐标归一化

```haskell
-- 类型: TorusPoint → TorusPoint
-- 宪法: 极向 mod 144, 环向 mod 46
-- 锚点: 极向周期 144, 环向周期 46

normalizeTorus :: TorusPoint -> TorusPoint
normalizeTorus (TorusPoint p t) =
  TorusPoint (p `rem` 144) (t `rem` 46)
```

**测试**: `normalizeTorus (TorusPoint 200 50) == TorusPoint 56 4`

### 任务 3.3: `reduceMod144` / `reduceMod46` 模运算归约

```haskell
-- 类型: Word16 → Word16 / Word16 → Word8
-- 宪法: 使用 4320D 纯模运算, 不展开 mod-helper
reduceMod144 :: Word16 -> Word16
reduceMod144 n = n `rem` 144

reduceMod46 :: Word16 -> Word8
reduceMod46 n = fromIntegral (n `rem` 46)
```

### 任务 3.4: `crtReduce` CRT 投影归约

```haskell
-- 类型: Word16 → (Word8, Word8)
-- 公式: idx → (idx%144, idx%46) — 等价于 lookupCrt
-- 锚点: 6624 相位对齐点 → (0, 0)

crtReduce :: Word16 -> (Word8, Word8)
crtReduce idx = (fromIntegral (idx `rem` 144), fromIntegral (idx `rem` 46))
```

**测试**: `crtReduce 6624 == (0, 0)`  
**测试**: `crtReduce 0 == (0, 0)`  
**测试**: 前 6624 项与 `lookupCrt` 一致

### 任务 3.5: `rewriteDiv3k` / `rewriteMod3k` 4320D 规则

```haskell
-- 类型: 纯模运算范式 [m+kn]%n ≡ m%n
-- 宪法: 4320D 范式, 不展开 mod-helper/div-helper
-- 来源: T6.agda div3k/mod3k REWRITE 规则

-- 基 3 剥离: (3*k) / 3 ≡ k
div3 :: Word16 -> Word16
div3 n = n `div` 3

-- 基 3 剥离: (3*k) % 3 ≡ 0
mod3 :: Word16 -> Word16
mod3 n = n `rem` 3
```

**测试**: `div3 15 == 5`, `mod3 15 == 0`  
**测试**: `div3 17 == 5`, `mod3 17 == 2`

### 任务 3.6: `convTrit` 三进制等式判定

```haskell
-- 类型: Trit → Trit → Bool
-- 宪法: 使用 Trit (GF(3) 域), 非 Fin 3
convTrit :: Trit -> Trit -> Bool
convTrit = (==)
```

**测试**: 9 种组合全覆盖

### 任务 3.7: `convTryte` 格点等式判定 (含 A4 正则化)

```haskell
-- 类型: Tryte → Tryte → Bool
-- 宪法: A4 群作用于前 4 维，排序编码消除置换差异
-- 实现: 先 A4-正则化 (排序前4坐标), 再 Word16 比对
-- 锚点: T6Lattice ≅ Fin 729 双射

convTryte :: Tryte -> Tryte -> Bool
convTryte a b = unTryte (canonicalize a) == unTryte (canonicalize b)
  where
    canonicalize (Tryte n) = Tryte (sort4Prefix n)
    -- sort4Prefix: 对前 4 层 trit 升序排列
    sort4Prefix = ... -- 复用 gf3Toℕ 中的 sort4 逻辑
```

**测试**: 同构对 `convTryte (encode v) (encode (a4Action g v)) == True`  
**测试**: 非等格点 `convTryte (Tryte 0) (Tryte 1) == False`

### 任务 3.8: `convType` / `convTerm` 类型/项转换

```haskell
-- 类型: Type → Type → Bool / Term → Term → Bool
-- 宪法: 有限模型论 — 穷举替代归纳
-- 策略: 
--   Fin n → byte 比对 (n ≤ 729)
--   T⁶ 晶格 → Tryte 比对
--   命题等式 → CRT 投影余数比对
--   嵌套类型 → 结构递归

convType :: Type -> Type -> Bool
convType (TFin a) (TFin b) = convTerm a b
convType (TVec a n) (TVec b m) = convType a b && convTerm n m
convType (TPi a b) (TPi c d) = convType a c && convType b d  -- 简化版本
convType a b = a == b  -- 回退到结构相等

convTerm :: Term -> Term -> Bool
convTerm (Lit (LNat a)) (Lit (LNat b)) = a == b
convTerm Refl Refl = True
convTerm (App f a) (App g b) = convTerm f g && convTerm a b
convTerm (Lam _ a) (Lam _ b) = convTerm a b  -- α 等价简化
convTerm a b = a == b
```

**测试**: `convType (TFin (Lit (LNat 5))) (TFin (Lit (LNat 5))) == True`  
**测试**: `convType (TFin (Lit (LNat 3))) (TFin (Lit (LNat 4))) == False`

### 任务 3.9: Conversion 全量测试

```haskell
-- 测试: 729 格点全量自反性
spec_convRefl = describe "Conversion.reflexivity" $ do
  it "convTryte reflexive for all 729" $
    forall729 (\t -> convTryte t t) `shouldBe` True
  it "convTerm Refl ≡ Refl" $
    convTerm Refl Refl `shouldBe` True
```

---

## Phase 2 优化 (性能补丁, ~100 行, 1-2天)

### 任务 2.1: CRT 逆向 O(1) 查表

```haskell
-- 文件: src/Dayan/Compute/CRT.hs
-- 问题: 当前 reverseTable 是线性搜索 O(n)
-- 修复: 预计算 Vector 6624 → 直接索引 O(1)

-- 新增:
reverseVector :: Vector (Maybe Word16)
reverseVector = Vector.generate 6624 $ \idx ->
  let (p, t) = lookupCrt (fromIntegral idx)
  in if p < 144 && t < 46 then Just idx else Nothing
```

**测试**: 前 100 项 `reverseVector ! lookupCrt n == Just n`

### 任务 2.2: Cascade O(1) pointToIndex

```haskell
-- 文件: src/Dayan/Compute/Cascade.hs
-- 问题: pointToIndex 使用 listToMaybe 线性搜索
-- 修复: 预计算 Map TorusPoint → Word16

import qualified Data.Map.Strict as Map

pointToIndexMap :: Map TorusPoint Word16
pointToIndexMap = Map.fromList
  [(TorusPoint p t, idx) | idx <- [0..6623],
   let p = fromIntegral (idx `rem` 144),
   let t = fromIntegral (idx `rem` 46)]
```

### 任务 2.3: zhonglvSync CRT 谱投影同步

```haskell
-- 文件: src/Dayan/Compute/Cascade.hs
-- 问题: 当前是简化模型 "只是一个简化模型"
-- 修复: 使用 CRT 投影对齐相位

zhonglvSync :: TorusPoint -> TorusPoint -> Bool
zhonglvSync a b =
  let (pa, ta) = (polarPhase a, toroidalPhase a)
      (pb, tb) = (polarPhase b, toroidalPhase b)
  in (pa - pb) `rem` 144 == 0 && (ta - tb) `rem` 46 == 0
```

### 任务 2.4: Orbit A4 真 6 维置换

```haskell
-- 文件: src/Dayan/Compute/Orbit.hs
-- 问题: 当前仅作用前 4 个 Trit
-- 修复: 已经是正确的 — A4 定义上仅作用前 4 维
-- 需要: 测试验证后 2 维不变
```

**测试**: `tritAt (a4Action g t) 4 == tritAt t 4` (对所有 g, t)

### 任务 2.5: Constants 修复

```haskell
-- 文件: src/Dayan/Core/Constants.hs
-- 问题: crtInv144mod46 = 0 (因为 gcd=2)
-- 修复: 使用 Bezout 系数 (gcd=2 时是 2 的倍数)
-- 或标记为不适用 (144 和 46 不互质，无模逆)

-- 修复:
-- | 144 和 46 不互质 (gcd=2)，无标准 Bezout 系数
-- 使用 CRT 查表替代模逆计算
crtInv144mod46 :: Word16
crtInv144mod46 = 0  -- 标记为不适用, 查表模式使用 lookupCrt
```

---

## Phase 4: 前端 .dy 解析器 (M3, ~350 行, 3-4周)

### 设计原理

复用 Agda 的 `Agda.Syntax.*` (Parser/Lexer/AST) 作为语法前端，在语义层替换为大衍的格点计算。`.dy` 文件是 `.agda` 的真子集 + 大衍扩展。

### 任务 4.1: 修复模块名解析

```haskell
-- 文件: src/Dayan/Parse/Dy.hs
-- 问题: 硬编码 "TODO"
-- 修复: 从 module Name where 提取

parseModule :: Text -> Maybe Text
parseModule line = case T.stripPrefix "module " line of
  Just rest -> Just (T.takeWhile (/= ' ') rest)
  Nothing   -> Nothing
```

### 任务 4.2-4.9: 完整解析器

**.dy 语法规范 (宪法级)**:

```
-- 顶层声明
module Name where                          → Module
open import Module using (names)           → Import
{-# OPTIONS --rewriting #-}                → Pragma
postulate name : Type                      → Postulate
name : Type                                → Signature
name = refl                                → Definition
name : Type
name args = term                           → Definition (with body)

-- 类型
Fin n                                      → TFin
Vec A n                                    → TVec
A → B                                      → TPi
Set                                        → TSet
ℕ                                          → TNat

-- 项
variable                                   → Var
constructor                                → Def
refl                                       → Refl
f a                                        → App
λ x → body                                 → Lam
```

### 任务 4.6: Lexer (核心)

```haskell
-- 文件: src/Dayan/Parse/Lexer.hs (新建)
-- 设计: 简化版 Agda lexer, 只支持 .dy 子集

data Token
  = TokModule | TokWhere | TokOpen | TokImport | TokUsing
  | TokPostulate | TokRefl | TokHole | TokLambda | TokArrow
  | TokLParen | TokRParen | TokColon | TokEqual
  | TokName Text | TokNum Integer
  deriving (Show, Eq)

lexDy :: Text -> [Token]
```

**测试**: `lexDy "postulate foo : Fin 3"` → `[TokPostulate, TokName "foo", TokColon, TokName "Fin", TokNum 3]`

### 任务 4.7: Agda.Syntax 桥接

```haskell
-- 文件: src/Dayan/Adapter/Agda.hs
-- 策略: 复用 vendor/agda-src 中的 Agda.Syntax.Internal

-- Phase 4 中期: 替换恒等映射
toAgdaTerm :: D.Term -> AgdaTerm
toAgdaTerm (D.Var x) = Agda.Var (Agda.mkName x)
toAgdaTerm (D.Def f) = Agda.Def (Agda.mkQName f)
toAgdaTerm (D.App f a) = Agda.App (toAgdaTerm f) (toAgdaTerm a)
-- ...
```

### 任务 4.8: dyToAgda 完整编译管线

```haskell
-- 文件: src/Dayan/Adapter/Agda.hs
-- 管线: .dy → Parse → Dayan AST → Agda AST → .agda 文件 → agda 验证

dyToAgda :: FilePath -> IO (Either Error FilePath)
dyToAgda dyFile = do
  -- 1. 解析 .dy 文件
  result <- parseDyFile dyFile
  case result of
    Left err -> pure (Left err)
    Right dyAst -> do
      -- 2. 生成 Agda AST + 证明项
      let agdaAst = toAgdaFile dyAst
      -- 3. 写出 .agda 文件
      writeAgda agdaAst
```

---

## Phase 5: 核心定理 + 验证集成 (M4, ~280 行, 3-4周)

### 任务 5.1: `t6≃fin729` 去 postulate

```haskell
-- 文件: src/Dayan/ProofGen/Templates.hs
-- 问题: 当前 DPostulate "t6≃fin729"
-- 修复: 生成 leftInv 穷举证明 (729 case refl)

genT6LeftInv :: AgdaFile
genT6LeftInv = AgdaFile
  { agdaOptions = "{-# OPTIONS --rewriting #-}"
  , agdaModule = "Generated.T6LeftInv"
  , agdaDecls =
      [ genEncodeDecode  -- 编解码函数
      , genLeftInv       -- finToT6 ∘ t6ToFin ≡ id (729 refl)
      ]
  }
```

**验证**: 生成的 `Generated.T6LeftInv.agda` → `agda` 类型检查通过

### 任务 5.2: `toℕ-sum-injective` 去 postulate

```haskell
-- 策略: 6 层 %3 + /3 剥离模板
-- 公式: [m+kn]%n ≡ m%n (4320D 纯模运算)

genInjectiveProof :: AgdaFile
genInjectiveProof = genDiv3Stripping 6  -- 6 层剥离
```

### 任务 5.8: verifyAgda 外部验证

```haskell
-- 文件: src/Dayan/Verify/Agda.hs
-- 管线: 在临时目录写 .agda 文件 → 调用 agda → 解析错误

verifyAgda :: FilePath -> IO (Either [AgdaError] ())
verifyAgda fp = do
  tmp <- createTempDirectory "dayan-verify"
  let agdaFile = tmp </> takeFileName fp
  writeFile agdaFile =<< readFile fp
  (exit, stdout, stderr) <- readProcessWithExitCode "agda" [agdaFile] ""
  pure $ case exit of
    ExitSuccess -> Right ()
    ExitFailure _ -> Left (parseAgdaErrors stderr)
```

---

## 里程碑验证矩阵

| 里程碑 | 验证命令 | 预期结果 |
|--------|---------|---------|
| M1 | `stack test --fast` | Conversion 全量测试通过 |
| M2 | `stack bench --fast` | CRT/Cascade O(1) 性能 |
| M3 | `stack run gen-demo -- test/T6.dy` | 生成 T6.agda 等价于手写版 |
| M4 | `agda Generated/T6.agda` | 0 errors, postulate 计数 ≤ 基线 |
| M5 | `make ci-test` | 全管线: .dy → .agda → verify ✅ |

---

*基于 wiki v6.5 知识图谱 + 09-roadmap.md · 2026-07-15*
