# 大衍高维同构证明器 (Da-Yan Proof Engine)

> **代号**: DY-PE
> **定位**: 律算合一·大衍高维同构理论的工程实现
> **架构**: Clang/LLVM 同构模型 — 大衍自研前端 + Agda 验证后端

---

## 范式宣言

大衍证明器不是 Agda 的替代品，而是 Agda 在离散拓扑基底上的**工程升维**。

| | Agda | 大衍 |
|:---|:---|:---|
| **数学基底** | Luo UTT + Martin-Löf 依赖类型论 | 有限模型论 + CRT 谱投影 + 群轨道 |
| **相等性判定** | 2400行语法树逐节点匹配 | ~200行格点坐标 byte 比对 |
| **全称量词** | 归纳法 (不可判) | 穷举法 (729 格点 O(1) 可判) |
| **宇宙层级** | Setᵢ 无限层级 | 显式基数 {729, 144, 46, 12, 4, 3, 2} |

## 架构

```
┌──────────────────────────────────────┐
│  大衍引擎 (Haskell/GHC) —— 前端自研    │
│  · .dy 文件解析                       │
│  · 格点巡游 / CRT 查表 / Orbit 分解   │
│  · 证明项生成 → .agda 文件            │
└──────────────┬───────────────────────┘
               │ Agda AST
┌──────────────▼───────────────────────┐
│  Agda 验证器 —— 后端复用 (20年内核)    │
│  · 类型检查 (Conversion.hs)           │
│  · Universe 层级管理                  │
│  · 边界条件宪法对齐                   │
└──────────────────────────────────────┘
```

## 项目结构

```
dype/
├── src/Dayan/
│   ├── Core/          # 格点基本类型
│   │   ├── Trit.hs    # GF(3) 三进制
│   │   ├── Tryte.hs   # Fin 729
│   │   ├── Torus.hs   # 离散环面 T⁶
│   │   └── Constants.hs
│   │
│   ├── Compute/       # 计算引擎
│   │   ├── CRT.hs     # CRT 全局查表
│   │   ├── Orbit.hs   # Orbit-Stabilizer 分解
│   │   ├── Cascade.hs # 极限环级联
│   │   └── ModArith.hs # 模算术
│   │
│   └── ProofGen/      # 证明项生成
│       ├── AST.hs     # Agda AST
│       ├── Emit.hs    # → .agda 文件
│       └── Templates.hs
│
├── tables/            # 预计算数据
├── test/              # hspec 测试
├── bench/             # criterion 基准
├── docs/              # 设计文档
└── README.md
```

## 执行计划

| 阶段 | 内容 | 预计 |
|:---|:---|:---|
| Phase 1 | Core 类型定义 (Trit/Tryte/Torus) | 1 周 |
| Phase 2 | 计算引擎 (CRT/Orbit/Cascade) | 2-4 周 |
| Phase 3 | Conversion 替换 (~200行) | 2-4 周 |
| Phase 4 | 前端复用 (Agda.Syntax) | 4-8 周 |
| Phase 5 | 验证器集成 | 4-8 周 |

## 关联项目

- [discrete-mathematics](https://github.com/triqchem-lab/discrete-mathematics) — 大衍理论的 Agda 形式化证明 (8700+ 行)
- [大衍高维同构理论 Wiki](https://github.com/triqchem-lab/discrete-mathematics/tree/master/docs/wiki) — 15 篇知识文档

## 许可

MIT License
