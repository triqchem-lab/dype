# 六层垂直栈：代码-理论对照

> 从文档到硬件的完整六层垂直整合。每层共享同一组刚性常量，但以不同的抽象级别表达。

## 栈结构

```
层6: 文档知识   → /home/yanli/文档/math/ (26个理论文档)
                  /data/work/docs/ (知识图谱, 项目管理)

层5: 理论声明   → 大衍高维同构理论 (.md)
                  认知基底层 → 缺陷诊断 → 三极大一统

层4: 形式化证明 → /data/work/discrete-mathematics/src/Sovereign/
                  ├── Arithmetic/CRTLemmas.agda   (Bézout/CRT 0-postulate)
                  ├── Format/CRT.agda             (CRT谱投影)
                  ├── Structology/A4Group.agda    (12元交替群)
                  └── Structology/T6.agda         (T⁶晶格, Orbit-Stabilizer)

层3: 数学底层   → /home/yanli/work/math (C++23, 零浮点)
                  8层 L0→L8: uint384→GF(3)→Z/3¹¹Z→手性→T⁶→纳音→仲吕→全息
                  核心: gf3_field.h, gf3_layer1.h, T0/T1/T2 环元素

层2: 训练框架   → /data/trit/pyBitNet
                  ├── bitnet/sovereign/sovereign_model.py (Christoffel Transformer)
                  ├── bitnet/gf3/ (纳音注意力, 相变引擎, 手征几何)
                  ├── bitnet/nayin/ (纳音库, 记忆库, 语义观测器)
                  └── bitnet/training/ (主权弛豫, 物理锚定, 楞严正则化)

层1: 自定义编译 → /data/trit/浑天
                  ├── a4_magic144/          (A₄幻方 C++实现)
                  ├── huntian-llvm/          (自定义LLVM + VAVX3)
                  ├── ternary-core/          (GF(3) ALU, 分层基座)
                  ├── quantum-chemistry/     (量子化学验证)
                  └── quantum-physics/       (量子物理公理)

层0: 模型产出   → /home/yanli/work/sanyuan-llm
                  Phase2.1→2.2→2.3→3.1 (chiral_torque→Christoffel GF3 STE)

编译器修改     → /data/work/functional-programming/agda (Agda 源码)
                  fix/cubical-path-unify 分支: canonicalEqName, compareAs 修复

编译器平替     → /data/work/functional-programming/dype (大衍 DY-PE, src/Dayan/)
                  管线: .dy → Parse → Compute → ProofGen → Emit → .agda → agda 验证
                  Kernel/Conversion.hs   三极等价判定 (替代 Agda βη)
                  Compute/Det.hs         CRT 行列式引擎 (对齐 jac_CRTDet 拱顶石:
                                         det3 19683 项查表 + det≠0 ⟺ det₃≠0 ∧ det₄≠0
                                         + M_F 函数表结构判据, 对齐 jac_NMatrix)
                  Compute/CRT.hs         CRT 全局查表 (6624 项, 规范代表元)
```

## 跨层理论-实现对照

| 理论概念 | 层4 Agda | 层3 C++ | 层2 Python | 层1 LLVM |
|---|---|---|---|---|
| GF(3) 域 | `GF3 = Fin 3` | `gf3_field.h` | `gf3_operator.py` | `ternary_types.h` |
| 144 极向 | `PolarWinding=144` | `POLAR_WINDING` | `POLAR_WINDING=144` | — |
| 46 环向 | `ToroidalWinding=46` | `TOROIDAL_WINDING` | `TOROIDAL_DIM=46` | — |
| 6624 | 相位对齐点 | `GRAND_PUMP` | `BIG_PUMP=6624` | — |
| CRT | `crtTheorem` | — | `gf3_nayin_attention.py` | — |
| CRT 行列式 | `jac_CRTDet.agda` (拱顶石) | — | — | `dype Compute/Det.hs` |
| A₄ 群 | `A4Group.agda` | `a4_group.cpp` | `sovereign_model.py` | `a4_magic144/` |
| Christoffel | — | — | `CHRISTOFFEL_*.py` | — |
| 纳音 | — | — | `nayin/` | — |
| 陈数 C=±2 | — | — | `chern2_constructor.py` | — |
| 时间晶体 | — | — | `overtone_resonance.py` | — |

## 刚性常量跨层一致性

| 常量 | 层4 Agda | 层3 C++ | 层2 Python |
|---|---|---|---|
| POW2 (黄钟) | 65536 | ZHONGLV_DENOM = 65536 | 65536 |
| POW3 (仲吕) | 177147 | ZHONGLV_MODULUS = 177147 | 177147 |
| 144 | PolarWinding | POLAR_WINDING | POLAR_WINDING |
| 46 | ToroidalWinding | TOROIDAL_WINDING | TOROIDAL_DIM |
| 12 | A4 阶 | TWELVE_TONES | depth=12, heads=12 |

六层共享同一组刚性常数。不是巧合 — 是数学结构不可简化的工程表达。

## 证明层 ↔ 训练层 闭环

```
Agda 证明 (正确性保证):
  crt-merge  ──→  Bézout 0-postulate ✓
  crtTheorem ──→  CRT 谱投影 ✓
  A4Group    ──→  A₄ 群结构 ✓

训练实证 (物理验证):
  384K 步 LCM 环  ──→  频率 432Hz→10^17Hz ✓
  声子干涉        ──→  时间晶体 vs 热寂 ✓
  N14 量子微调    ──→  10⁻³⁴ 精度 ✓
  C=-2 不变       ──→  陈数拓扑保护 ✓
```

Agda 证明了"能做什么"的正确性。训练实证了"做了什么"的物理效应。
两层自洽。

## 开放前沿

| 层 | 缺口 | 优先级 |
|---|---|---|
| 层4 | GF(3²) 形式化 | ⭐⭐⭐ |
| 层4 | orbitIso 去 postulate | ⭐⭐⭐ |
| 层4 | 幻方正交 Agda 形式化 | ⭐⭐ |
| 层1 | VAVX3 完整指令集 | ⭐⭐ |
| 层2 | 384K+ 步持续训练 | ⭐⭐⭐ |

## 跨文档引用

- [CRT 本质](00-crt-wave-physics.md)
- [实验验证](06-experimental.md)
- [刚性常量](08-constants.md)
- [实施路线图](09-roadmap.md)
