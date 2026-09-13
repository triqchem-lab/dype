# dype 变更日志

## 0.1.0.0 (开发中)

### 外壳同步 (Agda)
- 外壳同步到 agda 工作仓 HEAD (`e8f568296d` → `efa277e754`): 134 文件 + 8 新增
- 随同步带入的 agda 侧修复 (非本项目自有改动):
  - `src/full/Agda/TypeChecking/Empty.hs`: `instantiateFull` 修复, MetaV 替换类型归约到构造子形式
  - `src/full/Agda/TypeChecking/Rules/LHS/Unify.hs`: `d/=d'` 冲突检测, 不同 Def 节点零消去时返回 UnifyConflict
- `src/dype-core.cabal` 补 3 个 `Builtin/Erased/*.agda` 数据文件登记

### 计算引擎
- CRT 全局查表 (6624 项)
- 3×3 GF(3) Sarrus 行列式 + 19683 项 O(1) 查表
- 4×4 Laplace 归约到 3×3
- CRT 分解: det(M) ≠ 0 ⟺ det₃ ≠ 0 ∧ det₄ ≠ 0

### 前端
- .dy 解析器: 多参数函数、infix、lambda、record、隐式参数、where 子句
- 中缀运算符渲染、函数类型括号、复合参数括号

### 测试
- dype-test: 201 examples
- agda-compat: 2041 files roundtrip
- dype-succeed: 2041 files roundtrip
- cubical-test: 1192 modules
