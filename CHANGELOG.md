# dype 变更日志

## 0.1.0.0 (开发中)

### 内核修改
- Empty.hs: instantiateFull 修复，MetaV 替换类型归约到构造子形式
- Unify.hs: d/=d' 冲突检测，不同 Def 节点零消去时返回 UnifyConflict

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
