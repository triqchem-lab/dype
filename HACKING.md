# dype 开发指南

## 项目结构

```
dype/
├── src/Dayan/           # dype 自有代码 (大衍内核: CRT/Trit/Parse/ProofGen/Pipeline)
├── src/full/Agda/       # Agda 外壳 (语法前端 / 类型检查 / 后端)
├── src/setup/           # Agda.Setup 与数据文件清单 (Agda.Setup.DataFiles)
├── test/                # 测试套件 (自带 Agda 全量测试)
├── cubical/             # cubical 库 (clearnature fork)
├── std-lib/             # 标准库 (clearnature fork)
├── config/              # stack 构建配置 (多 GHC 版本)
├── mk/                  # Makefile 辅助
├── app/                 # 可执行文件 (gen-demo)
├── bench/               # 基准测试
└── tables/              # CRT 预计算表
```

## 构建

```bash
# cabal
cabal build all

# stack (GHC 9.14.1)
stack build

# 安装类型检查器
make install-bin
```

## 测试

```bash
# dype 自有测试
cabal test dype-test

# Agda 全量测试 (使用 dype 类型检查器)
make succeed
make fail
make cubical-test
make test    # 全量串行
```

## 内核修改

Agda 外壳 (`src/full/Agda/**`) 同步自 Agda 侧工作仓 `/data/work/functional-programming/agda`；
下列两个类型检查文件携带 **agda 侧**的修复 (该侧本地提交 `eb1251683f`)，
经 2026-09-13 的外壳同步 (基线 `e8f568296d` → `efa277e754`) **随同步带入**，
并非本项目自行改动:

- `src/full/Agda/TypeChecking/Empty.hs`: instantiateFull 修复
- `src/full/Agda/TypeChecking/Rules/LHS/Unify.hs`: d/=d' 冲突检测

## 代码规范

- 遵循 Haskell 风格指南
- 文档用 haddock 风格
- 新功能记录到 CHANGELOG.md
- 测试通过后再提交

## 子模块

```bash
git submodule update --init --depth 1 std-lib cubical
```

子模块指向 clearnature fork:
- cubical: https://github.com/clearnature/cubical.git
- std-lib: https://github.com/clearnature/agda-stdlib.git
