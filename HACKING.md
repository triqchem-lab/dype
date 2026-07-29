# dype 开发指南

## 项目结构

```
dype/
├── src/Dayan/           # dype 自有代码 (CRT/Trit/Parse/ProofGen/Pipeline)
├── vendor/
│   ├── agda-src/Agda/   # 类型检查器源码 (已修改内核)
│   └── agda-syntax/     # cabal 库 (编译 agda-src)
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

dype 修改了 Agda 类型检查器的两个核心文件:

- `vendor/agda-src/Agda/TypeChecking/Empty.hs`: instantiateFull 修复
- `vendor/agda-src/Agda/TypeChecking/Rules/LHS/Unify.hs`: d/=d' 冲突检测

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
