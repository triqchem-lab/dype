# dype Makefile — 测试与构建入口
# 复制自 Agda Makefile, 适配 dype 项目结构

# 并行测试数 (默认 = CPU 核心数)
PARALLEL_TESTS ?= $(shell getconf _NPROCESSORS_ONLN)

# 测试选项 (传递给 tasty test runner)
AGDA_TESTS_OPTIONS ?= -i -j$(PARALLEL_TESTS)

# Agda 二进制 (dype 使用本地构建的 agda)
AGDA_BIN ?= $(shell which agda)

# 构建工具
CABAL ?= cabal
STACK ?= stack

# 版本
VERSION = 0.1.0.0

.PHONY: all
all: build

# ============================================================
# 构建
# ============================================================

.PHONY: build
build:
	$(CABAL) build all

.PHONY: install
install:
	$(CABAL) install --overwrite-policy=always

# ============================================================
# 测试
# ============================================================

.PHONY: test
test: ## 运行完整测试套件
	@echo "===================== dype test suite ===================="
	@echo "AGDA_BIN = $(AGDA_BIN)"
	@echo "PARALLEL_TESTS = $(PARALLEL_TESTS)"
	@echo "AGDA_TESTS_OPTIONS = $(AGDA_TESTS_OPTIONS)"
	AGDA_BIN=$(AGDA_BIN) AGDA_TESTS_OPTIONS="$(AGDA_TESTS_OPTIONS)" \
		$(CABAL) test all

.PHONY: test-unit
test-unit: ## 仅运行单元测试 (dype-test)
	$(CABAL) test dype-test

.PHONY: test-compat
test-compat: ## 仅运行 Agda 兼容性测试 (agda-compat)
	$(CABAL) test agda-compat

.PHONY: test-succeed
test-succeed: ## 仅运行 Succeed 测试 (dype-succeed)
	$(CABAL) test dype-succeed

# ============================================================
# 子模块
# ============================================================

.PHONY: submodules
submodules: ## 初始化子模块 (std-lib + cubical)
	git submodule update --init --recursive

# ============================================================
# 清理
# ============================================================

.PHONY: clean
clean:
	$(CABAL) clean
	rm -rf dist-newstyle

.PHONY: distclean
distclean: clean
	rm -rf .stack-work

# ============================================================
# 帮助
# ============================================================

.PHONY: help
help: ## 显示帮助
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
