# dype Makefile — 完整 CI 测试基础设施
# 复制自 Agda Makefile, 适配 dype 项目
#
# CI 流程 (云端):
#   Step 1: make install-bin          (编译)
#   Step 2: make test                 (全量测试, 串行)
#   或 4 组并行:
#     make cubical-test cubical-succeed
#     GHCRTS=-M6G AGDA_TESTS_OPTIONS="-j1" make bugs common succeed fail examples interactive api-test internal-tests compiler-test
#     yes y | make interaction
#     GHCRTS=-M6G AGDA_TESTS_OPTIONS="-j1" make std-lib-test std-lib-compiler-test std-lib-succeed std-lib-interaction
#
# 本地测试:
#   make test          — 全量串行
#   make test-quick    — 快速 (common + succeed + fail)
#   make succeed       — 仅成功测试
#   make cubical-test  — 仅 cubical 测试

# ============================================================
# 配置
# ============================================================

# 并行测试数 (默认 = CPU 核心数)
PARALLEL_TESTS ?= $(shell getconf _NPROCESSORS_ONLN)

# 测试选项 (传递给 tasty test runner)
AGDA_TESTS_OPTIONS ?= -i -j$(PARALLEL_TESTS)

# Agda 二进制 (dype 使用本地构建的 agda, 含 CRT 正交分解内核)
AGDA_BIN ?= $(shell which agda)

# Agda 测试目录 (使用 agda 仓库的测试套件)
AGDA_REPO ?= /data/work/functional-programming/agda
AGDA_TEST_DIR = $(AGDA_REPO)/test

# Tasty 测试运行器 (从 agda 仓库构建)
AGDA_TESTS_BIN ?= $(AGDA_REPO)/dist-newstyle/build/x86_64-linux/ghc-9.14.1/agda-2.9.0/t/agda-tests/build/agda-tests/agda-tests

# 构建工具
CABAL ?= cabal
STACK ?= stack

# 版本
VERSION = 0.1.0.0

# 装饰输出
define decorate
	@echo "======================================================================"
	@echo "$(1)"
	@echo "======================================================================"
	$(2)
endef

# ============================================================
# 构建
# ============================================================

.PHONY: all
all: build

.PHONY: build
build: ## 构建 dype
	$(CABAL) build all

.PHONY: install
install: ## 安装 dype
	$(CABAL) install --overwrite-policy=always

.PHONY: install-bin
install-bin: ## 编译 Agda (CI Step 1)
	cd $(AGDA_REPO) && $(CABAL) install --overwrite-policy=always

.PHONY: install-deps
install-deps: ## 安装依赖
	cd $(AGDA_REPO) && $(CABAL) build --dependencies-only

# ============================================================
# 全量测试 (CI Step 2, 串行)
# ============================================================

.PHONY: test
test: ## 运行完整测试套件 (串行, CI 等价)
	@echo "===================== dype full test suite ===================="
	@echo "AGDA_BIN = $(AGDA_BIN)"
	@echo "PARALLEL_TESTS = $(PARALLEL_TESTS)"
	$(MAKE) common
	$(MAKE) succeed
	$(MAKE) fail
	$(MAKE) bugs
	$(MAKE) build-succeed-test
	$(MAKE) build-fail-test
	$(MAKE) interaction
	$(MAKE) examples
	$(MAKE) cubical-test
	$(MAKE) cubical-succeed
	$(MAKE) interactive
	$(MAKE) latex-html-test
	$(MAKE) api-test
	$(MAKE) internal-tests
	$(MAKE) compiler-test
	$(MAKE) std-lib-test
	$(MAKE) std-lib-compiler-test
	$(MAKE) std-lib-succeed
	$(MAKE) std-lib-interaction

.PHONY: test-quick
test-quick: ## 快速测试 (dype-test + agda-compat, 60s 内完成)
	@echo "=== dype-test (hspec) ==="
	timeout 60 $(CABAL) test dype-test --test-show-details=direct
	@echo "=== agda-compat (前端 roundtrip + 冒烟) ==="
	timeout 60 $(CABAL) test agda-compat --test-show-details=direct

.PHONY: det-test
det-test: ## Det 行列式专项测试
	@echo "=== Det 专项测试 ==="
	$(CABAL) test dype-test --test-show-details=direct --test-options="--match Det"

.PHONY: bench
bench: ## 基准测试 (编译模式)
	@echo "=== Da-Yan Benchmarks ==="
	$(CABAL) run dype-bench

.PHONY: test-using-std-lib
test-using-std-lib: ## 标准库相关测试
	$(MAKE) std-lib-test std-lib-compiler-test std-lib-succeed std-lib-interaction

# ============================================================
# 分类测试目标 (CI 并行组)
# ============================================================

# --- cubical 组 ---

.PHONY: cubical-test
cubical-test: ## Cubical 核心修复验证
	@$(call decorate, "Cubical test suite", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/CubicalSucceed)

.PHONY: cubical-succeed
cubical-succeed: ## Cubical 成功测试
	@$(call decorate, "Cubical succeed tests", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/CubicalSucceed)

# --- test 组 ---

.PHONY: bugs
bugs: ## 回归测试
	@$(call decorate, "Suite of tests for bugs", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/Bugs)

.PHONY: common
common: ## 公共库测试
	@$(call decorate, "Suite of successful tests: mini-library Common", \
		$(MAKE) -C $(AGDA_TEST_DIR)/Common)

.PHONY: succeed
succeed: ## 成功测试集
	@$(call decorate, "Suite of successful tests", \
		echo $$(command -v $(AGDA_BIN)) > $(AGDA_TEST_DIR)/helpers/exec-tc/executables && \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/Succeed ; \
		rm -f $(AGDA_TEST_DIR)/helpers/exec-tc/executables)

.PHONY: fail
fail: ## 失败测试集
	@$(call decorate, "Suite of failing tests", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/Fail)

.PHONY: examples
examples: ## 示例测试
	@$(call decorate, "Suite of example tests", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/Examples)

.PHONY: build-succeed-test
build-succeed-test: ## 构建成功测试
	@$(call decorate, "Suite of successful --build-library tests", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/BuildSucceed)

.PHONY: build-fail-test
build-fail-test: ## 构建失败测试
	@$(call decorate, "Suite of failing --build-library tests", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/BuildFail)

.PHONY: internal-tests
internal-tests: ## 内部单元测试
	@$(call decorate, "Internal test suite", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/Internal)

.PHONY: api-test
api-test: ## API 测试
	@$(call decorate, "API test suite", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/API)

.PHONY: compiler-test
compiler-test: ## 编译器后端测试
	@$(call decorate, "Compiler test suite", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/Compiler)

# --- interaction 组 ---

.PHONY: interaction
interaction: ## 交互测试
	@$(call decorate, "Interaction test suite", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/Interaction)

.PHONY: interactive
interactive: ## 交互模式测试
	@$(call decorate, "Interactive test suite", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/Interactive)

# --- latex/html 组 ---

.PHONY: latex-html-test
latex-html-test: ## LaTeX/HTML 后端测试
	@$(call decorate, "LaTeX and HTML test suite", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/LaTeXAndHTML)

# --- stdlib 组 ---

.PHONY: std-lib-test
std-lib-test: ## 标准库测试
	@$(call decorate, "Standard library test suite", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/LibSucceed)

.PHONY: std-lib-compiler-test
std-lib-compiler-test: ## 标准库编译器测试
	@$(call decorate, "Standard library compiler tests", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/LibCompiler)

.PHONY: std-lib-succeed
std-lib-succeed: ## 标准库成功测试
	@$(call decorate, "Standard library succeed tests", \
		$(AGDA_BIN) --ignore-interfaces --no-default-libraries \
			-i std-lib -i std-lib/src std-lib/Everything.agda +RTS -s)

.PHONY: std-lib-interaction
std-lib-interaction: ## 标准库交互测试
	@$(call decorate, "Standard library interaction tests", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/LibInteraction)

# ============================================================
# dype 专有测试
# ============================================================

.PHONY: test-unit
test-unit: ## dype 单元测试 (179 examples)
	$(CABAL) test dype-test

.PHONY: test-compat
test-compat: ## Agda 兼容性测试 (1975 files, 100%)
	$(CABAL) test agda-compat

.PHONY: test-succeed-dype
test-succeed-dype: ## dype Succeed 测试
	$(CABAL) test dype-succeed

# ============================================================
# 子模块
# ============================================================

.PHONY: submodules
submodules: ## 初始化子模块 (std-lib + cubical)
	git submodule update --init --depth 1 std-lib cubical

# ============================================================
# 清理
# ============================================================

.PHONY: clean
clean: ## 清理构建产物
	$(CABAL) clean
	rm -rf dist-newstyle

.PHONY: clean-caches
clean-caches: ## 清理 Agda 缓存 (解决旧二进制问题)
	rm -rf $(AGDA_REPO)/.stack-work/dist/x86_64-linux-tinfo6/
	rm -rf cubical/_build
	rm -rf std-lib/_build

.PHONY: clean-nuclear
clean-nuclear: ## 核弹级清理 (删除整个 .stack-work)
	rm -rf $(AGDA_REPO)/.stack-work/
	rm -rf cubical/_build
	rm -rf std-lib/_build

.PHONY: distclean
distclean: clean clean-nuclear ## 完全清理

# ============================================================
# 帮助
# ============================================================

.PHONY: help
help: ## 显示帮助
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
