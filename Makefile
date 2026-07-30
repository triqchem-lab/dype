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

# 并行编译线程数 (0=自动全核, 1=串行)
# GitHub CI 8GB 默认用 1, 本地可覆盖: make cubical-test JFLAG=0
JFLAG ?= 1

# 并行测试数 (默认 = CPU 核心数)
# 并行测试数 (默认最多 4 线程, CI 安全)
PARALLEL_TESTS ?= 4

# 子 agda 进程的内存限制 (默认 6GB)
AGDA_RTS ?= +RTS -M6G -RTS

# 全量测试组 (agda CI 对齐) — GHCRTS 给 agda-tests, AGDA_TESTS_OPTIONS 含 RTS 给子进程
# 用法: make succeed | make fail | make test-quick
# 或:   GHCRTS=-M6G make succeed (需要大堆时手动覆盖)
test-group-options = AGDA_TESTS_OPTIONS="-j$(PARALLEL_TESTS) $(AGDA_RTS)"

# 测试选项 (传递给 tasty test runner)
AGDA_TESTS_OPTIONS ?= -i -j$(PARALLEL_TESTS)

# 二进制路径 — cabal 动态解析 (跨平台/跨GHC版本)
AGDA_BIN        ?= $(shell cabal list-bin dype-core:exe:dype)
AGDA_TESTS_BIN  ?= $(shell cabal list-bin dype-core:exe:dype-tests)

# 测试目录
AGDA_TEST_DIR = test

# 数据目录 — 由 Setup.hs 自动从源码树推断, 不设环境变量

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
build: ## 编译 dype
	$(CABAL) build all

.PHONY: install
install: ## 安装 dype
	$(CABAL) install --overwrite-policy=always

.PHONY: install-bin
install-bin: build ## CI Step 1: 编译

.PHONY: dev-link
dev-link: build ## 开发模式: 链接 ~/.local/bin/dype → dist-newstyle
	@ln -sf $$(find dist-newstyle -name dype -type f -path '*/x/dype/*' | head -1) $(HOME)/.local/bin/dype 2>/dev/null && \
	 ln -sf $$(find dist-newstyle -name dype-tests -type f -path '*/x/dype-tests/*' | head -1) $(HOME)/.local/bin/dype-tests 2>/dev/null && \
	 echo "dype → dist-newstyle (dev mode)" || echo "ERROR: run 'make build' first"

.PHONY: install-deps
install-deps: ## 安装依赖
	$(CABAL) build --dependencies-only

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
	-rm -rf cubical/_build
	$(call decorate, "Cubical library test", \
		/usr/bin/time $(MAKE) -C cubical \
			AGDA_BIN="$(AGDA_BIN)" AGDA_FLAGS="-j$(JFLAG)" RTS_OPTIONS=$(AGDA_OPTS))
	@echo ""
	@echo "=== Cubical test summary ==="
	@echo "Agda files: $$(find cubical/Cubical -name '*.agda' | wc -l)"
	@echo "Interface files (.agdai): $$(find cubical/_build -name '*.agdai' 2>/dev/null | wc -l)"
	@echo "============================"

.PHONY: cubical-succeed
cubical-succeed: ## Cubical 成功测试
	@$(call decorate, "Cubical succeed tests", \
		AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/CubicalSucceed)

# --- test 组 ---

.PHONY: bugs
bugs: ## 回归测试
	@$(call decorate, "Suite of tests for bugs", \
		$(test-group-options) AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) --regex-include all/Bugs)

.PHONY: common
common: ## 公共库测试
	@$(call decorate, "Suite of successful tests: mini-library Common", \
		$(MAKE) -C $(AGDA_TEST_DIR)/Common)

.PHONY: succeed
succeed: ## 成功测试集
	@$(call decorate, "Suite of successful tests", \
		$(test-group-options) echo $(shell command -v $(AGDA_BIN)) > $(AGDA_TEST_DIR)/helpers/exec-tc/executables && \
		$(test-group-options) AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) --regex-include all/Succeed ; \
		rm -f $(AGDA_TEST_DIR)/helpers/exec-tc/executables)

.PHONY: fail
fail: ## 失败测试集
	@$(call decorate, "Suite of failing tests", \
		$(test-group-options) AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) --regex-include all/Fail)

.PHONY: examples
examples: ## 示例测试
	@$(call decorate, "Suite of examples", \
		$(MAKE) -C examples)

.PHONY: build-succeed-test
build-succeed-test: ## 构建成功测试
	@$(call decorate, "Suite of successful --build-library tests", \
		$(test-group-options) AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) --regex-include all/BuildSucceed)

.PHONY: build-fail-test
build-fail-test: ## 构建失败测试
	@$(call decorate, "Suite of failing --build-library tests", \
		$(test-group-options) AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) --regex-include all/BuildFail)

.PHONY: internal-tests
internal-tests: ## 内部单元测试
	@$(call decorate, "Internal test suite", \
		$(test-group-options) AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) --regex-include all/Internal)

.PHONY: api-test
api-test: ## API 测试
	@$(call decorate, "API test suite", \
		$(test-group-options) AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) --regex-include all/API)

.PHONY: compiler-test
compiler-test: ## 编译器后端测试
	@$(call decorate, "Compiler tests", \
		$(test-group-options) AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) --regex-include all/Compiler --regex-exclude AllStdLib)

# --- interaction 组 ---

.PHONY: interaction
interaction: ## 交互测试
	@$(call decorate, "Suite of interaction tests", \
		$(MAKE) -C test/interaction AGDA_BIN="$(AGDA_BIN)" HAS_STACK= RUNGHC="cabal exec --project-dir=../.. -- runghc -package dype-core")

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
	@$(call decorate, "Standard library test", \
		cd std-lib && cabal run --project-dir=. GenerateEverything && \
		/usr/bin/time $(AGDA_BIN) $(AGDA_OPTS) --ignore-interfaces --no-default-libraries $(PROFILEOPTS) \
			-i. -isrc Everything.agda +RTS -s)

.PHONY: std-lib-compiler-test
std-lib-compiler-test: ## 标准库编译器测试
	@$(call decorate, "Standard library compiler tests", \
	  AGDA_TESTS_OPTIONS="$(AGDA_TESTS_OPTIONS) +RTS -M6G -RTS" \
	  AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include AllStdLib --regex-exclude AllStdLibJS)

.PHONY: std-lib-succeed
std-lib-succeed: ## 标准库成功测试
	@$(call decorate, "Successful tests using the standard library", \
	  find test/LibSucceed -type f -name '*.agdai' -delete ; \
	  AGDA_TESTS_OPTIONS="$(AGDA_TESTS_OPTIONS) +RTS -M6G -RTS" \
	  AGDA_BIN=$(AGDA_BIN) $(AGDA_TESTS_BIN) $(AGDA_TESTS_OPTIONS) --regex-include all/LibSucceed)

.PHONY: std-lib-interaction
std-lib-interaction: ## 标准库交互测试
	@$(call decorate, "Interaction tests using the standard library", \
	  $(MAKE) -C test/lib-interaction)

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
clean-caches: ## 清理缓存
	rm -rf .stack-work/dist/x86_64-linux-tinfo6/
	rm -rf cubical/_build
	rm -rf std-lib/_build

.PHONY: clean-nuclear
clean-nuclear: ## 核弹级清理 (删除整个 .stack-work)
	rm -rf .stack-work/
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
