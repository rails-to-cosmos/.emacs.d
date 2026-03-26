EMACS ?= emacs

# Find all .el files in src/, excluding archive/
SRC_FILES := $(shell find src -name '*.el' -not -path 'src/archive/*' | sort)

# Set up Emacs batch environment with proper load paths
EMACS_BATCH = $(EMACS) --batch \
  --eval "(require 'cl-lib)" \
  --eval "(let ((default-directory \"$(CURDIR)/elpa\")) (normal-top-level-add-subdirs-to-load-path))" \
  --eval "(let ((default-directory \"$(CURDIR)/packages\")) (normal-top-level-add-subdirs-to-load-path))" \
  -L src -L src/repos -L src/network-manager

# Find all test .el files (in test/ and src/ subdirs)
TEST_FILES := $(shell find test src -name 'test-*.el' | sort)

.PHONY: typecheck typecheck-strict clean-elc test

## Type-check all elisp files via byte compilation (warnings displayed, fails on errors)
typecheck:
	@$(EMACS_BATCH) \
	  -f batch-byte-compile $(SRC_FILES) 2>&1; \
	ret=$$?; \
	find src -name '*.elc' -delete 2>/dev/null; \
	exit $$ret

## Strict type-check: treats warnings as errors
typecheck-strict:
	@$(EMACS_BATCH) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(SRC_FILES) 2>&1; \
	ret=$$?; \
	find src -name '*.elc' -delete 2>/dev/null; \
	exit $$ret

## Run ERT unit tests
test:
	@$(EMACS_BATCH) \
	  -L test \
	  $(foreach f,$(TEST_FILES),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

## Remove any stale .elc files
clean-elc:
	@find src -name '*.elc' -delete 2>/dev/null; true
