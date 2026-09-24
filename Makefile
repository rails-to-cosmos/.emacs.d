EMACS ?= emacs

# Find all .el files in src/, excluding archive/
SRC_FILES := $(shell find src -name '*.el' -not -path 'src/archive/*' | sort)

# Set up Emacs batch environment with proper load paths.
# `package-initialize' activates installed packages so their autoloads override
# built-ins (e.g. an upgraded `transient' shadowing the older built-in that
# Magit rejects on Emacs 29/30); the manual add-subdirs cover packages/, which
# is not a package archive.
EMACS_BATCH = $(EMACS) --batch \
  --eval "(require 'cl-lib)" \
  --eval "(package-initialize)" \
  -l $(CURDIR)/src/mijn-packages.el \
  --eval "(let ((default-directory \"$(CURDIR)/elpa\")) (normal-top-level-add-subdirs-to-load-path))" \
  --eval "(let ((default-directory \"$(CURDIR)/packages\")) (normal-top-level-add-subdirs-to-load-path))" \
  -L src -L src/network-manager -L packages

# Find all test .el files (in test/ and src/ subdirs)
TEST_FILES := $(shell find test src -name 'test-*.el' | sort)

.PHONY: typecheck typecheck-strict clean-elc test test-unit startup-test integration

# Emacs versions exercised by the podman integration suite.  `latest' tracks
# the newest image published by silex/emacs, in addition to the pinned matrix.
INTEG_VERSIONS ?= 29.4 30.2 31.1 latest
TEST_EMACS_VERSION ?= 31.1

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

## Run unit tests and a fresh containerized startup check.
test: test-unit startup-test

## Run only the local ERT unit tests (also used inside the test container).
test-unit:
	@$(EMACS_BATCH) \
	  -L test \
	  $(foreach f,$(TEST_FILES),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

## Test a clean package bootstrap and full startup under Podman.
startup-test:
	@EMACS_CONFIG_FRESH=1 test/integration/run.sh $(TEST_EMACS_VERSION)

## Run the podman integration suite across Emacs versions ($(INTEG_VERSIONS))
integration:
	@test/integration/run.sh $(INTEG_VERSIONS)

## Remove any stale .elc files
clean-elc:
	@find src -name '*.elc' -delete 2>/dev/null; true
