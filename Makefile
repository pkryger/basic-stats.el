export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

files = $$(cask files | grep -Ev 'basic-stats-(pkg|autoloads).el')
test_files = $(wildcard test/basic-stats*.t.el)

.PHONY: cask-install
cask-install:
	cask install

$(CASK_DIR): Cask
	$(MAKE) cask-install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: bytecompile
bytecompile: cask
	cask emacs -batch -L . -L test                  \
      --eval "(setq byte-compile-error-on-warn t)"  \
      -f batch-byte-compile $(files) $(test_files)
	  (ret=$$? ; cask clean-elc ; rm -f test/*.elc ; exit $$ret)

.PHONY: lint
lint: cask
	cask emacs -batch -L .                                      \
      --load package-lint                                       \
      --eval '(setq package-lint-main-file "basic-stats.el")'   \
      --funcall package-lint-batch-and-exit $(files)

.PHONY: relint
relint: cask
	cask emacs -batch -L . -L test                  \
      --load relint                                 \
      --funcall relint-batch $(files) $(test_files)

.PHONY: checkdoc
checkdoc: cask
	cask emacs -batch -L .                      \
      --load checkdoc-batch                     \
      --funcall checkdoc-batch $(files)

ifeq ($(RUNNER_DEBUG),1)
    define test_debug
"(setq print-level 50                            \
       print-length 100                          \
       backtrace-line-length 5000                \
       eval-expression-print-level nil           \
       eval-expression-print-length nil          \
       ert-batch-print-level print-level         \
       ert-batch-print-length print-length       \
       ert-batch-backtrace-right-margin nil)"
    endef
else
    define test_debug
'(message "Set RUNNER_DEBUG=1 to enable debugging in tests")'
    endef
endif

.PHONY: test
test: cask
	cask emacs -batch                                           \
      $(foreach test_file,$(test_files),--load $(test_file))    \
      --eval $(test_debug)                                      \
      --funcall ert-run-tests-batch-and-exit
