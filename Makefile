export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

files = $$(cask files | grep -Ev 'basic-stats-(pkg|autoloads).el')
test_files = $(wildcard test/basic-stats*.t.el)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: bytecompile
bytecompile: cask
	cask emacs -batch -L . -L test \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(files) $(test_files)
	  (ret=$$? ; cask clean-elc ; rm -f test/*.elc ; exit $$ret)

.PHONY: lint
lint: cask
	cask emacs -batch -L . \
	  --load package-lint \
      --eval '(setq package-lint-main-file "basic-stats.el")' \
	  --funcall package-lint-batch-and-exit $(files)

.PHONY: relint
relint: cask
	cask emacs -batch -L . -L test \
	  --load relint \
	  --funcall relint-batch $(files) $(test_files)

.PHONY: checkdoc
checkdoc: cask
	cask emacs -batch -L . \
	  --load checkdoc-batch \
	  --funcall checkdoc-batch $(files)

.PHONY: test
test: cask
	cask emacs -batch \
      $(foreach test_file,$(test_files),--load $(test_file)) \
	  --eval "(setq print-level 50 \
	                eval-expression-print-level 50 \
                    eval-expression-print-length 1000 \
                    edebug-print-level 50 \
                    edebug-print-length 1000 \
	                ert-batch-print-level 50 \
	                ert-batch-print-length 1000 \
	                ert-batch-backtrace-line-length 1000 \
	                ert-batch-backtrace-right-margin 1000)" \
	  --funcall ert-run-tests-batch-and-exit
