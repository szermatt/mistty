export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

BASH ?= $(shell which bash)
ZSH ?= $(shell which zsh)

setup_shells="(setq oterm-test-bash-exe \"$(BASH)\" oterm-test-zsh-exe \"$(ZSH)\")"

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask emacs -batch -L . -L test \
          --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $$(cask files); \
	  (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: test
test: compile
	cask emacs --batch -L . --eval $(setup_shells) -L test -l oterm_test.el -f ert-run-tests-batch

