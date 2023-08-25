export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

BASH ?= $(shell which bash)
ZSH ?= $(shell which zsh)
FISH ?= $(shell which fish)
PYTHON ?= $(shell which python3 || which python)

setup_shells="(setq \
  mistty-test-bash-exe \"$(BASH)\" \
  mistty-test-zsh-exe \"$(ZSH)\" \
  mistty-test-fish-exe \"$(FISH)\"\
  mistty-test-py-exe \"$(PYTHON)\"\
)"

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
	cask emacs --batch -L . --eval $(setup_shells) -L test -l mistty_test.el -f ert-run-tests-batch-and-exit

