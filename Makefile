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
  mistty-test-python-exe \"$(PYTHON)\"\
)"

tests = $(wildcard *_test.el)

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
	cask emacs --batch -L . --eval $(setup_shells) -L test $(foreach f,$(tests),-l $f) -f ert-run-tests-batch-and-exit

