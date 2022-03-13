SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

PKG-FILES := grammarly.el

TEST-FILES := $(shell ls test/grammarly-*.el)

.PHONY: clean checkdoc lint build compile unix-test

ci: clean build compile

build:
	EMACS=$(EMACS) $(CASK) install
	EMACS=$(EMACS) $(CASK) build
	EMACS=$(EMACS) $(CASK) clean-elc

compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(PKG-FILES)

unix-test:
	@echo "Testing..."
	$(CASK) exec ert-runner -L . $(LOAD-TEST-FILES) -t '!no-win' -t '!org'

clean:
	rm -rf .cask *.elc
