EMACS ?= $(shell which emacs)
export CASK := $(shell which cask)
ifeq ($(CASK),)
$(error Please install CASK at https://cask.readthedocs.io/en/latest/guide/installation.html)
endif
CASK_DIR := $(shell EMACS=$(EMACS) cask package-directory || exit 1)
SRC=$(shell $(CASK) files)
PKBUILD=2.3
ELCFILES = $(SRC:.el=.elc)
ifeq ($(GITHUB_REPOSITORY),)
GITHUB_REPOSITORY := $(shell git config user.name)/$(shell basename `git rev-parse --show-toplevel`)
endif
ifeq ($(GITHUB_BASE_REF),)
GITHUB_BASE_REF := $(shell git rev-parse --abbrev-ref HEAD)
endif
ifeq ($(GITHUB_SHA),)
GITHUB_SHA := $(shell if git show-ref --quiet --verify origin/$(GITHUB_BASE_REF) ; then git rev-parse origin/$(GITHUB_BASE_REF) ; fi))
endif

.DEFAULT_GOAL := test-compile

README.rst: README.in.rst nnhackernews.el
	grep ';;' nnhackernews.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.in.rst > README.rst
	rm -f README.rst0 README.rst1

.PHONY: test-clean
test-clean:
	rm -rf tests/.emacs* tests/.newsrc* tests/Mail tests/News tests/request tests/request-log

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

.PHONY: clean
clean: test-clean
	$(CASK) clean-elc
	rm -f tests/log/*
	rm -f ert-profile*
	rm -rf tests/test-install

.PHONY: test-compile
test-compile: cask
	-sh -ex tools/package-lint.sh nnhackernews.el
	! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):")
	$(CASK) clean-elc

.PHONY: test-install
test-install:
	mkdir -p tests/test-install
	if [ ! -s "tests/test-install/$(PKBUILD).tar.gz" ] ; then \
	  cd tests/test-install ; curl -sLOk https://github.com/melpa/package-build/archive/$(PKBUILD).tar.gz ; fi
	cd tests/test-install ; tar xfz $(PKBUILD).tar.gz
	cd tests/test-install ; rm -f $(PKBUILD).tar.gz
	cd tests/test-install/package-build-$(PKBUILD) ; make -s loaddefs
	mkdir -p tests/test-install/recipes
	cd tests/test-install/recipes ; curl -sfLOk https://raw.githubusercontent.com/melpa/melpa/master/recipes/nnhackernews || cp -f ../../../tools/recipe ./nnhackernews
	! ( $(EMACS) -Q --batch -L tests/test-install/package-build-$(PKBUILD) \
	--eval "(require 'package-build)" \
	--eval "(require 'subr-x)" \
	--eval "(package-initialize)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	--eval "(package-refresh-contents)" \
	--eval "(setq rcp (package-recipe-lookup \"nnhackernews\"))" \
	--eval "(unless (file-exists-p package-build-archive-dir) \
	           (make-directory package-build-archive-dir))" \
	--eval "(let* ((my-repo \"$(GITHUB_REPOSITORY)\") \
	               (my-branch \"$(GITHUB_BASE_REF)\") \
	               (my-commit \"$(GITHUB_SHA)\")) \
	           (oset rcp :repo my-repo) \
	           (oset rcp :branch my-branch) \
	           (oset rcp :commit my-commit))" \
	--eval "(package-build--package rcp (package-build--checkout rcp))" \
	--eval "(package-install-file (car (file-expand-wildcards (concat package-build-archive-dir \"nnhackernews*.el\"))))" 2>&1 | egrep -ia "error: |fatal" )

.PHONY: test-unit
test-unit:
	$(CASK) exec ert-runner -L . -L tests tests/test*.el

.PHONY: test
test: test-compile test-unit test-int

.PHONY: test-int
test-int: test-clean
	$(CASK) exec ecukes --reporter magnars

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	$(CASK) package

.PHONY: install
install: dist
	$(EMACS) -Q --batch -f package-initialize \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install-file (car (file-expand-wildcards \"dist/nnhackernews*.el\")))"
