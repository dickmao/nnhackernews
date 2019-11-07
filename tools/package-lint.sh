#!/bin/sh -ex

# The following is a derivative work of
# https://github.com/purcell/package-lint
# licensed under GNU General Public License v3.0.

EMACS="${EMACS:=emacs}"

INIT_PACKAGE_EL="(progn
  (require 'package)
  (package-initialize)
  (push (quote (\"melpa\" . \"http://melpa.org/packages/\")) package-archives)
  (package-refresh-contents)
  (unless (package-installed-p (quote package-lint))
    (package-install (quote package-lint))))"

# rm -rf "$HOME"/.emacs.d/elpa/package-lint-*

# Get mainline package-lint, then replace package-lint.el with dickmao's.
# quelpa doesn't get data/stdlib-changes.gz for whatever reason.
( cd /tmp ; curl -OskL https://raw.githubusercontent.com/dickmao/package-lint/datetime/package-lint.el )
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         --eval "(let ((dir (file-name-directory (locate-library \"package-lint\")))) \
                     (ignore-errors (delete-file (expand-file-name \"package-lint.elc\" dir))) \
                     (copy-file (expand-file-name \"package-lint.el\" \
                         \"/tmp\") (expand-file-name \"package-lint.el\" dir) t))"

BASENAME=$(basename "$1")
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.el \
         --visit "$1" \
         --eval "(checkdoc-eval-current-buffer)" \
         --eval "(princ (with-current-buffer checkdoc-diagnostic-buffer (buffer-string)))" \
         2>&1 | egrep -a "^$BASENAME:" | egrep -v "Messages should start" && [ -n "${EMACS_LINT_IGNORE+x}" ]

# Lint ourselves
# Lint failures are ignored if EMACS_LINT_IGNORE is defined, so that lint
# failures on Emacs 24.2 and below don't cause the tests to fail, as these
# versions have buggy imenu that reports (defvar foo) as a definition of foo.
# Reduce purity via:
# --eval "(fset 'package-lint--check-defs-prefix (symbol-function 'ignore))" \
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.el \
         --eval "(defconst package-lint--sane-prefixes \
                   (rx \
                    string-start \
                    (or \
                     \"org-dblock-write:\" \
                     \"string-trim-left\" \
                     \"org-babel-execute:\" \
                     \"org-babel-prep-session:\" \
                     \"org-babel-variable-assignments:\" \
                     \"org-babel-default-header-args:\" \
                     \"pcomplete/\")))" \
         -f package-lint-batch-and-exit \
         "$1" || [ -n "${EMACS_LINT_IGNORE+x}" ]
