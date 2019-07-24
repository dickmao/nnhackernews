;;; nnhackernews.el --- Gnus backend for hackernews  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019 The Authors of nnhackernews.el

;; Authors: dickmao <github id: dickmao>
;; Version: 0
;; Keywords: news
;; URL: https://github.com/dickmao/nnhackernews
;; Package-Requires: ((emacs "25"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with nnhackernews.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A Gnus backend for Hackernews.

;;; Code:

(require 'nnoo)
(require 'gnus)
(require 'gnus-start)
(require 'gnus-art)
(require 'gnus-sum)
(require 'gnus-msg)
(require 'gnus-cite)
(require 'gnus-srvr)
(require 'gnus-cache)
(require 'gnus-bcklg)
(require 'python)
(require 'json-rpc)
(require 'mm-url)
(require 'cl-lib)
(require 'virtualenvwrapper)

(nnoo-declare nnhackernews)

(defmacro nnhackernews--gethash (string hashtable)
  "Get corresponding value of STRING from HASHTABLE.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  `(,(if (fboundp 'gnus-gethash-safe)
         'gnus-gethash-safe
       'gethash)
    ,string ,hashtable))

(defmacro nnhackernews--sethash (string value hashtable)
  "Set corresponding value of STRING to VALUE in HASHTABLE.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  `(,(if (fboundp 'gnus-sethash)
         'gnus-sethash
       'puthash)
    ,string ,value ,hashtable))

(defcustom nnhackernews-python-command (if (equal system-type 'windows-nt)
                                       (or (executable-find "py")
                                           (executable-find "pythonw")
                                           "python")
                                     "python")
  "Python executable name."
  :type (append '(choice)
                (let (result)
                  (dolist (py '("python" "python2" "python3" "pythonw" "py")
                              result)
                    (setq result (append result `((const :tag ,py ,py))))))
                '((string :tag "Other")))
  :group 'nnhackernews)

(defcustom nnhackernews-venv
  (let* ((library-directory (file-name-directory (locate-library "nnhackernews")))
         (defacto-version (file-name-nondirectory
                           (directory-file-name library-directory)))
         (venv-id (concat defacto-version "-" nnhackernews-python-command))
         (result (concat venv-location venv-id))
         (requirements (concat library-directory "requirements.txt"))
         (install-args (if (file-exists-p requirements)
                           (list "-r" requirements)
                         (list "virtualenv")))
         (already-in-venv
          (not (zerop (apply #'call-process nnhackernews-python-command
                             nil nil nil
                             (list
                              "-c"
                              "import sys; sys.exit(hasattr(sys, 'real_prefix'))")))))
         (pip-args (append (list "-m" "pip" "install")
                           (unless already-in-venv (list "--user"))
                           install-args))
         (pip-status
          (apply #'call-process nnhackernews-python-command nil nil nil
                 pip-args)))
    (gnus-message 7 "nnhackernews-venv: %s %s" nnhackernews-python-command
                  (mapconcat 'identity pip-args " "))
    (cond ((numberp pip-status)
           (unless (zerop pip-status)
             (gnus-message 3 "nnhackernews-venv: pip install exit %s" pip-status)))
          (t (gnus-message 3 "nnhackernews-venv: pip install signal %s" pip-status)))
    (gnus-message 7 "nnhackernews-venv: %s" result)
    (unless (file-exists-p venv-location)
      (make-directory venv-location))
    (cond ((member venv-id (split-string (venv-list-virtualenvs))) result)
          (t (gnus-message 5 "nnhackernews-venv: installing venv to %s..." result)
             (condition-case err
                 (progn
                   (venv-mkvirtualenv-using nnhackernews-python-command venv-id)
                   (venv-with-virtualenv-shell-command
                    venv-id
                    ;; `python` and not `nnhackernews-python-command` because
                    ;; venv normalizes the executable to `python`.
                    (format "cd %s && python setup.py install" library-directory))
                   (gnus-message 5 "nnhackernews-venv: installing venv to %s...done" result)
                   result)
               (error (when (venv-is-valid venv-id)
                        (condition-case rmerr
                            (venv-rmvirtualenv venv-id)
                          (error (gnus-message 3 (format "venv-rmvirtualenv: %s"
                                                         (error-message-string rmerr))))))
                      (gnus-message 3 (format "nnhackernews-venv: %s"
                                              (error-message-string err)))
                      "/dev/null")))))
  "Full path to venv directory.

To facilitate upgrades, the name gloms a de facto version (the directory
name where this file resides) and the `nnhackernews-python-command'."
  :type '(choice (string :tag "Directory" (get (quote nnhackernews-env) (quote standard-value)))
                 (const :tag "Development" nil))
  :group 'nnhackernews)

;; keymaps made by `define-prefix-command' in `gnus-define-keys-1'
(defvar nnhackernews-article-mode-map)
(defvar nnhackernews-group-mode-map)

;; keymaps I make myself
(defvar nnhackernews-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" 'gnus-summary-followup)
    (define-key map "R" 'gnus-summary-followup-with-original)
    (define-key map "F" 'gnus-summary-followup-with-original)
    map))

(defcustom nnhackernews-log-rpc nil
  "Turn on PRAW logging."
  :type 'boolean
  :group 'nnhackernews)

(defcustom nnhackernews-rpc-request-timeout 60
  "Turn on PRAW logging."
  :type 'integer
  :group 'nnhackernews)

(defcustom nnhackernews-localhost "127.0.0.1"
  "Some users keep their browser in a separate domain.

Do not set this to \"localhost\" as a numeric IP is required for the oauth handshake."
  :type 'string
  :group 'nnhackernews)

(defvar nnhackernews-rpc-log-filename nil)

(defvar nnhackernews--python-module-extra-args nil "Primarily for testing.")

(define-minor-mode nnhackernews-article-mode
  "Minor mode for nnhackernews articles.  Disallow `gnus-article-reply-with-original'.

\\{gnus-article-mode-map}
"
  :lighter " Hackernews"
  :keymap gnus-article-mode-map

  (when nnhackernews-article-mode
    (gnus-define-keys (nnhackernews-article-mode-map "R" gnus-article-mode-map)
      "0" nnhackernews-novote
      "-" nnhackernews-downvote
      "=" nnhackernews-upvote
      "+" nnhackernews-upvote)))

(define-minor-mode nnhackernews-summary-mode
  "Disallow \"reply\" commands in `gnus-summary-mode-map'.

\\{nnhackernews-summary-mode-map}
"
  :lighter " Hackernews"
  :keymap nnhackernews-summary-mode-map)

(define-minor-mode nnhackernews-group-mode
  "Add `R-g' go-to-subhackernews binding to *Group*.

\\{gnus-group-mode-map}
"
  :keymap gnus-group-mode-map
  (when nnhackernews-group-mode
    (gnus-define-keys (nnhackernews-group-mode-map "R" gnus-group-mode-map)
      "g" nnhackernews-goto-group)))

(defsubst nnhackernews-novote ()
  "Retract vote."
  (interactive)
  (nnhackernews-vote-current-article 0))

(defsubst nnhackernews-downvote ()
  "Downvote the article in current buffer."
  (interactive)
  (nnhackernews-vote-current-article -1))

(defsubst nnhackernews-upvote ()
  "Upvote the article in current buffer."
  (interactive)
  (nnhackernews-vote-current-article 1))

(defvar nnhackernews--seq-map-indexed
  (if (fboundp 'seq-map-indexed)
      #'seq-map-indexed
    (lambda (function sequence)
      (let ((index 0))
        (seq-map (lambda (elt)
                   (prog1
                       (funcall function elt index)
                     (setq index (1+ index))))
                 sequence)))))

(defmacro nnhackernews--normalize-server ()
  "Disallow \"server\" from being empty string, which is unsettling.
Normalize it to \"nnhackernews-default\"."
  `(let ((canonical "nnhackernews-default"))
    (when (equal server "")
      (setq server nil))
    (unless server
      (setq server canonical))
    (unless (string= server canonical)
      (error "nnhackernews--normalize-server: multiple servers unsupported!"))))

(defmacro nnhackernews-aif (test-form then-form &rest else-forms)
  "Anaphoric if TEST-FORM THEN-FORM ELSE-FORMS.  Adapted from `e2wm:aif'."
  (declare (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'nnhackernews-aif 'lisp-indent-function 2)

(defmacro nnhackernews-aand (test &rest rest)
  "Anaphoric conjunction of TEST and REST.  Adapted from `e2wm:aand'."
  (declare (debug (form &rest form)))
  `(let ((it ,test))
     (if it ,(if rest (macroexpand-all `(nnhackernews-aand ,@rest)) 'it))))

(defmacro nnhackernews-and-let* (bindings &rest form)
  "Gauche's `and-let*'.  Each of BINDINGS must resolve to t before evaluating FORM."
  (declare (debug ((&rest &or symbolp (form) (gate symbolp &optional form))
                   body))
           ;; See: (info "(elisp) Specification List")
           (indent 1))
  (if (null bindings)
      `(progn ,@form)
    (let* ((head (car bindings))
           (tail (cdr bindings))
           (rest (macroexpand-all `(nnhackernews-and-let* ,tail ,@form))))
      (cond
       ((symbolp head) `(if ,head ,rest))
       ((= (length head) 1) `(if ,(car head) ,rest))
       (t `(let (,head) (if ,(car head) ,rest)))))))

(defvar nnhackernews-headers-hashtb (gnus-make-hashtable)
  "Group (subhackernews) string -> interleaved submissions and comments sorted by created time.")

(defvar nnhackernews-refs-hashtb (gnus-make-hashtable)
  "Who replied to whom (global over all entries).")

(defvar nnhackernews-authors-hashtb (gnus-make-hashtable)
  "For fast lookup of parent-author (global over all entries).")

(defsubst nnhackernews-get-headers (group)
  "List headers from GROUP."
  (nnhackernews--gethash group nnhackernews-headers-hashtb))

(defun nnhackernews-find-header (group id)
  "O(n) search of GROUP headers for ID."
  (nnhackernews-and-let* ((headers (nnhackernews-get-headers group))
                      (found (seq-position headers id
                                           (lambda (plst id)
                                             (equal id (plist-get plst :id))))))
                     (nnhackernews--get-header (1+ found) group)))

(defsubst nnhackernews-refs-for (name &optional depth)
  "Get message ancestry for NAME up to DEPTH."
  (unless depth
    (setq depth most-positive-fixnum))
  (when (> depth 0)
    (nreverse (cl-loop with parent-id = (nnhackernews--gethash name nnhackernews-refs-hashtb)
                       for level = 0 then level
                       for name = parent-id then
                       (nnhackernews--gethash name nnhackernews-refs-hashtb)
                       until (null name)
                       collect name
                       until (>= (cl-incf level) depth)))))

(defsubst nnhackernews-sort-append-headers (group &rest lvp)
  "Append to hashed headers of GROUP the LVP (list of vector of plists)."
  (nnhackernews--sethash group (append (nnhackernews-get-headers group)
                              (apply #'nnhackernews--sort-headers lvp))
                nnhackernews-headers-hashtb))

(defvar nnhackernews-directory (nnheader-concat gnus-directory "hackernews")
  "Where to retrieve last read state.")

(defvar nnhackernews-processes nil
  "Garbage collect PRAW processes.")

(nnoo-define-basics nnhackernews)

(defsubst nnhackernews-rpc-call (server generator_kwargs method &rest args)
  "Make jsonrpc call to SERVER with GENERATOR_KWARGS using METHOD ARGS.

Process stays the same, but the jsonrpc connection (a cheap struct) gets reinstantiated with every call."
  (nnhackernews--normalize-server)
  (nnhackernews-and-let* ((proc (nnhackernews-rpc-get server))
                      (connection (json-rpc--create :process proc
                                                    :host nnhackernews-localhost
                                                    :id-counter 0)))
    (apply #'nnhackernews-rpc-request connection generator_kwargs method args)))

(defun nnhackernews-goto-group (realname)
  "Jump to the REALNAME subhackernews."
  (interactive (list (read-no-blanks-input "Subhackernews: r/")))
  (let* ((canonical (nnhackernews-rpc-call nil nil "canonical_spelling" realname))
         (group (gnus-group-full-name canonical "nnhackernews")))
    (gnus-activate-group group t)
    (gnus-group-read-group t t group)))

(defun nnhackernews-vote-current-article (vote)
  "VOTE is +1, -1, 0."
  (unless gnus-article-current ;; gnus-article-current or gnus-current-article?
    (error "No current article"))
  (unless gnus-newsgroup-name
    (error "No current newgroup"))
  (let* ((header (nnhackernews--get-header (cdr gnus-article-current)
                                       (gnus-group-real-name (car gnus-article-current))))
         (orig-score (format "%s" (plist-get header :score)))
         (new-score (if (zerop vote) orig-score
                      (concat orig-score " "
                              (if (> vote 0) "+" "")
                              (format "%s" vote))))
         (article-name (plist-get header :name)))
    (let ((inhibit-read-only t))
      (nnheader-replace-header "score" new-score))
    (nnhackernews-rpc-call nil nil "vote" article-name vote)))

(defsubst nnhackernews--gate (&optional group)
  "Apply our minor modes only when the following conditions hold for GROUP."
  (unless group
    (setq group gnus-newsgroup-name))
  (and (stringp group)
       (listp (gnus-group-method group))
       (eq 'nnhackernews (car (gnus-group-method group)))))

(defun nnhackernews-update-subscription (group level oldlevel &optional _previous)
  "Nnhackernews `gnus-group-change-level' callback of GROUP to LEVEL from OLDLEVEL."
  (when (nnhackernews--gate group)
    (let ((old-subbed-p (<= oldlevel gnus-level-default-subscribed))
          (new-subbed-p (<= level gnus-level-default-subscribed)))
      (unless (eq old-subbed-p new-subbed-p)
        ;; afaict, praw post() doesn't return status
        (if new-subbed-p
            (nnhackernews-rpc-call nil nil "subscribe" (gnus-group-real-name group))
          (nnhackernews-rpc-call nil nil "unsubscribe" (gnus-group-real-name group)))))))

(defun nnhackernews-rpc-kill (&optional server)
  "Kill the jsonrpc process named SERVER."
  (interactive (list nil))
  (nnhackernews--normalize-server)
  (let (new-processes)
    (mapc (lambda (proc) (if (and server (not (string= server (process-name proc))))
                             (push proc new-processes)
                           (delete-process proc)))
          nnhackernews-processes)
    (setq nnhackernews-processes new-processes)))

(deffoo nnhackernews-request-close ()
  (nnhackernews-close-server)
  t)

(deffoo nnhackernews-request-type (_group &optional _article)
  'news)

(deffoo nnhackernews-server-opened (&optional server)
  (nnhackernews--normalize-server)
  (cl-remove-if-not (lambda (proc) (string= server (process-name proc)))
                 nnhackernews-processes))

(deffoo nnhackernews-status-message (&optional server)
  (nnhackernews--normalize-server)
  "")

(deffoo nnhackernews-open-server (_server &optional _defs)
  t)

(deffoo nnhackernews-close-group (_group &optional server)
  (nnhackernews--normalize-server)
  t)

(defmacro nnhackernews--with-group (group &rest body)
  "Disambiguate GROUP if it's empty and execute BODY."
  (declare (debug (form &rest form))
           (indent 1))
  `(let* ((group (or ,group (gnus-group-real-name gnus-newsgroup-name)))
          (gnus-newsgroup-name (gnus-group-prefixed-name group "nnhackernews")))
     ,@body))

(defun nnhackernews--get-header (article-number &optional group)
  "Get header indexed ARTICLE-NUMBER for GROUP."
  (nnhackernews--with-group group
    (let ((headers (nnhackernews-get-headers group)))
      (elt headers (1- article-number)))))

(defun nnhackernews--get-body (name &optional group server)
  "Get full text of submission or comment NAME for GROUP at SERVER."
  (nnhackernews--normalize-server)
  (nnhackernews--with-group group
    (nnhackernews-rpc-call server nil "body" group name)))

(defsubst nnhackernews-hack-name-to-id (name)
  "Get x from t1_x (NAME)."
  (cl-subseq name 3))

(defsubst nnhackernews--br-tagify (body)
  "Hackernews-html BODY shies away from <BR>.  Should it?"
  (replace-regexp-in-string "\n" "<br>" body))

(defsubst nnhackernews--citation-wrap (author body)
  "Cite AUTHOR using `gnus-message-cite-prefix-regexp' before displaying BODY.

Originally written by Paul Issartel."
  (with-temp-buffer
    (insert body)
    (mm-url-remove-markup)
    (mm-url-decode-entities)
    (fill-region (point-min) (point-max))
    (let* ((trimmed-1 (replace-regexp-in-string "\\(\\s-\\|\n\\)+$" "" (buffer-string)))
           (trimmed (replace-regexp-in-string "^\\(\\s-\\|\n\\)+" "" trimmed-1)))
      (concat author " wrote:<br>\n"
              "<pre>\n"
              (cl-subseq (replace-regexp-in-string "\n" "\n> " (concat "\n" trimmed)) 1)
              "\n</pre>\n\n"))))

(defun nnhackernews-add-entry (hashtb e field)
  "Add to HASHTB the pair consisting of entry E's name to its FIELD."
  (nnhackernews--sethash (plist-get e :name) (plist-get e field) hashtb))

(defun nnhackernews--filter-after (after-this vop)
  "Get elements created AFTER-THIS in VOP (vector of plists)."
  (cl-loop for elt-idx in (funcall nnhackernews--seq-map-indexed
                                   (lambda (elt idx) (cons elt idx)) vop)
           until (>= (plist-get (car elt-idx) :created_utc) after-this)
           finally return (seq-drop vop (or (cdr elt-idx) 0))))

(defsubst nnhackernews--base10 (base36)
  "Convert BASE36 hackernews name encoding to a base10 integer."
  (apply #'+ (funcall nnhackernews--seq-map-indexed
                      (lambda (elt idx)
                        (* (expt 36 idx)
                           (if (>= elt ?a) (+ 10 (- elt ?a)) (- elt ?0))))
                      (reverse base36))))

(deffoo nnhackernews-request-group-scan (group &optional server _info)
  "M-g from *Group* calls this.

Set flag for the ensuing `nnhackernews-request-group' to avoid going out to PRAW yet again."
  (nnhackernews--normalize-server)
  (nnhackernews--with-group group
    (gnus-message 5 "nnhackernews-request-group-scan: scanning %s..." group)
    (gnus-activate-group (gnus-group-full-name group '("nnhackernews" (or server ""))) t)
    (gnus-message 5 "nnhackernews-request-group-scan: scanning %s...done" group)
    t))

;; gnus-group-select-group
;;   gnus-group-read-group
;;     gnus-summary-read-group
;;       gnus-summary-read-group-1
;;         gnus-summary-setup-buffer
;;           sets gnus-newsgroup-name
;;         gnus-select-newsgroup
;;           gnus-request-group
;;             nnhackernews-request-group
(deffoo nnhackernews-request-group (group &optional server _fast info)
  (nnhackernews--normalize-server)
  (nnhackernews--with-group group
    (let* ((info
            (or info
                (gnus-get-info gnus-newsgroup-name)
                (list group
                      gnus-level-default-subscribed
                      nil nil
                      (gnus-method-simplify (gnus-group-method gnus-newsgroup-name)))))
           (params (gnus-info-params info))
           (newsrc-read-ranges (gnus-info-read info))
           (newsrc-seen-cons (gnus-group-parameter-value params 'last-seen t))
           (newsrc-seen-index (car newsrc-seen-cons))
           (newsrc-seen-id (cdr newsrc-seen-cons)))
      (let* ((headers (nnhackernews-get-headers group))
             (num-headers (length headers))
             (status (format "211 %d %d %d %s" num-headers 1 num-headers group)))
        (gnus-message 7 "nnhackernews-request-group: %s" status)
        (nnheader-insert "%s\n" status)

        ;; remind myself how this works:
        ;; old-praw (1 - 20=emkdjrx)
        ;; read-ranges (1 - 10)                   (15 - 20)
        ;; unread-ranges       (11, 12, 13, 14)
        ;; new-praw    (12 13 14 15 16 17 18 19 20 - 100)
        ;; 20=emkdjrx in old-praw is 9=emkdjrx in new-praw.  index shift is 20-9=+11
        ;; new-unread-ranges   (0,  1,   2,  3)
        ;; new-read-ranges                        (4 - 9)
        (when (gnus-group-entry gnus-newsgroup-name)
          ;; seen-indices are one-indexed !
          (let* ((newsrc-seen-index-now
                  (if (or (not (stringp newsrc-seen-id))
                          (zerop (nnhackernews--base10 newsrc-seen-id)))
                      1
                    (cl-loop for cand = nil
                             for plst in headers
                             for i = 1 then (1+ i)
                             if (= (nnhackernews--base10 (plist-get plst :id))
                                   (nnhackernews--base10 newsrc-seen-id))
                             do (gnus-message 7 "nnhackernews-request-group: exact=%s" i)
                             and return i ;; do not go to finally
                             end
                             if (and (null cand)
                                     (> (nnhackernews--base10 (plist-get plst :id))
                                        (nnhackernews--base10 newsrc-seen-id)))
                             do (gnus-message 7 "nnhackernews-request-group: cand=%s" (setq cand i))
                             end
                             finally return (or cand 0))))
                 (updated-seen-index (- num-headers
                                        (nnhackernews-aif
                                            (seq-position (reverse headers) nil
                                                          (lambda (plst _e)
                                                            (not (plist-get plst :title))))
                                            it -1)))
                 (updated-seen-id (nnhackernews-aif (nth (1- updated-seen-index) headers)
                                      (plist-get it :id) ""))
                 (delta (if newsrc-seen-index
                            (max 0 (- newsrc-seen-index newsrc-seen-index-now))
                          0))
                 (newsrc-read-ranges-shifted
                  (cl-remove-if-not (lambda (e)
                                      (cond ((numberp e) (> e 0))
                                            (t (> (cdr e) 0))))
                                    (mapcar (lambda (e)
                                              (cond ((numberp e) (- e delta))
                                                    (t `(,(max 1 (- (car e) delta)) .
                                                         ,(- (cdr e) delta)))))
                                            newsrc-read-ranges))))
            (gnus-message 7 "nnhackernews-request-group: seen-id=%s          seen-index=%s -> %s"
                          newsrc-seen-id newsrc-seen-index newsrc-seen-index-now)
            (gnus-message 7 "nnhackernews-request-group: seen-id-to-be=%s seen-index-to-be=%s delta=%d"
                          updated-seen-id updated-seen-index delta)
            (gnus-message 7 "nnhackernews-request-group: read-ranges=%s shifted-read-ranges=%s"
                          newsrc-read-ranges newsrc-read-ranges-shifted)
            (gnus-info-set-read info newsrc-read-ranges-shifted)
            (gnus-info-set-marks
             info
             (append (assq-delete-all 'seen (gnus-info-marks info))
                     (list `(seen (1 . ,num-headers)))))
            (while (assq 'last-seen params)
              (gnus-alist-pull 'last-seen params))
            (gnus-info-set-params
             info
             (cons `(last-seen ,updated-seen-index . ,updated-seen-id) params)
             t)
            (gnus-set-info gnus-newsgroup-name info)
            (gnus-message 7 "nnhackernews-request-group: new info=%s" info)))))
    t))

(deffoo nnhackernews-request-scan (&optional group server)
  (nnhackernews--normalize-server)
  (unless (null group)
    (nnhackernews--with-group group
      (let* ((comments (nnhackernews-rpc-call server nil "comments" group))
             (raw-submissions (nnhackernews-rpc-call server nil "submissions" group))
             (submissions (if (zerop (length comments))
                              raw-submissions
                            (nnhackernews--filter-after
                             (- (plist-get (aref comments 0) :created_utc) 7200)
                             raw-submissions))))
        (seq-doseq (e comments)
          (nnhackernews-add-entry nnhackernews-refs-hashtb e :parent_id)) ;; :parent_id is fullname
        (seq-doseq (e (vconcat submissions comments))
          (nnhackernews-add-entry nnhackernews-authors-hashtb e :author))
        (gnus-message 5 "nnhackernews-request-scan: %s: +%s comments +%s submissions"
                      group (length comments) (length submissions))
        (nnhackernews-sort-append-headers group submissions comments)))))

(defsubst nnhackernews--make-message-id (fullname)
  "Construct a valid Gnus message id from FULLNAME."
  (format "<%s@hackernews.com>" fullname))

(defsubst nnhackernews--make-references (fullname)
  "Construct a space delimited string of message ancestors of FULLNAME."
  (mapconcat (lambda (ref) (nnhackernews--make-message-id ref))
             (nnhackernews-refs-for fullname) " "))

(defsubst nnhackernews--make-header (article-number &optional group)
  "Construct full headers of articled indexed ARTICLE-NUMBER in GROUP."
  (let* ((header (nnhackernews--get-header article-number group))
         (score (plist-get header :score))
         (num-comments (plist-get header :num_comments)))
    (make-full-mail-header
     article-number
     (or (plist-get header :title)
         (concat "Re: " (plist-get header :link_title)))
     (plist-get header :author)
     (format-time-string "%a, %d %h %Y %T %z (%Z)" (plist-get header :created_utc))
     (nnhackernews--make-message-id (plist-get header :name))
     (nnhackernews--make-references (plist-get header :name))
     0 0 nil
     (append `((X-Hackernews-Name . ,(plist-get header :name)))
             `((X-Hackernews-ID . ,(plist-get header :id)))
             (nnhackernews-aif (plist-get header :permalink)
                           `((X-Hackernews-Permalink . ,it)))
             (and (integerp score)
                  `((X-Hackernews-Score . ,(number-to-string score))))
             (and (integerp num-comments)
                  `((X-Hackernews-Comments . ,(number-to-string num-comments))))))))

(deffoo nnhackernews-request-article (article-number &optional group server buffer)
  (nnhackernews--normalize-server)
  (nnhackernews--with-group group
    (with-current-buffer (or buffer nntp-server-buffer)
      (erase-buffer)
      (let* ((header (nnhackernews--get-header article-number group))
             (mail-header (nnhackernews--make-header article-number))
             (score (cdr (assq 'X-Hackernews-Score (mail-header-extra mail-header))))
             (permalink (cdr (assq 'X-Hackernews-Permalink (mail-header-extra mail-header))))
             (body (nnhackernews--get-body (plist-get header :name) group server)))
        (when body
          (insert
           "Newsgroups: " group "\n"
           "Subject: " (mail-header-subject mail-header)  "\n"
           "From: " (or (mail-header-from mail-header) "nobody") "\n"
           "Date: " (mail-header-date mail-header) "\n"
           "Message-ID: " (mail-header-id mail-header) "\n"
           "References: " (mail-header-references mail-header) "\n"
           "Content-Type: text/html; charset=utf-8" "\n"
           (if permalink
               (format "Archived-at: <https://www.hackernews.com%s>\n" permalink)
             "")
           "Score: " score "\n"
           "\n")
          (nnhackernews-and-let*
           ((parent-name (plist-get header :parent_id)) ;; parent_id is full
            (parent-author (or (nnhackernews--gethash parent-name nnhackernews-authors-hashtb)
                               "Someone"))
            (parent-body (nnhackernews--get-body parent-name group server)))
           (insert (nnhackernews--citation-wrap parent-author parent-body)))
          (insert (nnhackernews--br-tagify body))
          (cons group article-number))))))

(deffoo nnhackernews-retrieve-headers (article-numbers &optional group server _fetch-old)
  (nnhackernews--normalize-server)
  (nnhackernews--with-group group
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (i article-numbers)
        (nnheader-insert-nov (nnhackernews--make-header i group)))
      'nov)))

(defsubst nnhackernews--earliest-among (indices lvp)
  "Return (list-to-iterate . next-earliest) from INDICES (thus-far iterators)
and LVP (list of vectors of plists).  Used in the interleaving of submissions and comments."
  (let (earliest next-earliest)
    (dolist (plst-idx
             (cl-remove-if-not #'car
                            (funcall nnhackernews--seq-map-indexed
                                     (lambda (plst idx) (cons plst idx))
                                     (seq-mapn
                                      (lambda (v i)
                                        (if (< i (length v)) (aref v i)))
                                      lvp indices)))
             (list (cdr earliest)
                   (nnhackernews-aif next-earliest
                       (plist-get (car it) :created_utc))))
      (cond ((null earliest)
             (setq earliest plst-idx))
            ((< (plist-get (car plst-idx) :created_utc)
                (plist-get (car earliest) :created_utc))
             (setq next-earliest earliest)
             (setq earliest plst-idx))
            ((null next-earliest)
             (setq next-earliest plst-idx))))))

(defun nnhackernews--sort-headers (&rest lvp)
  "Sort headers for LVP (list of vectors of plists)."
  (let* ((indices (make-list (length lvp) 0))
         result)
    (while (not (equal indices (mapcar #'length lvp)))
      (cl-destructuring-bind (to-iterate bogey-created)
          (nnhackernews--earliest-among indices lvp)
        (cl-loop with arr = (elt lvp to-iterate)
                 for j in (number-sequence (elt indices to-iterate) (1- (length arr)))
                 for plst = (aref arr j)
                 for created = (plist-get plst :created_utc)
                 until (> created (or bogey-created most-positive-fixnum))
                 do (cl-incf (elt indices to-iterate))
                 do (push plst result))))
    (nreverse result)))

(deffoo nnhackernews-close-server (&optional server)
  (nnhackernews--normalize-server)
  (condition-case err
      (progn (nnhackernews-rpc-kill server) t)
    (error
     (gnus-message 2 "nnhackernews-close-server: %s" (error-message-string err))
     nil)))

(deffoo nnhackernews-request-list (&optional server)
  (nnhackernews--normalize-server)
  (with-current-buffer nntp-server-buffer
    (let ((groups (nnhackernews-rpc-call server nil "user_subhackernewss"))
          (newsrc (cl-mapcan (lambda (info)
                               (when (and (equal "nnhackernews:" (gnus-info-method info))
                                          (<= (gnus-info-level info)
                                              gnus-level-default-subscribed))
                                 (list (gnus-info-group info))))
                             gnus-newsrc-alist)))
      (mapc (lambda (realname)
              (let ((group (gnus-group-full-name realname '("nnhackernews" (or server "")))))
                (erase-buffer)
                (gnus-message 5 "nnhackernews-request-list: scanning %s..." realname)
                (gnus-activate-group group t)
                (gnus-message 5 "nnhackernews-request-list: scanning %s...done" realname)
                (gnus-group-unsubscribe-group group gnus-level-default-subscribed t)
                (setq newsrc (cl-remove group newsrc :test #'string=))))
            groups)
      (mapc (lambda (fullname)
              (gnus-message 4 "nnhackernews-request-list: missing subscription %s" fullname)
              (nnhackernews-rpc-call nil nil "subscribe" (gnus-group-real-name fullname))
              (gnus-activate-group fullname t))
            newsrc)
      (erase-buffer)
      (mapc (lambda (group)
              (insert (format "%s %d 1 y\n" group
                              (length (nnhackernews-get-headers group)))))
            groups)))
  t)

(defun nnhackernews-sentinel (process event)
  "Wipe headers state when PROCESS dies from EVENT."
  (unless (string= "open" (substring event 0 4))
    (gnus-message 2 "nnhackernews-sentinel: process %s %s"
                  (car (process-command process))
                  (replace-regexp-in-string "\n$" "" event))
    (setq nnhackernews-headers-hashtb (gnus-make-hashtable))
    (gnus-backlog-shutdown)))

(defun nnhackernews--message-user (server beg end _prev-len)
  "Message SERVER related alert with `buffer-substring' from BEG to END."
  (let ((string (buffer-substring beg end))
        (magic "::user::"))
    (when (string-prefix-p magic string)
      (message "%s: %s" server (substring string (length magic))))))

(defsubst nnhackernews--install-failed ()
  "If we can't install the virtualenv then all bets are off."
  (string= nnhackernews-venv "/dev/null"))

(defun nnhackernews-rpc-get (&optional server)
  "Retrieve the PRAW process for SERVER."
  (nnhackernews--normalize-server)
  (unless (nnhackernews--install-failed)
    (let ((proc (get-buffer-process (get-buffer-create (format " *%s*" server)))))
      (unless proc
        (let* ((nnhackernews-el-dir (directory-file-name (file-name-directory (locate-library "nnhackernews"))))
               (nnhackernews-py-dir (directory-file-name
                                 (if (string= "lisp" (file-name-base nnhackernews-el-dir))
                                     (file-name-directory nnhackernews-el-dir)
                                   nnhackernews-el-dir)))
               (python-shell-extra-pythonpaths (list nnhackernews-py-dir))
               (process-environment (python-shell-calculate-process-environment))
               (python-executable (if nnhackernews-venv
                                      (format "%s/bin/python" nnhackernews-venv)
                                    (executable-find nnhackernews-python-command)))
               (python-module (if (featurep 'nnhackernews-test) "tests" "nnhackernews"))
               (praw-command (append (list python-executable "-m" python-module)
                                     nnhackernews--python-module-extra-args)))
          (unless (featurep 'nnhackernews-test)
            (setq praw-command (append praw-command (list "--localhost" nnhackernews-localhost)))
            (when nnhackernews-log-rpc
              (setq nnhackernews-rpc-log-filename
                    (concat (file-name-as-directory temporary-file-directory)
                            "nnhackernews-rpc-log."))
              (setq praw-command (append praw-command
                                         (list "--log" nnhackernews-rpc-log-filename)))))
          (setq proc (make-process :name server
                                   :buffer (get-buffer-create (format " *%s*" server))
                                   :command praw-command
                                   :connection-type 'pipe
                                   :noquery t
                                   :sentinel #'nnhackernews-sentinel
                                   :stderr (get-buffer-create (format " *%s-stderr*" server))))
          (with-current-buffer (get-buffer-create (format " *%s-stderr*" server))
            (add-hook 'after-change-functions
                      (apply-partially 'nnhackernews--message-user server)
                      nil t)))
        (push proc nnhackernews-processes))
      proc)))

(defun nnhackernews-rpc-request (connection kwargs method &rest args)
  "Send to CONNECTION a request with generator KWARGS calling METHOD ARGS.

Library `json-rpc--request' assumes HTTP transport which jsonrpyc does not, so we make our own."
  (unless (hash-table-p kwargs)
    (setq kwargs #s(hash-table)))
  (let* ((id (cl-incf (json-rpc-id-counter connection)))
         (request `(:method ,method
                    :id ,id
                    :params (:args ,(apply json-array-type args) :kwargs ,kwargs)))
         (proc (json-rpc-process (json-rpc-ensure connection)))
         (encoded (json-encode (append '(:jsonrpc "2.0") request)))
         (json-object-type 'plist)
         (json-key-type 'keyword)
         (iteration-seconds 6))
    (with-current-buffer (process-buffer proc)
      (erase-buffer)
      (gnus-message 7 "nnhackernews-rpc-request: send %s" encoded)
      (process-send-string proc (concat encoded "\n"))
      (cl-loop repeat (/ nnhackernews-rpc-request-timeout iteration-seconds)
               with result
               until (or (not (json-rpc-live-p connection))
                         (and (not (zerop (length (buffer-string))))
                              (condition-case err
                                  (setq result (json-read-from-string (buffer-string)))
                                (error
                                 (let* ((resp (if (< (length (buffer-string)) 100)
                                                  (buffer-string)
                                                (format "%s...%s"
                                                        (cl-subseq (buffer-string) 0 50)
                                                        (cl-subseq (buffer-string) -50)))))
                                   (setq result
                                         `(:error ,(format "%s on %s"
                                                           (error-message-string err)
                                                           resp))))
                                 nil))))
               do (accept-process-output proc iteration-seconds 0)
               finally return
               (cond ((null result)
                      (error "nnhackernews-rpc-request: response timed out"))
                     ((plist-get result :error)
                      (error "nnhackernews-rpc-request: %s" (plist-get result :error)))
                     (t
                      (gnus-message 7 "nnhackernews-rpc-request: recv ...%s"
                                    (cl-subseq (buffer-string)
                                               (- (min (length (buffer-string)) 50))))
                      (plist-get result :result)))))))

(defsubst nnhackernews--extract-name (from)
  "String match on something looking like t1_es076hd in FROM."
  (and (stringp from) (string-match "\\(t[0-9]+_[a-z0-9]+\\)" from) (match-string 1 from)))

;; C-c C-c from followup buffer
;; message-send-and-exit
;; message-send
;; message-send-method-alist=message-send-news-function=message-send-news
;; gnus-request-post
;; nnhackernews-request-post
(deffoo nnhackernews-request-post (&optional server)
  (nnhackernews--normalize-server)
  (let* ((ret t)
         (kwargs (make-hash-table))
         (title (or (message-fetch-field "Subject") (error "No Subject field")))
         (link (message-fetch-field "Link"))
         (reply-p (not (null message-reply-headers)))
         (edit-name (nnhackernews--extract-name (message-fetch-field "Supersedes")))
         (cancel-name (nnhackernews--extract-name (message-fetch-field "Control")))
         (root-p (message-fetch-field "Reply-Root"))
         (article-number (cdr gnus-article-current))
         (group (if (numberp article-number)
                    (gnus-group-real-name (car gnus-article-current))
                  (or (message-fetch-field "Newsgroups") (error "No Newsgroups field"))))
         (header (when (numberp article-number)
                   (nnhackernews--get-header article-number group)))
         (body
          (save-excursion
            (save-restriction
              (message-goto-body)
              (narrow-to-region (point) (point-max))
              (buffer-string)))))
    (cond (cancel-name (nnhackernews-rpc-call server nil "delete" cancel-name))
          (edit-name (nnhackernews-rpc-call server nil "edit" edit-name body))
          (reply-p (nnhackernews-rpc-call server nil "reply"
                                      (plist-get header :name)
                                      body (stringp root-p)))
          (link (let* ((parsed-url (url-generic-parse-url link))
                       (host (url-host parsed-url)))
                  (if (and (stringp host) (not (zerop (length host))))
                      (progn
                        (puthash 'url link kwargs)
                        (nnhackernews-rpc-call server kwargs "submit" group title))
                    ;; gnus-error might be better here
                    (error "nnhackernews-request-post: invalid url \"%s\"" link)
                    (setq ret nil))))
          (t (puthash 'selftext body kwargs)
             (nnhackernews-rpc-call server kwargs "submit" group title)))
    ret))

(add-to-list 'gnus-parameters `("^nnhackernews"
                                (gnus-summary-make-false-root 'adopt)
                                (gnus-cite-hide-absolute 5)
                                (gnus-cite-hide-percentage 0)
                                (gnus-cited-lines-visible '(2 . 2))
                                (gnus-auto-extend-newsgroup nil)
                                (gnus-add-timestamp-to-message t)
                                (gnus-header-button-alist
                                 (quote ,(cons '("^\\(Message-I[Dd]\\|^In-Reply-To\\):" "<[^<>]+>"
                                           0 (>= gnus-button-message-level 0)
                                           gnus-button-message-id 0)
                                         (cdr gnus-header-button-alist))))
                                (gnus-visible-headers ,(concat gnus-visible-headers "\\|^Score:"))))

(nnoo-define-skeleton nnhackernews)

(defun nnhackernews-article-mode-activate ()
  "Augment the `gnus-article-mode-map' conditionally."
  (when (nnhackernews--gate)
    (nnhackernews-article-mode)))

(defun nnhackernews-summary-mode-activate ()
  "Shadow some bindings in `gnus-summary-mode-map' conditionally."
  (when (nnhackernews--gate)
    (nnhackernews-summary-mode)))

(defun nnhackernews-group-mode-activate ()
  "Augment the `gnus-group-mode-map' unconditionally."
  (setq gnus-group-change-level-function 'nnhackernews-update-subscription)
  (nnhackernews-group-mode))

;; I believe I did try buffer-localizing hooks, and it wasn't sufficient
(add-hook 'gnus-article-mode-hook 'nnhackernews-article-mode-activate)
(add-hook 'gnus-group-mode-hook 'nnhackernews-group-mode-activate)
(add-hook 'gnus-summary-mode-hook 'nnhackernews-summary-mode-activate)

;; `gnus-newsgroup-p' requires valid method post-mail to return t
(add-to-list 'gnus-valid-select-methods '("nnhackernews" post-mail) t)

;; Add prompting for replying to thread root to gnus-summary-followup.
;; The interactive spec of gnus-summary-followup is putatively preserved.
(let* ((prompt-loose
        (lambda (f &rest args)
          (cond ((nnhackernews--gate)
                 (or (nnhackernews-and-let*
                      ((article-number (gnus-summary-article-number))
                       (header (nnhackernews--get-header article-number))
                       (root-name (car (nnhackernews-refs-for (plist-get header :name))))
                       (rootless (or (not (stringp root-name))
                                     (not (string-prefix-p "t3_" root-name))
                                     (not (nnhackernews-find-header
                                           (gnus-group-real-name gnus-newsgroup-name)
                                           (nnhackernews-hack-name-to-id root-name)))))
                       (reply-root (read-char-choice
                                    "Reply loose thread [m]essage or [r]oot: " '(?m ?r)))
                       ((eq reply-root ?r)))
                      (let* ((link-header (apply-partially #'message-add-header
                                                           "Reply-Root: yes"))
                             (add-link-header (apply-partially #'add-hook
                                                               'message-header-setup-hook
                                                               link-header))
                             (remove-link-header (apply-partially #'remove-hook
                                                                  'message-header-setup-hook
                                                                  link-header)))
                        (funcall add-link-header)
                        (condition-case err
                            (progn
                              (apply f args)
                              (funcall remove-link-header))
                          (error (funcall remove-link-header)
                                 (error (error-message-string err)))))
                      t)
                     (apply f args)))
                (t (apply f args)))))
       (advise-gnus-summary-followup
        (lambda ()
          (add-function :around (symbol-function 'gnus-summary-followup) prompt-loose)))
       (suspend-prompt-loose
        (lambda (f &rest args)
          (cond ((nnhackernews--gate)
                 (remove-function (symbol-function 'gnus-summary-followup) prompt-loose)
                 (condition-case err
                     (prog1 (apply f args)
                       (funcall advise-gnus-summary-followup))
                   (error (funcall advise-gnus-summary-followup)
                          (error (error-message-string err)))))
                (t (apply f args)))))
       (advise-gnus-summary-cancel-article
        (lambda ()
          (add-function :around (symbol-function 'gnus-summary-cancel-article)
                        suspend-prompt-loose))))
  (funcall advise-gnus-summary-cancel-article)
  (funcall advise-gnus-summary-followup))

(add-function
 :around (symbol-function 'message-supersede)
 (lambda (f &rest args)
   (cond ((nnhackernews--gate)
          (add-function :override
                        (symbol-function 'mml-insert-mml-markup)
                        'ignore)
          (condition-case err
              (prog1 (apply f args)
                (remove-function (symbol-function 'mml-insert-mml-markup) 'ignore)
                (save-excursion
                  (save-restriction
                    (message-replace-header "From" (message-make-from))
                    (message-goto-body)
                    (narrow-to-region (point) (point-max))
                    (goto-char (point-max))
                    (mm-inline-text-html nil)
                    (delete-region (point-min) (point)))))
            (error (remove-function (symbol-function 'mml-insert-mml-markup) 'ignore)
                   (error (error-message-string err)))))
         (t (apply f args)))))

(add-function
 :around (symbol-function 'message-send-news)
 (lambda (f &rest args)
   (cond ((nnhackernews--gate)
          (let* ((dont-ask (lambda (prompt)
                             (when (cl-search "mpty article" prompt) t)))
                 (link-p (not (null (message-fetch-field "Link"))))
                 (message-shoot-gnksa-feet (if link-p t message-shoot-gnksa-feet)))
            (condition-case err
                (progn
                  (when link-p
                    (add-function :before-until (symbol-function 'y-or-n-p) dont-ask))
                  (prog1 (apply f args)
                    (remove-function (symbol-function 'y-or-n-p) dont-ask)))
              (error (remove-function (symbol-function 'y-or-n-p) dont-ask)
                     (error (error-message-string err))))))
         (t (apply f args)))))

(add-function
 :around (symbol-function 'gnus-summary-post-news)
 (lambda (f &rest args)
   (cond ((nnhackernews--gate)
          (let* ((nnhackernews-post-type (read-char-choice "[l]ink / [t]ext: " '(?l ?t)))
                 (link-header (apply-partially #'message-add-header "Link: https://"))
                 (add-link-header (apply-partially #'add-hook
                                                   'message-header-setup-hook
                                                   link-header))
                 (remove-link-header (apply-partially #'remove-hook
                                                      'message-header-setup-hook
                                                      link-header)))
            (cl-case nnhackernews-post-type
              (?l (funcall add-link-header)))
            (condition-case err
                (progn
                  (apply f args)
                  (funcall remove-link-header))
              (error (funcall remove-link-header)
                     (error (error-message-string err))))))
         (t (apply f args)))))

(add-function
 :filter-return (symbol-function 'message-make-fqdn)
 (lambda (val)
   (if (and (nnhackernews--gate)
            (cl-search "--so-tickle-me" val))
       "hackernews.com" val)))

(add-function
 :before-until (symbol-function 'message-make-from)
 (lambda (&rest _args)
   (when (nnhackernews--gate)
     (concat (nnhackernews-rpc-call nil nil "user_attr" "name") "@hackernews.com"))))

(add-function
 :around (symbol-function 'message-is-yours-p)
 (lambda (f &rest args)
   (let ((concat-func (lambda (f &rest args)
                       (let ((fetched (apply f args)))
                         (if (string= (car args) "from")
                             (concat fetched "@hackernews.com")
                           fetched)))))
     (when (nnhackernews--gate)
       (add-function :around
                     (symbol-function 'message-fetch-field)
                     concat-func))
     (condition-case err
         (prog1 (apply f args)
           (remove-function (symbol-function 'message-fetch-field) concat-func))
       (error (remove-function (symbol-function 'message-fetch-field) concat-func)
              (error (error-message-string err)))))))

;; disallow caching as the article numbering is wont to change
;; after PRAW restarts!
(setq gnus-uncacheable-groups
      (nnhackernews-aif gnus-uncacheable-groups
          (format "\\(%s\\)\\|\\(^nnhackernews\\)" it)
        "^nnhackernews"))

(provide 'nnhackernews)

;;; nnhackernews.el ends here
