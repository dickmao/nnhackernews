;;; nnhackernews.el --- Gnus backend for Hacker News  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019 The Authors of nnhackernews.el

;; Authors: dickmao <github id: dickmao>
;; Version: 0.1.0
;; Keywords: news
;; URL: https://github.com/dickmao/nnhackernews
;; Package-Requires: ((emacs "25") (request "20190730") (dash "20190401") (anaphora "20180618"))

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

;; A Gnus backend for Hacker News.

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
(require 'gnus-score)
(require 'mm-url)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'request)
(require 'dash)
(require 'anaphora)
(require 'url-http)

(nnoo-declare nnhackernews)

(defconst nnhackernews--group-ask "ask")
(defconst nnhackernews--group-show "show")
(defconst nnhackernews--group-job "job")
(defconst nnhackernews--group-stories "news")
(defconst nnhackernews--group-comments "comments")

(defcustom nnhackernews-render-story t
  "If non-nil, follow link upon `gnus-summary-select-article'.

Otherwise, just display link."
  :type 'boolean
  :group 'nnhackernews)

(defcustom nnhackernews-localhost "127.0.0.1"
  "Some users keep their browser in a separate domain.

Do not set this to \"localhost\" as a numeric IP is required for the oauth handshake."
  :type 'string
  :group 'nnhackernews)

(defcustom nnhackernews-max-items-per-scan 100
  "Maximum items to fetch from firebase each refresh cycle."
  :type 'integer
  :group 'nnhackernews)

(defvar nnhackernews--last-item nil "Keep track of where we are.")

(defvar nnhackernews--debug-request-items nil "Keep track of ids to re-request for testing.")

(defvar nnhackernews--last-scan-time (truncate (float-time))
  "Don't scan more than once every few seconds.")

(defmacro nnhackernews--callback (result &optional callback)
  "Set RESULT to return value of CALLBACK."
  `(cl-function (lambda (&rest args &key data &allow-other-keys)
                  (setq ,result (if ,callback
                                    (apply ,callback args)
                                  data)))))

(defsubst nnhackernews--gethash (string hashtable &optional dflt)
  "Get corresponding value of STRING from HASHTABLE, or DFLT if undefined.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  (if (fboundp 'gnus-gethash)
      (let ((sym (intern-soft string hashtable)))
        (if (or (null sym) (not (boundp sym))) dflt (symbol-value sym)))
    (gethash string hashtable dflt)))

(defsubst nnhackernews--replace-hash (string func hashtable)
  "Set value of STRING to FUNC applied to existing STRING value in HASHTABLE.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  (let* ((capture (nnhackernews--gethash string hashtable))
         (replace-with (funcall func capture)))
    (if (fboundp 'gnus-sethash)
       (set (intern string hashtable) replace-with)
      (puthash string replace-with hashtable))))

(defmacro nnhackernews--sethash (string value hashtable)
  "Set corresponding value of STRING to VALUE in HASHTABLE.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  `(,(if (fboundp 'gnus-sethash)
         'gnus-sethash
       'puthash)
    ,string ,value ,hashtable))

(defmacro nnhackernews--maphash (func table)
  "Map FUNC over TABLE, return nil.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  `(,(if (fboundp 'gnus-gethash-safe)
         'mapatoms
       'maphash)
    ,(if (fboundp 'gnus-gethash-safe)
         `(lambda (k) (funcall (apply-partially ,func (gnus-gethash-safe k ,table)) k))
       func)
    ,table))

;; keymaps made by `define-prefix-command' in `gnus-define-keys-1'
(defvar nnhackernews-article-mode-map)

;; keymaps I make myself
(defvar nnhackernews-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" 'gnus-summary-followup)
    (define-key map "R" 'gnus-summary-followup-with-original)
    (define-key map "F" 'gnus-summary-followup-with-original)
    map))

(define-minor-mode nnhackernews-article-mode
  "Minor mode for nnhackernews articles.  Disallow `gnus-article-reply-with-original'.

\\{gnus-article-mode-map}
"
  :lighter " HN"
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
  :lighter " HN"
  :keymap nnhackernews-summary-mode-map)

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

(defvar nnhackernews-location-hashtb (gnus-make-hashtable)
  "Id -> ( group . index ).")

(defvar nnhackernews-root-hashtb (gnus-make-hashtable)
  "Id -> possibly ancient header.")

(defvar nnhackernews-headers-hashtb (gnus-make-hashtable)
  "Group -> headers.")

(defvar nnhackernews-refs-hashtb (gnus-make-hashtable)
  "Who replied to whom (global over all entries).")

(defvar nnhackernews-authors-hashtb (gnus-make-hashtable)
  "For fast lookup of parent-author (global over all entries).")

(defsubst nnhackernews-get-headers (group)
  "List headers from GROUP."
  (nnhackernews--gethash group nnhackernews-headers-hashtb))

(defun nnhackernews-find-header (id &optional noquery)
  "Retrieve property list of ID.

If NOQUERY, return nil and avoid querying if not extant."
  (if-let ((location (nnhackernews--gethash id nnhackernews-location-hashtb)))
      (cl-destructuring-bind (group . index) location
        (nnhackernews--get-header (1+ index) group))
    (unless noquery
      (nnhackernews--request-item id))))

(defsubst nnhackernews-refs-for (id)
  "Return descending ancestors as list for ID."
  (cl-loop with root-plst
           for prev-id = id then cur-id
           for cur-id =
           (let ((cached (nnhackernews--gethash prev-id
                                                nnhackernews-refs-hashtb
                                                'NULL)))
             (if (eq cached 'NULL)
                 (progn (setq root-plst (nnhackernews--request-item prev-id))
                        (nnhackernews-add-entry nnhackernews-refs-hashtb
                                                root-plst :parent)
                        (plist-get root-plst :parent))
               (setq root-plst (nnhackernews-find-header prev-id t))
               cached))
           until (null cur-id)
           collect cur-id into rresult
           finally do
           (let ((result (nreverse rresult)))
             (when (and result
                        (string= (plist-get root-plst :id) (car result)))
               (nnhackernews--sethash (car result) root-plst
                                      nnhackernews-root-hashtb))
             (cl-return result))))

(defun nnhackernews--retrieve-root (header)
  "Retrieve and cache property list HEADER root."
  (if-let ((root-id (car (nnhackernews-refs-for (plist-get header :id)))))
      (nnhackernews--gethash root-id nnhackernews-root-hashtb)
    header))

(defun nnhackernews--group-for (header)
  "Classify HEADER as one of ask, show, or news based on title."
  (let* ((root-plst (nnhackernews--retrieve-root header))
         (title (or (plist-get header :link_title) ""))
         (type (or (plist-get root-plst :type) "")))
    ;; string-match-p like all elisp searching is case-insensitive
    (cond ((string= type "job") nnhackernews--group-job)
          ((string-match-p "^\\(Launch\\|Show\\) HN" title) nnhackernews--group-show)
          ((string-match-p "^\\(Ask\\|Tell\\) HN" title) nnhackernews--group-ask)
          (t nnhackernews--group-stories))))

(defsubst nnhackernews--who-am-i ()
  "Get my Hacker News username."
  (when-let ((user-cookie
              (cdr (assoc-string
                    "user"
                    (cl-loop with result
                             for securep in '(t nil)
                             do (setq result
                                      (request-cookie-alist "news.ycombinator.com"
                                                            "/" securep))
                             until result
                             finally return result)))))
    (car (split-string user-cookie "&"))))

(defsubst nnhackernews--append-header (plst &optional group)
  "Update data structures for PLST \"header\".

If GROUP classification omitted, figure it out."
  (let* ((id (plist-get plst :id))
         (group (or group (nnhackernews--group-for plst))))
    (nnhackernews--sethash id
                           (cons group (length (nnhackernews-get-headers group)))
                           nnhackernews-location-hashtb)
    (nnhackernews--sethash group (nconc (nnhackernews-get-headers group) (list plst))
                           nnhackernews-headers-hashtb)
    plst))

(nnoo-define-basics nnhackernews)

(defun nnhackernews-vote-current-article (vote)
  "VOTE is +1, -1, 0."
  (unless gnus-article-current ;; gnus-article-current or gnus-current-article?
    (error "No current article"))
  (unless gnus-newsgroup-name
    (error "No current newgroup"))
  (let* ((header (nnhackernews--get-header
                  (cdr gnus-article-current)
                  (gnus-group-real-name (car gnus-article-current))))
         (orig-score (format "%s" (plist-get header :score)))
         (new-score (if (zerop vote) orig-score
                      (concat orig-score " "
                              (if (> vote 0) "+" "")
                              (format "%s" vote)))))
    (let ((inhibit-read-only t))
      (nnheader-replace-header "score" new-score))
    (nnhackernews--request-vote (plist-get header :id) vote)))

(defsubst nnhackernews--gate (&optional group)
  "Apply our minor modes only when the following conditions hold for GROUP."
  (unless group
    (setq group gnus-newsgroup-name))
  (and (stringp group)
       (listp (gnus-group-method group))
       (eq 'nnhackernews (car (gnus-group-method group)))))

(deffoo nnhackernews-request-close ()
  (nnhackernews-close-server)
  t)

(deffoo nnhackernews-request-type (_group &optional _article)
  'news)

(deffoo nnhackernews-server-opened (&optional server)
  (nnhackernews--normalize-server)
  t)

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

(defun nnhackernews--get-body (header &optional server)
  "Get full text of submission or comment HEADER at SERVER."
  (nnhackernews--normalize-server)
  (if-let ((url (plist-get header :url)))
      (format "<div><p><a href=\"%s\">%s</a></div>" url url)
    (or (plist-get header :text) "")))

(defun nnhackernews--massage (body)
  "Precede each quoted line of BODY broken by `shr-fill-line' with '>'."
  (with-temp-buffer
    (insert body)
    (mm-url-decode-entities)
    (cl-loop initially (goto-char (point-min))
             until (and (null (re-search-forward "\\(^>\\( .*?\\)\\)<p>" nil t))
                        (null (re-search-forward "\\(<p>\\s-*>\\( .*?\\)\\)<p>" nil t)))
             do (let* ((start (match-beginning 1))
                       (end (match-end 1))
                       (matched (match-string 2)))
                  (perform-replace
                   ".*"
                   (concat "<p>\n"
                           (with-temp-buffer
                             (insert matched)
                             (fill-region (point-min) (point-max))
                             (insert
                              (prog1
                                  (cl-subseq (replace-regexp-in-string
                                              "\n" "<br>\n> " (concat "\n" (buffer-string)))
                                             5)
                                (erase-buffer)))
                             (buffer-string))
                           "\n")
                   nil t nil nil nil start end)))
    (buffer-string)))

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
      (concat author " wrote:<p>\n"
              "<pre>\n"
              (cl-subseq (replace-regexp-in-string "\n" "\n> " (concat "\n" trimmed)) 1)
              "\n</pre><p>"))))

(defun nnhackernews-add-entry (hashtb e field)
  "Add to HASHTB the pair consisting of entry E's name to its FIELD."
  (nnhackernews--sethash (plist-get e :id) (plist-get e field) hashtb))

(defsubst nnhackernews--score-unread (group)
  "Avoid having to select the GROUP to make the unread number go down."
  (nnhackernews--with-group group
    (let* ((group-full-name (gnus-group-full-name group "nnhackernews:"))
           (unread (gnus-group-unread group-full-name)))
      (when (and (numberp unread) (> unread 0))
        (save-excursion
          (ignore-errors
            (let ((gnus-auto-select-subject nil))
              (gnus-summary-read-group group-full-name nil t)))
          (let ((gnus-summary-next-group-on-exit nil))
            (gnus-summary-exit)))))))

(deffoo nnhackernews-request-group-scan (group &optional server _info)
  "M-g from *Group* calls this."
  (nnhackernews--normalize-server)
  (nnhackernews--with-group group
    (gnus-message 5 "nnhackernews-request-group-scan: scanning %s..." group)
    (gnus-activate-group (gnus-group-full-name group '("nnhackernews" (or server ""))) t)
    (gnus-message 5 "nnhackernews-request-group-scan: scanning %s...done" group)
    (nnhackernews--score-unread group)
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
           (newsrc-seen-id (cdr newsrc-seen-cons))
           (headers (nnhackernews-get-headers group))
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
                        (zerop (string-to-number newsrc-seen-id)))
                    1
                  (cl-loop with cand = nil
                           for plst in headers
                           for i = 1 then (1+ i)
                           if (= (string-to-number (plist-get plst :id))
                                 (string-to-number newsrc-seen-id))
                           do (gnus-message 7 "nnhackernews-request-group: exact=%s" i)
                           and return i ;; do not go to finally
                           end
                           if (and (null cand)
                                   (> (string-to-number (plist-get plst :id))
                                      (string-to-number newsrc-seen-id)))
                           do (gnus-message 7 "nnhackernews-request-group: cand=%s" (setq cand i))
                           end
                           finally return (or cand 0))))
               (updated-seen-index (- num-headers
                                      (aif
                                          (seq-position (reverse headers) nil
                                                        (lambda (plst _e)
                                                          (not (plist-get plst :title))))
                                          it -1)))
               (updated-seen-id (aif (nth (1- updated-seen-index) headers)
                                    (plist-get it :id) nil))
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
          (when updated-seen-id
            (while (assq 'last-seen params)
              (gnus-alist-pull 'last-seen params))
            (gnus-info-set-params
             info
             (cons `(last-seen ,updated-seen-index . ,updated-seen-id) params)
             t))
          (gnus-set-info gnus-newsgroup-name info)
          (gnus-message 7 "nnhackernews-request-group: new info=%S" info))))
    t))

(defsubst nnhackernews--json-read ()
  "Copied from ein:json-read() by tkf."
  (goto-char (point-max))
  (backward-sexp)
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (json-read)))

(defun nnhackernews--request-max-item ()
  "Retrieve the max-item from which all read-unread accounting stems."
  (let (max-item)
    (nnhackernews--request
     "nnhackernews--request-max-item"
     "https://hacker-news.firebaseio.com/v0/maxitem.json"
     :parser 'nnhackernews--json-read
     :success (nnhackernews--callback max-item))
    max-item))

(defun nnhackernews--request-newstories ()
  "Return list of id's which we know to be stories (as opposed to comments)."
  (let (stories)
    (nnhackernews--request
     "nnhackernews--request-newstories"
     "https://hacker-news.firebaseio.com/v0/newstories.json"
     :success (nnhackernews--callback
               stories
               (cl-function
                (lambda (&key data &allow-other-keys)
                  ;; manual recommended listify appends nil
                  (append
                   (eval (car (read-from-string (subst-char-in-string ?, ?\  data))))
                   nil)))))
    stories))

(cl-defun nnhackernews--request (caller url
                                 &rest attributes &key parser (backend 'url-retrieve)
                                 &allow-other-keys)
  "Prefix errors with CALLER when executing synchronous request to URL."
  (unless parser
    (setq attributes (nconc attributes (list :parser #'buffer-string))))
  (setq attributes (cl-loop for (k v) on attributes by (function cddr)
                            unless (eq k :backend)
                            collect k and collect v))
  (let ((request-backend backend))
    (apply #'request url
           :sync t
           :error (apply-partially #'nnhackernews--request-error caller)
           attributes)))

(defsubst nnhackernews--domify (html)
  "Parse HTML into dom."
  (with-temp-buffer
    (insert html)
    (when (fboundp 'libxml-parse-html-region)
      (libxml-parse-html-region (point-min) (point-max)))))

(cl-defun nnhackernews--request-vote-success (&key data response &allow-other-keys)
  "If necessary, login first, then return plist of :fnid and :fnop."
  (let* ((dom (nnhackernews--domify data))
         (form (alist-get 'form (alist-get 'body dom)))
         (login-p (string= "vote" (alist-get 'action (car form))))
         hidden)
    (when login-p
      (-tree-map-nodes
       (lambda (x)
         (and (listp x)
              (eq (car x) 'input)
              (string= "hidden" (alist-get 'type (cl-second x)))))
       (lambda (x)
         (!cons (alist-get 'value (cl-second x)) hidden)
         (!cons (intern (concat ":" (alist-get 'name (cl-second x)))) hidden))
       form)
      (setq dom (nnhackernews--domify
                 (nnhackernews--request-form (request-response-url response) hidden))))))

(defun nnhackernews--request-vote (id _vote)
  "Tally for ID the VOTE."
  (let (result)
    (nnhackernews--request
     "nnhackernews--request-vote"
     (format "https://news.ycombinator.com/vote?%s"
             (url-build-query-string
              `((id ,id) (how "up") (goto ,(format "item?id=%s" id)))))
     :backend 'curl
     :success (nnhackernews--callback result #'nnhackernews--request-vote-success))
    result))

(defun nnhackernews--request-reply (_id _body _root-p)
  "Reply to ID with BODY."
)

(defun nnhackernews--request-edit (_id _body)
  "Replace body of ID with BODY."
)

(defun nnhackernews--request-delete (_id)
  "Cancel ID."
)

(defsubst nnhackernews--enforce-curl ()
  "Curl must exist."
  (unless (executable-find "curl")
    (error "nnhackernews--enforce-curl: the 'curl' program was not found")))

(cl-defun nnhackernews--request-error (caller
                                       &key response symbol-status error-thrown
                                       &allow-other-keys
                                       &aux (response-status
                                             (request-response-status-code response)))
  "Refer to CALLER when reporting a submit error."
  (gnus-message 3 "%s %s: http status %s, %s" caller symbol-status response-status
                (error-message-string error-thrown)))

(defun nnhackernews--request-submit-link (title link hidden)
  "Submit TITLE with LINK and HIDDEN."
  (nnhackernews--enforce-curl)
  (let (result)
    (nnhackernews--request
     "nnhackernews--request-submit-link"
     "https://news.ycombinator.com/submit"
     :backend 'curl
     :data (append (cl-loop for (k v) on hidden by (function cddr)
                            collect (cons (cl-subseq (symbol-name k) 1) v))
                   `(("title" . ,title)
                     ("url" . ,link)))
     :success (nnhackernews--callback result))
    result))

(defun nnhackernews--request-submit-text (text hidden)
  "Submit TEXT and HIDDEN."
  (nnhackernews--enforce-curl)
  (let (result)
    (nnhackernews--request
     "nnhackernews--request-submit-text"
     "https://news.ycombinator.com/submit"
     :backend 'curl
     :data (append (cl-loop for (k v) on hidden by (function cddr)
                            collect (cons (cl-subseq (symbol-name k) 1) v))
                   `(("text" . ,text)))
     :success (nnhackernews--callback result))
    result))

(defun nnhackernews--request-item (id)
  "Retrieve ID as a property list."
  (push id nnhackernews--debug-request-items)
  (let ((utf-decoder (lambda (x)
                       (decode-coding-string (with-temp-buffer
                                               (set-buffer-multibyte nil)
                                               (insert x)
                                               (buffer-string))
                                             'utf-8)))
        plst)
    (add-function :filter-return (symbol-function 'json-read-string) utf-decoder)
    (nnhackernews--request
     "nnhackernews--request-item"
     (format "https://hacker-news.firebaseio.com/v0/item/%s.json" id)
     :parser 'nnhackernews--json-read
     :success (nnhackernews--callback plst))
    (remove-function (symbol-function 'json-read-string) utf-decoder)
    (when-let ((id (plist-get plst :id)))
      (setq plst (plist-put plst :id (number-to-string id)))
      (when-let (parent (plist-get plst :parent))
        (setq plst (plist-put plst :parent (number-to-string parent))))
      (unless (plist-get plst :score)
        (setq plst (plist-put plst :score 0)))
      plst)))

(defun nnhackernews--select-items (start-item max-item &optional static-newstories)
  "Return a list of items to retrieve between START-ITEM and MAX-ITEM.

STATIC-NEWSTORIES can be provided in lieu of querying out.

Since we are constrained by `nnhackernews-max-items-per-scan', we prioritize
stories and may throw away comments, etc."
  (mapcar
   #'number-to-string
   (if (> (1+ (- max-item start-item)) nnhackernews-max-items-per-scan)
       (let* ((stories (seq-take-while
                        (lambda (x) (>= x start-item))
                        (or static-newstories (nnhackernews--request-newstories))))
              (excess (- nnhackernews-max-items-per-scan (length stories))))
         (if (<= excess 0)
             (nreverse (cl-subseq stories 0 nnhackernews-max-items-per-scan))
           (cl-loop with excess-count = 0
                    with j = 0
                    for i from max-item downto start-item by 1
                    until (or (>= excess-count excess) (>= j (length stories)))
                    if (= i (elt stories j))
                    do (cl-incf j)
                    else
                    do (cl-incf excess-count)
                    end
                    collect i into result
                    finally return (nreverse (append result (nthcdr j stories)
                                                     (cl-loop for k from 0 below
                                                              (- excess excess-count) by 1
                                                              collect (- i k)))))))
     (cl-loop for i from start-item to max-item by 1
              collect i))))

(defun nnhackernews--incoming (&optional static-max-item static-newstories)
  "Drink from the firehose.

Optionally provide STATIC-MAX-ITEM and STATIC-NEWSTORIES to prevent querying out."
  (interactive)
  (setq nnhackernews--debug-request-items nil)
  (when-let ((max-item (or static-max-item (nnhackernews--request-max-item))))
    (let* ((start-item (1+ (or nnhackernews--last-item
                               (- max-item nnhackernews-max-items-per-scan))))
           (counts (gnus-make-hashtable))
           (items (nnhackernews--select-items start-item max-item static-newstories)))
      (dolist (item items)
        (-when-let* ((plst (nnhackernews--request-item item))
                     (not-deleted (not (plist-get plst :deleted)))
                     (type (plist-get plst :type)))
          (nnhackernews-add-entry nnhackernews-refs-hashtb plst :parent)
          (nnhackernews-add-entry nnhackernews-authors-hashtb plst :by)
          (nnhackernews--replace-hash type (lambda (x) (1+ (or x 0))) counts)
          (setq plst (plist-put plst :link_title
                                (or (plist-get
                                     (nnhackernews--retrieve-root plst)
                                     :title) "")))
          (cl-case (intern type)
            (job (nnhackernews--append-header plst nnhackernews--group-job))
            ((story comment) (nnhackernews--append-header plst))
            (otherwise (gnus-message 5 "nnhackernews-incoming: ignoring type %s" type)))))
      (setq nnhackernews--last-item max-item)
      (gnus-message
       5 (concat "nnhackernews--incoming: "
                 (format "%d requests, " (length nnhackernews--debug-request-items))
                 (let ((result ""))
                   (nnhackernews--maphash
                    (lambda (key value)
                      (setq result (concat result (format "%s +%s " value key))))
                    counts)
                   result))))))

(deffoo nnhackernews-request-scan (&optional group server)
  (nnhackernews--normalize-server)
  (when group
    (if (> 2 (- (truncate (float-time)) nnhackernews--last-scan-time))
        (gnus-message 7 "nnhackernews-request-scan: last scanned at %s"
                      (current-time-string nnhackernews--last-scan-time))
      (nnhackernews--with-group group
        (cl-destructuring-bind (seconds num-gc seconds-gc)
            (benchmark-run (nnhackernews--incoming))
          (setq nnhackernews--last-scan-time (truncate (float-time)))
          (gnus-message 5 (concat "nnhackernews-request-scan: Took %s seconds,"
                                  " with %s gc runs taking %s seconds")
                        seconds num-gc seconds-gc))))))

(defsubst nnhackernews--make-message-id (id)
  "Construct a valid Gnus message id from ID."
  (format "<%s@ycombinator.com>" id))

(defsubst nnhackernews--make-references (id)
  "Construct a space delimited string of message ancestors of ID."
  (mapconcat (lambda (ref) (nnhackernews--make-message-id ref))
             (nnhackernews-refs-for id) " "))

(defsubst nnhackernews--make-header (article-number &optional group)
  "Construct full headers of articled indexed ARTICLE-NUMBER in GROUP."
  (let* ((header (nnhackernews--get-header article-number group))
         (score (plist-get header :score))
         (num-comments (plist-get header :num_comments)))
    (make-full-mail-header
     article-number
     (replace-regexp-in-string "\\S-+ HN: " ""
                               (or (plist-get header :title)
                                   (plist-get header :link_title)))
     (plist-get header :by)
     (format-time-string "%a, %d %h %Y %T %z (%Z)" (plist-get header :time))
     (nnhackernews--make-message-id (plist-get header :id))
     (nnhackernews--make-references (plist-get header :id))
     0 0 nil
     (append `((X-Hackernews-Name . ,(plist-get header :id)))
             `((X-Hackernews-ID . ,(plist-get header :id)))
             `((X-Hackernews-Permalink . ,(format "https://news.ycombinator.com/item?id=%s" (plist-get header :id))))
             (and (integerp score)
                  `((X-Hackernews-Score . ,(number-to-string score))))
             (and (integerp num-comments)
                  `((X-Hackernews-Comments . ,(number-to-string num-comments))))))))

(deffoo nnhackernews-request-article (article-number &optional group server buffer)
  (nnhackernews--normalize-server)
  (unless buffer (setq buffer nntp-server-buffer))
  (nnhackernews--with-group group
    (with-current-buffer buffer
      (erase-buffer)
      (let* ((header (nnhackernews--get-header article-number group))
             (mail-header (nnhackernews--make-header article-number))
             (score (cdr (assq 'X-Hackernews-Score (mail-header-extra mail-header))))
             (permalink (cdr (assq 'X-Hackernews-Permalink (mail-header-extra mail-header))))
             (body (nnhackernews--massage (nnhackernews--get-body header server))))
        (when body
          (insert
           "Newsgroups: " group "\n"
           "Subject: " (mail-header-subject mail-header)  "\n"
           "From: " (or (mail-header-from mail-header) "nobody") "\n"
           "Date: " (mail-header-date mail-header) "\n"
           "Message-ID: " (mail-header-id mail-header) "\n"
           "References: " (mail-header-references mail-header) "\n"
           "Content-Type: text/html; charset=utf-8" "\n"
           "Archived-at: " permalink "\n"
           "Score: " score "\n"
           "\n")
          (-when-let*
              ((parent (plist-get header :parent))
               (parent-author
                (or (nnhackernews--gethash parent nnhackernews-authors-hashtb)
                    "Someone"))
               (parent-body (nnhackernews--get-body
                             (nnhackernews-find-header parent) server)))
            (insert (nnhackernews--citation-wrap parent-author parent-body)))
          (aif (and nnhackernews-render-story (plist-get header :url))
              (condition-case err
                  (nnhackernews--request "nnhackernews-request-article" it
                                         :success (cl-function
                                                   (lambda (&key data &allow-other-keys)
                                                     (insert data))))
                (error (gnus-message 5 "nnhackernews-request-article: %s"
                                     (error-message-string err))
                       (insert body)))
            (insert body))
          (cons group article-number))))))

(deffoo nnhackernews-retrieve-headers (article-numbers &optional group server _fetch-old)
  (nnhackernews--normalize-server)
  (nnhackernews--with-group group
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (i article-numbers)
        (nnheader-insert-nov (nnhackernews--make-header i group)))
      'nov)))

(deffoo nnhackernews-close-server (&optional server)
  (nnhackernews--normalize-server)
  t)

(deffoo nnhackernews-request-list (&optional server)
  (nnhackernews--normalize-server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (mapc (lambda (group)
            (let ((full-name (gnus-group-full-name group '("nnhackernews" (or server "")))))
              (gnus-activate-group full-name t)
              (gnus-group-unsubscribe-group full-name gnus-level-default-subscribed t))
            (insert (format "%s %d 1 y\n" group
                            (length (nnhackernews-get-headers group)))))
          `(,nnhackernews--group-ask
            ,nnhackernews--group-show
            ,nnhackernews--group-job
            ,nnhackernews--group-stories)))
  t)

(defun nnhackernews-sentinel (process event)
  "Wipe headers state when PROCESS dies from EVENT."
  (unless (string= "open" (substring event 0 4))
    (gnus-message 2 "nnhackernews-sentinel: process %s %s"
                  (car (process-command process))
                  (replace-regexp-in-string "\n$" "" event))
    (setq nnhackernews-location-hashtb (gnus-make-hashtable))
    (gnus-backlog-shutdown)))

(defun nnhackernews--message-user (server beg end _prev-len)
  "Message SERVER related alert with `buffer-substring' from BEG to END."
  (let ((string (buffer-substring beg end))
        (magic "::user::"))
    (when (string-prefix-p magic string)
      (message "%s: %s" server (substring string (length magic))))))

(defun nnhackernews--request-form (url &optional hidden)
  "Store a cookie from URL with HIDDEN plist."
  (let* (result
         (auth-source-do-cache nil)
         (auth-source-creation-prompts '((user . "news.ycombinator.com user: ")
                                         (secret . "Password for %u: ")))
         (found (car (auth-source-search :max 1 :host "news.ycombinator.com" :require
                                         '(:user :secret) :create t))))
    (nnhackernews--request
     "nnhackernews--request-form"
     url
     :backend 'curl
     :data (append
            (cl-loop for (k v) on hidden by (function cddr)
                     collect (cons (cl-subseq (symbol-name k) 1) v))
            `(("acct" . ,(plist-get found :user))
              ("pw" . ,(let ((secret (plist-get found :secret)))
                         (if (functionp secret)
                             (funcall secret)
                           secret)))))
     :success (nnhackernews--callback result))
    result))

(cl-defun nnhackernews--request-submit-success (&key data response &allow-other-keys)
  "If necessary, login first, then return plist of :fnid and :fnop."
  (let* ((dom (nnhackernews--domify data))
         (form (car (alist-get 'form (alist-get 'body dom))))
         (login-p (string= "submit" (alist-get 'action form))))
    (when login-p
      (setq dom (nnhackernews--domify
                 (nnhackernews--request-form (request-response-url response)))))
    (let (hidden)
      (-tree-map-nodes
       (lambda (x)
         (and (listp x)
              (eq (car x) 'input)
              (string= "hidden" (alist-get 'type (cl-second x)))))
       (lambda (x)
         (!cons (alist-get 'value (cl-second x)) hidden)
         (!cons (intern (concat ":" (alist-get 'name (cl-second x)))) hidden))
       dom)
      hidden)))

(defun nnhackernews--request-submit ()
  "Get the hidden fields FNID and FNOP from `https://news.ycombinator.com/submit'."
  (let (result)
    (nnhackernews--request
     "nnhackernews--request-submit"
     "https://news.ycombinator.com/submit"
     :backend 'curl
     :success (nnhackernews--callback result #'nnhackernews--request-submit-success))
    result))

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
  (when-let ((hidden (nnhackernews--request-submit)))
    (let* ((ret t)
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
      (cond (cancel-name (nnhackernews--request-delete cancel-name))
            (edit-name (nnhackernews--request-edit edit-name body))
            (reply-p (nnhackernews--request-reply (plist-get header :id)
                                                  body (stringp root-p)))
            (link (let* ((parsed-url (url-generic-parse-url link))
                         (host (url-host parsed-url)))
                    (if (and (stringp host) (not (zerop (length host))))
                        (nnhackernews--request-submit-link title link hidden)
                      (error "nnhackernews-request-post: invalid url \"%s\"" link)
                      (setq ret nil))))
            (t (nnhackernews--request-submit-text body hidden)))
      ret)))

(defun nnhackernews--browse-story (&rest _args)
  "What happens when I click on hackernews Subject."
  (-when-let* ((group-article gnus-article-current)
               (url (plist-get (nnhackernews--retrieve-root
                                (nnhackernews--get-header
                                 (cdr group-article)
                                 (gnus-group-real-name (car group-article))))
                           :url)))
    (browse-url url)))

(defun nnhackernews--header-button-alist ()
  "Construct a buffer-local `gnus-header-button-alist' for nnhackernews."
  (let* ((result (copy-alist gnus-header-button-alist))
         (references-value (assoc-default "References" result
                                          (lambda (x y) (string-match-p y x))))
         (references-key (car (rassq references-value result))))
    (setq result (cl-delete "^Subject:" result :test (lambda (x y) (cl-search x (car y)))))
    (setq result (cl-delete references-key result :test (lambda (x y) (cl-search x (car y)))))
    (push (append '("^\\(Message-I[Dd]\\|^In-Reply-To\\):") references-value) result)
    (push '("^Subject:" ": *\\(.+\\)$" 1 (>= gnus-button-browse-level 0)
            nnhackernews--browse-story 1)
          result)
    result))

(defsubst nnhackernews--fallback-link ()
  "Cannot render story."
  (let* ((header (nnhackernews--get-header
                  (cdr gnus-article-current)
                  (gnus-group-real-name (car gnus-article-current))))
         (body (nnhackernews--massage (nnhackernews--get-body header))))
    (with-current-buffer gnus-original-article-buffer
      (article-goto-body)
      (delete-region (point) (point-max))
      (insert body))))

(defalias 'nnhackernews--display-article
  (lambda (article &optional all-headers _header)
    (condition-case err
        (gnus-article-prepare article all-headers)
      (error
       (if nnhackernews-render-story
           (progn
             (gnus-message 7 "nnhackernews--display-article: '%s' (falling back...)"
                           (error-message-string err))
             (nnhackernews--fallback-link)
             (gnus-article-prepare article all-headers))
         (error (error-message-string err))))))
  "In case of shr failures, dump original link.")

(defsubst nnhackernews--dense-time (time)
  "Convert TIME to a floating point number.

Written by John Wiegley (https://github.com/jwiegley/dot-emacs)."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defalias 'nnhackernews--format-time-elapsed
  (lambda (header)
    (condition-case nil
        (let ((date (mail-header-date header)))
          (if (> (length date) 0)
              (let*
                  ((then (nnhackernews--dense-time
                          (apply 'encode-time (parse-time-string date))))
                   (now (nnhackernews--dense-time (current-time)))
                   (diff (- now then))
                   (str
                    (cond
                     ((>= diff (* 86400.0 7.0 52.0))
                      (if (>= diff (* 86400.0 7.0 52.0 10.0))
                          (format "%3dY" (floor (/ diff (* 86400.0 7.0 52.0))))
                        (format "%3.1fY" (/ diff (* 86400.0 7.0 52.0)))))
                     ((>= diff (* 86400.0 30.0))
                      (if (>= diff (* 86400.0 30.0 10.0))
                          (format "%3dM" (floor (/ diff (* 86400.0 30.0))))
                        (format "%3.1fM" (/ diff (* 86400.0 30.0)))))
                     ((>= diff (* 86400.0 7.0))
                      (if (>= diff (* 86400.0 7.0 10.0))
                          (format "%3dw" (floor (/ diff (* 86400.0 7.0))))
                        (format "%3.1fw" (/ diff (* 86400.0 7.0)))))
                     ((>= diff 86400.0)
                      (if (>= diff (* 86400.0 10.0))
                          (format "%3dd" (floor (/ diff 86400.0)))
                        (format "%3.1fd" (/ diff 86400.0))))
                     ((>= diff 3600.0)
                      (if (>= diff (* 3600.0 10.0))
                          (format "%3dh" (floor (/ diff 3600.0)))
                        (format "%3.1fh" (/ diff 3600.0))))
                     ((>= diff 60.0)
                      (if (>= diff (* 60.0 10.0))
                          (format "%3dm" (floor (/ diff 60.0)))
                        (format "%3.1fm" (/ diff 60.0))))
                     (t
                      (format "%3ds" (floor diff)))))
                   (stripped
                    (replace-regexp-in-string "\\.0" "" str)))
                (concat (cond
                         ((= 2 (length stripped)) "  ")
                         ((= 3 (length stripped)) " ")
                         (t ""))
                        stripped))))
      ;; print some spaces and pretend nothing happened.
      (error "    ")))
  "Return time elapsed since HEADER was sent.

Written by John Wiegley (https://github.com/jwiegley/dot-emacs).")

;; Evade package-lint!
(fset 'gnus-user-format-function-S
      (symbol-function 'nnhackernews--format-time-elapsed))


(let ((custom-defaults
       ;; For now, revert any user overrides that I can't predict.
       (mapcar (lambda (x)
                 (let* ((var (cl-first x))
                        (sv (get var 'standard-value)))
                   (when (eq var 'gnus-default-adaptive-score-alist)
                     (setq sv (list `(quote
                                      ,(mapcar (lambda (entry)
                                                 (cons (car entry)
                                                       (assq-delete-all 'from (cdr entry))))
                                               (eval (car sv)))))))
                   (cons var sv)))
               (seq-filter (lambda (x) (eq 'custom-variable (cl-second x)))
                           (append (get 'gnus-score-adapt 'custom-group)
                                   (get 'gnus-score-default 'custom-group))))))
  (add-to-list 'gnus-parameters `("^nnhackernews"
                                  ,@custom-defaults
                                  (gnus-summary-make-false-root 'adopt)
                                  (gnus-cite-hide-absolute 5)
                                  (gnus-cite-hide-percentage 0)
                                  (gnus-cited-lines-visible '(2 . 2))
                                  (gnus-use-adaptive-scoring (quote (line)))
                                  (gnus-newsgroup-adaptive t)
                                  (gnus-simplify-subject-functions (quote (gnus-simplify-subject-fuzzy)))

                                  (gnus-summary-line-format "%3t%U%R%uS %I%(%*%-10,10f  %s%)\n")
                                  (gnus-auto-extend-newsgroup nil)
                                  (gnus-add-timestamp-to-message t)
                                  (gnus-summary-display-article-function
                                   (quote ,(symbol-function 'nnhackernews--display-article)))
                                  (gnus-header-button-alist
                                   (quote ,(nnhackernews--header-button-alist)))
                                  (gnus-visible-headers ,(concat gnus-visible-headers "\\|^Score:")))))

(nnoo-define-skeleton nnhackernews)

(defun nnhackernews-article-mode-activate ()
  "Augment the `gnus-article-mode-map' conditionally."
  (when (nnhackernews--gate)
    (nnhackernews-article-mode)))

(defun nnhackernews-summary-mode-activate ()
  "Shadow some bindings in `gnus-summary-mode-map' conditionally."
  (when (nnhackernews--gate)
    (nnhackernews-summary-mode)))

;; I believe I did try buffer-localizing hooks, and it wasn't sufficient
(add-hook 'gnus-summary-mode-hook 'nnhackernews-summary-mode-activate)

;; Avoid having to select the GROUP to make the unread number go down.
(mapc (lambda (hook)
        (add-hook hook
                  (lambda () (mapc (lambda (group)
                                     (nnhackernews--score-unread group))
                                   `(,nnhackernews--group-ask
                                     ,nnhackernews--group-show
                                     ,nnhackernews--group-stories)))))
      '(gnus-started-hook gnus-after-getting-new-news-hook))

;; `gnus-newsgroup-p' requires valid method post-mail to return t
(add-to-list 'gnus-valid-select-methods '("nnhackernews" post-mail) t)

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
       "ycombinator.com" val)))

(add-function
 :before-until (symbol-function 'message-make-from)
 (lambda (&rest _args)
   (when (nnhackernews--gate)
     (concat (nnhackernews--who-am-i) "@ycombinator.com"))))

(add-function
 :around (symbol-function 'message-is-yours-p)
 (lambda (f &rest args)
   (let ((concat-func (lambda (f &rest args)
                       (let ((fetched (apply f args)))
                         (if (string= (car args) "from")
                             (concat fetched "@ycombinator.com")
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

(add-function
 :around (symbol-function 'url-http-generic-filter)
 (lambda (f &rest args)
   (cond ((nnhackernews--gate)
          (condition-case err
              (apply f args)
            (error (gnus-message 7 "url-http-generic-filter: %s"
                                 (error-message-string err)))))
         (t (apply f args)))))

;; Make the scoring entries Markovian
(add-function
 :around (symbol-function 'gnus-summary-score-entry)
 (lambda (f header match &rest args)
   (cond ((nnhackernews--gate)
          (let* ((new-touched
                  (let ((gnus-score-alist (copy-alist '((touched nil)))))
                    (cons (apply f header match args)
                          (cl-some #'identity (gnus-score-get 'touched)))))
                 (new (car new-touched))
                 (touched (cdr new-touched)))
            (when (and touched new)
              (-if-let* ((old (gnus-score-get header))
                         (elem (assoc match old))
                         (match-type (eq (nth 3 elem) (nth 3 new)))
                         (match-date (or (and (numberp (nth 2 elem)) (numberp (nth 2 new)))
                                         (and (not (nth 2 elem)) (not (nth 2 new))))))
                  (setcar (cdr elem) (nth 1 new))
                (gnus-score-set header (cons new old) nil t))
              (gnus-score-set 'touched '(t)))
            new))
         (t (apply f header match args)))))

;; the let'ing to nil of `gnus-summary-display-article-function'
;; in `gnus-summary-select-article' dates back to antiquity.
(add-function
 :around (symbol-function 'gnus-summary-display-article)
 (lambda (f &rest args)
   (cond ((nnhackernews--gate)
          (let ((gnus-summary-display-article-function
                 (symbol-function 'nnhackernews--display-article)))
            (apply f args)))
         (t (apply f args)))))

;; disallow caching as firebase might change the article numbering?
(setq gnus-uncacheable-groups
      (aif gnus-uncacheable-groups
          (format "\\(%s\\)\\|\\(^nnhackernews\\)" it)
        "^nnhackernews"))

;; (push '((and (eq (car gnus-current-select-method) 'nnhackernews)
;;              (eq mark gnus-unread-mark)
;;              (not (string-match-p
;;                    "^Re: " (gnus-summary-article-subject))))
;;         . gnus-summary-high-unread)
;;       gnus-summary-highlight)

(provide 'nnhackernews)

;;; nnhackernews.el ends here
