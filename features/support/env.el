;;; -*- lexical-binding: t; coding: utf-8 -*-

;; (defsubst dir-up (x)
;;   "Replica of f-parent without using f.el.

;; I should just use f.el since ecukes loads it anyway."
;;   (file-name-directory (directory-file-name x)))

;; (let ((root-path (car (last (-iterate 'dir-up load-file-name 4)))))
;;   (add-to-list 'load-path (concat root-path "lisp"))
;;   (add-to-list 'load-path (concat root-path "tests")))

(add-to-list 'load-path (f-expand "lisp" (ecukes-project-path)))
(add-to-list 'load-path (f-expand "tests" (ecukes-project-path)))
(require 'espuds)
(require 'nnhackernews-test)
(require 'cl) ;; for auth-source

(when (and (<= emacs-major-version 25) (getenv "GITHUB_ACTIONS"))
  (!cons "vote_reply_login" ecukes-exclude-tags))

(defvar incoming-iteration 0 "Used in filter-args advice of `nnhackernews--incoming'.")

(defmacro if-demote (demote &rest forms)
  (declare (debug t) (indent 1))
  `(if ,demote
       (with-demoted-errors "demoted: %s"
         ,@forms)
     ,@forms))

(defun cleanup ()
  (let* ((newsrc-file (or (bound-and-true-p gnus-current-startup-file)
                          (bound-and-true-p gnus-dot-newsrc)))
         (quick-file (concat newsrc-file ".eld")))
    (when (file-exists-p quick-file)
      (message "Deleting %s" quick-file)
      (delete-file quick-file))))

(defun save-log (buffer-or-name file-name)
  "from tkf/emacs-ipython-notebook ein:testing-save-buffer."
  (when (and buffer-or-name (get-buffer buffer-or-name) file-name)
    (with-current-buffer buffer-or-name
      (let ((coding-system-for-write 'raw-text))
        (write-region (point-min) (point-max) file-name)))))

(defvar scenario-recording-alist '((touched nil)))
(defvar scenario-recording-p t)

(defun gen-key (sym alist)
  "If SYM is already in ALIST, check if 1-SYM is present...

And if so, 2-SYM, etc. until (N-1)-SYM, then return N-SYM."
  (cl-loop with occur = 0
           for cand = (intern (concat (if (zerop occur) "" (format "%d-" occur)) (symbol-name sym)))
           unless (gnus-score-get cand alist)
             return cand
           end
           do (cl-incf occur)))

(Setup
 (custom-set-variables '(gnus-background-get-unread-articles nil)
                       '(canlock-password "huh?"))
 (add-function
  :after (symbol-function 'nnhackernews--request-login)
  (lambda (&rest _)
    (unless scenario-recording-p
      (fset 'nnhackernews--get-user-cookie (lambda () "dickmao")))))

 (add-function
  :filter-args (symbol-function 'nnhackernews--request)
  (lambda (args)
    (when scenario-recording-p
      (let* ((url (cl-second args))
             (fun0 (plist-get args :success))
             (fun1 (cl-function
                    (lambda (&rest args &key data &allow-other-keys)
                      (gnus-score-set (intern url)
                                      (append (gnus-score-get
                                               (intern url)
                                               scenario-recording-alist)
                                              (list data))
                                      scenario-recording-alist)))))
        (setq args (plist-put
                    args :success
                    (lambda (&rest args)
                      (prog1 (apply fun0 args)
                        (apply fun1 args)))))))
    args))

 (add-function
  :before-until (symbol-function 'nnhackernews--request)
  (lambda (caller url &rest args)
    (unless scenario-recording-p
      (let* ((fun0 (plist-get args :success))
             (values (gnus-score-get (intern url) scenario-recording-alist))
             (plst (car values)))
        (if plst
            (progn
              (gnus-score-set (intern url) (cdr values) scenario-recording-alist)
              (funcall fun0 :data plst :response (make-request-response :url url)))
          (error "nnhackernews--request-item: could not playback %s" url))))))

 (add-function
  :filter-args (symbol-function 'nnhackernews--incoming)
  (lambda (_args)
    (pcase (cl-incf incoming-iteration)
      (1
       '(20724340
         (20724340 20724338 20724330 20724329 20724323 20724312 20724310
                   20724308 20724275 20724265 20724262 20724261 20724260
                   20724247 20724245 20724231)))
      (2
       '(20770771
         (20770761 20770759 20770751 20770740 20770738 20770718 20770714
                   20770713 20770703 20770649)))
      (3
       '(20784930
         (20784900 20784892 20784881 20784878 20784877 20784864 20784858
                   20784855 20784849 20784839 20784831 20784823)))
      (4
       '(20784968
         (20784964 20784962 20784961 20784957 20784944 20784900 20784892
                   20784881 20784878 20784877 20784864)))
      (5
       '(20803854
         (20803845 20803828 20803817 20803795 20803793 20803790 20803781
                   20803777 20803774 20803767 20803764 20803737)))
      (_ (error "Unprepared for iteration %s" incoming-iteration))))))

(defmacro with-scenario (scenario &rest body)
  `(let* ((name (ecukes-scenario-name ,scenario))
          (filename (f-expand (replace-regexp-in-string "\\s-+" "-" name)
                              (f-expand "tests/recordings" (ecukes-project-path)))))
     ,@body))

(put 'with-scenario 'lisp-indent-function 1)

(Before
 (setq nnhackernews--last-scan-time 0)
 (setq ecukes-reporter-before-scenario-hook
       (lambda (scenario)
         (with-scenario scenario
           (setq scenario-recording-p (not (file-exists-p filename)))
           (setq scenario-recording-alist
                 (if scenario-recording-p
                     '((touched nil))
                   (with-temp-buffer
                     (let ((coding-system-for-read score-mode-coding-system))
                       (insert-file-contents filename))
                     (goto-char (point-min))
                     (read (current-buffer))))))))
 (setq ecukes-reporter-after-scenario-hook
       (lambda (scenario)
         (with-scenario scenario
           (when scenario-recording-p
             (setq scenario-recording-alist
                   (assq-delete-all 'touched scenario-recording-alist))
             (gnus-make-directory (file-name-directory filename))
             (with-temp-buffer
               (gnus-prin1 scenario-recording-alist)
               (let ((coding-system-for-write score-mode-coding-system))
                 (gnus-write-buffer filename)))))
         (setq scenario-recording-alist '((touched nil)))
         (setq scenario-recording-p t))))

(After
 )

(Teardown
 (save-log request-log-buffer-name (f-expand "tests/request-log" (ecukes-project-path)))
 (cleanup)
)

(Fail
 (if noninteractive
     (with-demoted-errors "demote: %s"
       (Teardown))
   (backtrace)
   (keyboard-quit))) ;; useful to prevent emacs from quitting
