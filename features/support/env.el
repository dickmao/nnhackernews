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

(defmacro if-demote (demote &rest forms)
  (declare (debug t) (indent 1))
  `(if ,demote
       (with-demoted-errors "demoted: %s"
         ,@forms)
     ,@forms))

(defun cleanup ()
  (let* ((newsrc-file gnus-current-startup-file)
         (quick-file (concat newsrc-file ".eld")))
    (when (file-exists-p quick-file)
      (message "Deleting %s" quick-file)
      (delete-file quick-file))))

(defvar scenario-recording-alist '((touched nil)))
(defvar scenario-recording-p t)

(Setup
 (add-function
  :filter-args (symbol-function 'nnhackernews--request)
  (lambda (args)
    (when scenario-recording-p
      (let* ((url (cl-second args))
             (fun0 (plist-get args :success))
             (fun1 (cl-function
                    (lambda (&rest args &key data &allow-other-keys)
                      (gnus-score-set (intern url)
                                      (list data)
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
             (plst (or (car (gnus-score-get url scenario-recording-alist))
                       (error "nnhackernews--request-item: could not playback %s" url))))
        (funcall fun0 :data plst)))))

 (add-function
  :filter-args (symbol-function 'nnhackernews--incoming)
  (lambda (_args)
    '(20724340
      (20724340 20724338 20724330 20724329 20724323 20724312 20724310
                20724308 20724275 20724265 20724262 20724261 20724260
                20724247 20724245 20724231)))))

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
                   (gnus-delete-alist 'touched scenario-recording-alist))
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
 (cleanup)
)

(Fail
 (if noninteractive
     (with-demoted-errors "demote: %s"
       (Teardown))
   (backtrace)
   (keyboard-quit))) ;; useful to prevent emacs from quitting
