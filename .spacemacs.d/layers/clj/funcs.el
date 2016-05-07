;;; funcs.el --- clj Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Dan Pomohaci & Contributors
;;
;; Author: Dan Pomohaci <dan.pomohaci@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defun get-clj-completions (prefix)
  (let* ((proc (inferior-lisp-proc))
         (comint-filt (process-filter proc))
         (kept ""))
    (set-process-filter proc (lambda (proc string) (setq kept (concat kept string))))
    (process-send-string proc (format "(complete.core/completions \"%s\")\n"
                                      (substring-no-properties prefix)))
    (while (accept-process-output proc 0.1))
    (setq completions (read kept))
    (set-process-filter proc comint-filt)
    completions))

(defun company-infclj (command &optional arg &rest ignored)
 (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-infclj))
    (prefix (and (eq major-mode 'inferior-lisp-mode)
                 (company-grab-symbol)))
    (candidates (get-clj-completions arg))))

(defun reload-current-clj-ns ()
  (interactive)
  (let ((current-point (point)))
    (goto-char (point-min))
    (let ((ns-idx (re-search-forward clojure-namespace-name-regex nil t)))
      (when ns-idx
        (goto-char ns-idx)
        (let ((sym (symbol-at-point)))
          (message (format "Loading %s ..." sym))
          (lisp-eval-string (format "(require '%s :reload)" sym))
          (lisp-eval-string (format "(in-ns '%s)" sym)))))
    (goto-char current-point)))




(defun find-tag-without-ns (next-p)
  (interactive "P")
  (find-tag (first (last (split-string (symbol-name (symbol-at-point)) "/")))
            next-p))


(defun clj-switch-to-inf-lisp ()
  (interactive)
  (pop-to-buffer clj-inf-lisp-buffer)
  (goto-char (point-max)))

(defun clj-run (cmd)
  (split-window-below-and-focus)
  (run-lisp cmd))

(defun clj-run-lisp ()
  (interactive)
  (clj-run clj-inferior-lisp-program-lein))

(defun clj-run-boot ()
  (interactive)
  (clj-run clj-inferior-lisp-program-boot))

(defun clj-run-figwheel ()
  (interactive)
  (clj-run clj-inferior-lisp-program-figwheel))

(defun clj-get-last-sexp ()
  "Return the sexp preceding the point."
  (buffer-substring-no-properties
   (save-excursion
     (backward-sexp)
     (point))
   (point)))

(defun clj-insert-in-repl (form)
  (with-current-buffer clj-inf-lisp-buffer
    (goto-char (point-max))
    (insert form))
  )

(defun clj-insert-last-sexp-in-repl ()
  (interactive)
  (let ((form (clj-get-last-sexp)))
    (with-current-buffer clj-inf-lisp-buffer
      (goto-char (point-max))
      (insert form))
    (clj-switch-to-inf-lisp)))


(defun clj-load-current-cljs-ns ()
  (interactive)
  (let ((current-point (point)))
    (goto-char (point-min))
    (let ((ns-idx (re-search-forward clojure-namespace-name-regex nil t)))
      (when ns-idx
        (goto-char ns-idx)
        (let ((sym (symbol-at-point)))
          (clj-insert-in-repl (format "(require '%s)" sym)))))
    (clj-switch-to-inf-lisp)))

(defun clj-switch-to-current-cljs-ns ()
  (interactive)
  (let ((current-point (point)))
    (goto-char (point-min))
    (let ((ns-idx (re-search-forward clojure-namespace-name-regex nil t)))
      (when ns-idx
        (goto-char ns-idx)
        (let ((sym (symbol-at-point)))
          (clj-insert-in-repl (format "(in-ns '%s)" sym)))))
    (clj-switch-to-inf-lisp)))

(defun erase-inf-buffer ()
  (interactive)
  (with-current-buffer clj-inf-lisp-buffer
    (erase-buffer)
    (lisp-eval-string "")))
