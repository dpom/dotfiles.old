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


(defun dpom/switch-to-inf-lisp ()
  (interactive)
  (pop-to-buffer dpom/inf-lisp-buffer)
  (goto-char (point-max)))



(defun dpom/run-lisp ()
  (interactive)
  (split-window-below-and-focus)
  (run-clojure dpom/inferior-lisp-program-lein))

(defun dpom/run-figweel ()
  (interactive)
  (split-window-below-and-focus)
  (run-clojure dpom/inferior-lisp-program-figweel))

(defun dpom/run-node ()
  (interactive)
  (split-window-below-and-focus)
  (run-clojure dpom/inferior-lisp-program-node))

(defun dpom/run-boot ()
  (interactive)
  (split-window-below-and-focus)
  (run-lisp dpom/inferior-lisp-program-boot))

(defun dpom/get-last-sexp ()
  "Return the sexp preceding the point."
  (buffer-substring-no-properties
   (save-excursion
     (backward-sexp)
     (point))
   (point)))

(defun dpom/insert-last-sexp-in-repl ()
  (interactive)
  (let ((form (dpom/get-last-sexp)))
    (with-current-buffer dpom/inf-lisp-buffer
      (goto-char (point-max))
      (insert form))
    (dpom/switch-to-inf-lisp)))

(defun erase-inf-buffer ()
  (interactive)
  (with-current-buffer dpom/inf-lisp-buffer
    (erase-buffer)
    (lisp-eval-string "")))
