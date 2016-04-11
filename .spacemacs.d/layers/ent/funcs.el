;;; funcs.el --- ent Layer functions File for Spacemacs
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

(defun generate-autoload ()
  (require 'autoload)
  (setq generated-autoload-file (expand-file-name (concat ent-project-name "-autoloads.el") (expand-dir-name ent-elisp-src-dir ent-project-home)))
  (with-temp-file generated-autoload-file
    (insert (concat ";;; " ent-project-name "-autoloads.el --- autoloads for " ent-project-name "
;;
;;; Code:
")))
  (update-directory-autoloads ent-elisp-src-dir)
  (find-file generated-autoload-file)
  (goto-char (point-max))
  (insert ?\n)
  (insert (concat "(provide '" ent-project-name "-autoloads)
;;; " ent-project-name "-autoloads.el ends here
;;
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
"))
  (insert ?\n)
  (save-buffer 0)
  (kill-buffer (current-buffer)))


(defun install-info ()
  "Copy all info files"
  (ent-walk ent-elisp-src-dir "\\.info$"
            '(lambda (x)
               (message "copy %s" x)
               (copy-file x info-dir t)
               (call-process "sudo" nil t t "ginstall-info"(concat "--infodir=" (expand-dir-name info-dir)) x ))))


(defun ent-tangle ()
  ;; (add-to-list 'load-path orgmode-dir)
  ;; (require 'org)(require 'org-exp)(require 'ob)(require 'ob-tangle)
  (require 'org)(require 'ob-exp)(require 'ob)(require 'ob-tangle)
  (find-file (expand-file-name ent-project-config-filename ent-project-home))
  (org-babel-tangle)
  (kill-buffer))

(defun ent-get-version ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name  "project.clj" ent-project-home))
    (goto-char (point-min)) ;; From the beginning...
    (if (re-search-forward (concat "defproject " ent-project-name " \"\\(.*\\)\"") nil t 1)
        (match-string 1))))

(defun ent-make-cli-file (version template cli)
  "Generate CLI script file for VERSION from TEMPLATE file."
  (with-temp-file cli
    (insert-file-contents (expand-file-name  template ent-project-home))
    (goto-char (point-min)) ;; From the beginning...
    (while (re-search-forward "%version%" nil t)
      (replace-match version))))


(defun ent-make-all-cli-files ()
  (let ((ver (ent-get-version)))
    (mapc #'(lambda (x) (progn (message "Make %s" (cdr x)) (ent-make-cli-file ver (car x) (cdr x)))) ent-cli)))
;; ent-funcs ends here
