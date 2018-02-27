;;; packages.el --- myconfig layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Dan Pomohaci <dan.pomohaci@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;;
;;
;;; Code:

(defconst myconfig-packages
  '(
    ispell
    projectile
    yasnippet
    (avy-config      :location local)
    (eshell-config   :location local)
    (evil-config     :location local)
    (ivy-config      :location local)
    )
  "The list of Lisp packages required by the myconfig layer.")


(defun myconfig/post-init-ispell ()
  (setq ispell-program-name
        "aspell"))

(defun myconfig/post-init-projectile ()
  (setq projectile-file-exists-local-cache-expire (* 5 60)
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action 'projectile-dired)
  (setq projectile-indexing-method
        'alien))

(defun myconfig/pre-init-yasnippet ()
  (global-set-key (kbd "C-SPC") 'hippie-expand))

;;; Local Config

(defun myconfig/init-avy-config ()
  (use-package avy-config
    :after avy macros))

(defun myconfig/init-eshell-config ()
  (use-package eshell-config
    :after evil macros))

(defun myconfig/init-evil-config ()
  (use-package evil-config
    :after evil macros))


(defun myconfig/init-ivy-config ()
  (use-package ivy-config
    :after ivy macros))







;;; packages.el ends here
