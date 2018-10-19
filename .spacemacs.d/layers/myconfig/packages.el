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
    company
    ispell
    projectile
    yasnippet
    (avy-config      :location local)
    (eshell-config   :location local)
    (evil-config     :location local)
    (ivy-config      :location local)
    (package-config      :location local)
    )
  "The list of Lisp packages required by the myconfig layer.")

(defun myconfig/post-init-company ()

  (global-company-mode)

  (spacemacs|add-company-backends :modes sh-mode )

  (define-key company-active-map [tab] 'company-complete-common)
  ;;(global-set-key (kbd "TAB") 'hippie-expand)



  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
  
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

(defun myconfig/init-package-config ()
  (use-package package-config))





;;; packages.el ends here
