;;; packages.el --- myoutline layer packages file for Spacemacs.
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

(defconst myoutline-packages
  '(
    outshine
    (outline-ivy :location local)
    )
  "The list of Lisp packages required by the myoutline layer.")

;;; Outshine

(defun myoutline/init-outshine ()
  (defun advise-outshine-narrow-start-pos ()
    (unless (outline-on-heading-p t)
      (outline-previous-visible-heading 1)))

  (use-package outshine
    :after macros
    :init
    (progn
      (spacemacs/set-leader-keys
        "nn" 'outshine-narrow-to-subtree
        "nw" 'widen)
      (define-keys outline-minor-mode-map
        (kbd "M-RET") 'outshine-insert-heading
        (kbd "<backtab>") 'outshine-cycle-buffer))

    :config
    (progn
      ;; Narrowing works within the headline rather than requiring to be on it
      (advice-add 'outshine-narrow-to-subtree :before
                  'advise-outshine-narrow-start-pos)

      (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
      (add-hook 'prog-mode-hook 'outline-minor-mode))))



(defun myoutline/init-outline-ivy ()
  (use-package outline-ivy
    :after ivy outshine macros))


;;; packages.el ends here
