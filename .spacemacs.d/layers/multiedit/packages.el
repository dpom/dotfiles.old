;;; packages.el --- multiedit layer packages file for Spacemacs.
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

;;; Code:

(defconst multiedit-packages
  '(evil-multiedit)
  "The list of Lisp packages required by the multiedit layer.")


(defun multiedit/init-evil-multiedit ()
  (use-package evil-multiedit
    :bind (:map evil-normal-state-map
             ("M-d" . evil-multiedit-match-and-next)
             ("M-D" . evil-multiedit-match-and-prev)
           :map evil-visual-state-map
             ("M-d" . evil-multiedit-match-and-next)
             ("M-D" . evil-multiedit-match-and-prev)
             ("R" . evil-multiedit-match-all)
             ("C-M-D" . evil-multedit-restore)
           :map evil-multiedit-state-map
             ("C-n" . evil-multiedit-next)
             ("C-p" . evil-multiedit-prev)
             ("RET" . evil-multiedit-toggle-or-restrict-region)
           :map evil-multiedit-insert-state-map
             ("C-n" . evil-multiedit-next)
             ("C-p" . evil-multiedit-prev)
            )
    :config (progn
              ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
              (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match))
    ))

;;; packages.el ends here
