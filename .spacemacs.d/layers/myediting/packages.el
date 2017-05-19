;;; packages.el --- myediting layer packages file for Spacemacs.
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
;; Many evil-mode motions/operators will have slightly different behavior while
;; evil-multiedit is active or the cursor is in an iedit region:
;;
;; D: clear the region
;; C: clear to end-of-region and go into insert mode
;; A: go into insert mode at end-of-region
;; I: go into insert mode at start-of-region
;; V: select the region
;; $: go to end-of-region
;; 0/^: go to start-of-region
;; gg/G: go to the first/last region
;;
;;; Code:

(defconst myediting-packages
  '(evil-multiedit
    string-inflection)
  "The list of Lisp packages required by the myediting layer.")


(defun myediting/init-evil-multiedit ()
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

(defun myediting/init-string-inflection ()
    (use-package string-inflection
           :init
           (progn
                    (spacemacs/set-leader-keys
                               "xii" 'string-inflection-all-cycle
                               "xiu" 'string-inflection-underscore
                               "xiU" 'string-inflection-upcase
                               "xik" 'string-inflection-kebab-case
                               "xic" 'string-inflection-lower-camelcase
                               "xiC" 'string-inflection-camelcase))))

;;; packages.el ends here
