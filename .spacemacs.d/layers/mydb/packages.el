
;;; packages.el --- mydb layer packages file for Spacemacs.
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

(defconst mydb-packages
  '(
    direx
    clomacs 
    (ejc-sql :location local)
    )
  "The list of Lisp packages required by the mydb layer.")

(defun mydb/init-ejc-sql ()
  (use-package ejc-sql
    :after (direx clomacs)
    :bind (("C-c q" . ejc-switch-to-sql-editor-buffer)
           ("C-c n" . ejc-connect))
    :config (progn
              (ejc-set-rows-limit 100)
              
              (setq nrepl-sync-request-timeout 60)

              (spacemacs/set-leader-keys-for-major-mode 'sql-mode
                "'"    'ejc-connect
                
                "hl"  'ejc-show-tables-list
                "ht"  'ejc-describe-table
                "he"  'ejc-describe-entity
                "hc"  'ejc-show-constraints-list
                "hp"  'ejc-show-procedures-list

                "el"  'ejc-open-log
                "eq"  'ejc-quit-connection
                "ee"  'ejc-eval-user-sql-at-point
                "er"  'ejc-eval-user-sql-region
                "ed"  'ejc-direx:pop-to-buffer

                "mk"  'ejc-previous-sql
                "mj"  'ejc-next-sql
                "mm"  'ejc-mark-this-sql
                "mp"  'ejc-previous-sql
                "mn"  'ejc-next-sql
                )
              )))

(defun mydb/init-direx ()
  (use-package direx
    ;; :init
    ;; This block executes before the package has been loaded
    ;; :config
    ;; This block executes after the package has been loaded
    ))

(defun mydb/init-clomacs ()
  (use-package clomacs
    ;; :init
    ;; This block executes before the package has been loaded
    ;; :config
    ;; This block executes after the package has been loaded
    ))


;;; packages.el ends here
