
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

              (ejc-create-connection
               "BigDataSpark-connection"
               :classpath (file-truename "~/.local/lib/hive-jdbc-uber-2.6.3.0-235.jar")
               :classname "org.apache.hive.jdbc.HiveDriver"
               :subprotocol "hive2"
               :subname "//84.40.60.42:10000/"
               :user "root"
               :password "")

              (ejc-create-connection
               "RecEngineML-connection"
               :classpath (file-truename "~/.m2/repository/org/postgresql/postgresql/42.2.1.jre7/postgresql-42.2.1.jre7.jar")
               :classname "org.postgresql.Driver"
               :subprotocol "postgresql"
               :subname "//localhost:5432/pgdb"
               :user "pguser"
               :password "pguser")

              (spacemacs/set-leader-keys-for-major-mode 'sql-mode
                "'"    'ejc-connect
                
                "hl"  'ejc-show-tables-list
                "ht"  'ejc-describe-table
                "he"  'ejc-describe-entity
                "hc"  'ejc-show-constraints-list
                "hp"  'ejc-show-procedures-list

                "sl"  'ejc-open-log
                "sq"  'ejc-quit-connection
                "se"  'ejc-eval-user-sql-at-point
                "sr"  'ejc-eval-user-sql-region
                "sd"  'ejc-direx:pop-to-buffer

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
