
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
    ejc-sql
    )
  "The list of Lisp packages required by the mydb layer.")

(defun mydb/init-ejc-sql ()
  (use-package ejc-sql
    :config (progn
              (ejc-set-rows-limit 100)
              (setq nrepl-sync-request-timeout 60)
              (ejc-create-connection
               "Simba-db-connection"
               :classpath (concat "~/emag/lib/"
                                  "SparkJDBC41.jar")
               :classname "com.simba.spark.jdbc41.Driver"
               :subprotocol "postgresql"
               :subname "jdbc:spark://84.40.60.42:10000/"
               :user "root"
               :password "")

              )
    
    )
  )



;;; packages.el ends here
