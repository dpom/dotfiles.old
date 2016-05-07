;;; config.el --- clj Layer configuration File for Spacemacs
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


(spacemacs|defvar-company-backends clojure-mode)
(spacemacs|defvar-company-backends inferior-lisp-mode)

(defvar clj-inf-lisp-buffer "*inferior-lisp*")
(defvar clj-inferior-lisp-program-lein "lein with-profile +dev repl")
(defvar clj-inferior-lisp-program-figwheel "lein figwheel")
(defvar clj-inferior-lisp-program-boot "boot repl")

(defvar cljtest-error-regexp
  '(cljtest "FAIL in (.+) (\\(.+\\):\\([0-9,]+\\))" 1 2))

(setq clojure/key-binding-prefixes '(("mb" . "babel")
                                     ("mj" . "etags")
                                     ("ms" . "repl")
                                     ("mh" . "help")
                                     ("ml" . "start repl")
                                     ))

(dolist (mode '(clojure-mode
                clojurec-mode
                clojurescript-mode
                clojurex-mode))
  (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                     mode (car x) (cdr x)))
        clojure/key-binding-prefixes))
