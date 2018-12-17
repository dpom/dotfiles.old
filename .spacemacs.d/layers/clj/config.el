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


(spacemacs|define-jump-handlers clojure-mode)
(spacemacs|define-jump-handlers clojurec-mode)
(spacemacs|define-jump-handlers clojurescript-mode)
(spacemacs|define-jump-handlers clojurex-mode)

(defvar clj-inf-lisp-buffer "*inf-clojure*")
(defvar clj-inferior-lisp-program-lein "lein with-profile +dev repl")
(defvar clj-inferior-lisp-program-figwheel "lein figwheel")
(defvar clj-inferior-lisp-program-boot "boot repl")

(defvar cljtest-error-regexp
  '(cljtest "FAIL in (.+) (\\(.+\\):\\([0-9,]+\\))" 1 2))

 
