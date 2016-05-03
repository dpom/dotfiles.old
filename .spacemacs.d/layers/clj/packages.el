;;; packages.el --- clj Layer packages File for Spacemacs
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

(setq clj-packages
      '(clojure-mode
        company
        etags
        ))

(defun clj/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :config (progn
              (require 'compile)
              (unless (assq 'cljtest  compilation-error-regexp-alist-alist)
                (setq compilation-error-regexp-alist-alist (cons cljtest-error-regexp compilation-error-regexp-alist-alist)))
              (setq compilation-error-regexp-alist (mapcar 'car compilation-error-regexp-alist-alist))
              (add-hook 'clojure-mode-hook #'eldoc-mode)
              (add-hook 'inferior-lisp-mode-hook #'eldoc-mode)
              (add-hook 'inferior-lisp-mode-hook 'rainbow-delimiters-mode)
              (add-hook 'inferior-lisp-mode-hook 'subword-mode)
              (add-hook 'inferior-lisp-mode-hook 'spacemacs/load-yasnippet)
              )))

(defun clj/post-init-company ()
  (use-package company-etags
    :defer t
    :config (progn
              (add-to-list 'company-etags-modes 'clojure-mode)
              (add-to-list 'company-etags-modes 'inferior-lisp-mode)))
  (spacemacs|add-company-hook clojure-mode)
  (push 'company-infclj company-backends-clojure-mode)
  (push 'company-capf company-backends-clojure-mode)
  )


(defun clj/post-init-etags ()
  )

