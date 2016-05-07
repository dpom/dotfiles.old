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
        eldoc
        rainbow-delimiters
        subword
        etags
        ))

(defun clj/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
      (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode)))
    :config (progn
              (require 'compile)
              (unless (assq 'cljtest  compilation-error-regexp-alist-alist)
                (setq compilation-error-regexp-alist-alist (cons cljtest-error-regexp compilation-error-regexp-alist-alist)))
              (setq compilation-error-regexp-alist (mapcar 'car compilation-error-regexp-alist-alist))
              (add-hook 'inferior-lisp-mode-hook 'spacemacs/load-yasnippet)
              )))

(defun clj/post-init-company ()
  (use-package company-etags
    :defer t
    :config (progn
              (add-to-list 'company-etags-modes 'clojure-mode)
              (add-to-list 'company-etags-modes 'inferior-lisp-mode)))
  (push 'company-infclj company-backends-clojure-mode)
  (push 'company-capf company-backends-clojure-mode)
  (spacemacs|add-company-hook clojure-mode)
  (push 'company-infclj company-backends-inferior-lisp-mode)
  (push 'company-capf company-backends-inferior-lisp-mode)
  (spacemacs|add-company-hook inferior-lisp-mode)
  )


(defun clojure/post-init-eldoc ()
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  (add-hook 'inferior-lisp-mode-hook 'eldoc-mode))

(defun clojure/post-init-rainbow-delimiters ()
  (add-hook 'inferior-lisp-mode-hook 'rainbow-delimiters-mode))

(defun clojure/post-init-subword ()
  (add-hook 'inferior-lisp-mode-hook 'subword-mode))



(defun clj/post-init-etags ()
  )

