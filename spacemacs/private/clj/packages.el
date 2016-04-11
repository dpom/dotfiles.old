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
              (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
                ;; REPL
                "si" 'dpom/switch-to-inf-lisp
                ;;"sd" 'inf-clojure-eval-defun
                ;;"sD" 'spacemacs/clj-eval-defun
                ;;"sb" 'inf-clojure-eval-buffer
                ;;"sB" 'spacemacs/clj-eval-buffer
                "se" 'dpom/insert-last-sexp-in-repl
                ;;"sE" 'spacemacs/clj-eval-last-sexp
                ;;"sr" 'inf-clojure-eval-region
                ;;"sR" 'spacemacs/clj-eval-region
                "sn" 'reload-current-clj-ns
                ;;"hd" 'inf-clojure-show-var-documentation
                ;;"hs" 'inf-clojure-show-var-source
                "ll"  'dpom/run-lisp
                "lb"  'dpom/run-boot
                "bt" 'org-babel-tangle
                "bd" 'org-babel-detangle)
              (spacemacs/set-leader-keys-for-major-mode 'inferior-lisp-mode
                "sk" 'spacemacs/comint-clear-buffer)
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
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
    "jR" 'projectile-regenerate-tags
    "jf" 'find-tag
    "ja" 'tags-apropos
    "jg" 'find-tag-without-ns
    "js" 'tags-search
    "jh" 'pop-tag-mark
    "jl" 'list-tags
    "jo" 'find-tag-other-window
    "jr" 'tags-query-replace)
  )
