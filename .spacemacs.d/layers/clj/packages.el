;;; packages.el --- clj layer packages file for Spacemacs.
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `clj-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `clj/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `clj/pre-init-PACKAGE' and/or
;;   `clj/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst clj-packages
  '(clojure-mode
    compile
    inf-clojure
    company-etags
    eldoc
    )
  "The list of Lisp packages required by the clj layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun clj/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :init (progn
            (add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
            (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
            ;; This regexp matches shebang expressions like `#!/usr/bin/env boot'
            (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode)))
    :config (progn
              (require 'compile)
              (unless (assq 'cljtest  compilation-error-regexp-alist-alist)
                (setq compilation-error-regexp-alist-alist (cons cljtest-error-regexp compilation-error-regexp-alist-alist)))
              (setq compilation-error-regexp-alist (mapcar 'car compilation-error-regexp-alist-alist))
              (add-hook 'inferior-lisp-mode-hook 'spacemacs/load-yasnippet)
              )))

(defun clj/init-compile ()
  (use-package compile
    :defer t
    ))

(defun clj/init-inf-clojure ()
  (use-package inf-clojure
    :defer t
    :config (progn
              (setq inf-clojure-prompt-read-only nil)
              (add-hook 'inf-clojure-minor-mode-hook   ;; prevent company-mode from freezing Emacs when the REPL is busy
                        (lambda () (setq completion-at-point-functions nil)))
              (add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)

             (add-hook 'clojure-mode-hook
                        '(lambda ()
                           (define-key clojure-mode-map "\C-c\C-k" 'reload-current-clj-ns)
                           (define-key clojure-mode-map "\M-." 'find-tag-without-ns)
                           (define-key clojure-mode-map "\C-cl" 'erase-inf-buffer)
                           (define-key clojure-mode-map "\C-c\C-t" 'clojure-toggle-keyword-string)))
              (add-hook 'inf-clojure-mode-hook
                        '(lambda ()
                           (define-key inf-clojure-mode-map "\C-cl" 'erase-inf-buffer)))

              )))


;; company-mode
(defun clj/post-init-company-etags ()
  (use-package company-etags
    :defer t
    :config (progn
              (add-to-list 'company-etags-modes 'clojure-mode)
              (add-hook 'after-init-hook 'global-company-mode)
              ))
  )

;; eldoc-mode
(defun clj/post-init-eldoc ()
  (use-package eldoc
    :defer t
    :config (progn
              (add-hook 'clojure-mode-hook #'eldoc-mode)
              (add-hook 'inf-clojure-mode-hook #'eldoc-mode)
              ))
  )


;;; packages.el ends here
