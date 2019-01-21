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
    inf-clojure
    company-etags
    eldoc
    org
    clj-refactor
    parinfer
    projectile))

(defun clj/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :init (progn
            (add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
            (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
            (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
            ;; This regexp matches shebang expressions like `#!/usr/bin/env boot'
            (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode)))
    :config (progn
              (require 'compile)
              (unless (assq 'cljtest  compilation-error-regexp-alist-alist)
                (setq compilation-error-regexp-alist-alist (cons cljtest-error-regexp compilation-error-regexp-alist-alist)))
              (setq compilation-error-regexp-alist (mapcar 'car compilation-error-regexp-alist-alist))
              (add-hook 'inferior-lisp-mode-hook 'spacemacs/load-yasnippet)

              (setq clojure-align-forms-automatically t)
              )))


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
              ;; (setf inf-clojure-lein-cmd '("localhost" . 5555))

              )))


;; company-mode
(defun clj/post-init-company-etags ()
  (use-package company-etags
    :defer t
    :config (progn
              (add-to-list 'company-etags-modes 'clojure-mode)
              (add-to-list 'company-backends 'company-infclj)
              (add-hook 'after-init-hook 'global-company-mode)
              ))
  )

;; eldoc-mode
(defun clj/post-init-eldoc ()
  (use-package eldoc
    :defer t
    :config (progn
              (add-hook 'clojure-mode-hook #'eldoc-mode)
              (add-hook 'inf-clojure-mode-hook #'eldoc-mode))))

;; projectile-mode
(defun clj/post-init-projectile ()
  (use-package projectile
    :defer t
    :config (progn
              (add-hook 'clojure-mode-hook #'projectile-mode))))


(defun clj/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(clojure . t))
    (setq org-babel-clojure-backend 'slime)))

(defun clj/post-init-parinfer ()
  (spacemacs|forall-clojure-modes m
    (add-hook m 'parinfer-mode)))

(defun clj/init-clj-refactor ()
  (use-package clj-refactor
    :defer t
    :init (add-hook 'clojure-mode-hook 'clj-refactor-mode)
    :config (progn
              (add-hook 'clojure-mode-hook (lambda ()
                                             (clj-refactor-mode 1)
                                             (cljr-add-keybindings-with-prefix "C-c C-f")))
              (setq clj-refactor--key-binding-prefixes
                    '(("mr" . "refactor")
                      ("mra" . "add")
                      ("mrc" . "cycle/clean/convert")
                      ("mrd" . "destructure")
                      ("mre" . "extract/expand")
                      ("mrf" . "find/function")
                      ("mrh" . "hotload")
                      ("mri" . "introduce/inline")
                      ("mrm" . "move")
                      ("mrp" . "project/promote")
                      ("mrr" . "remove/rename/replace")
                      ("mrs" . "show/sort/stop")
                      ("mrt" . "thread")
                      ("mru" . "unwind/update")))
              (dolist (m '(clojure-mode
                           clojurec-mode
                           clojurescript-mode
                           clojurex-mode))
                (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                                    m (car x) (cdr x)))
                      clj-refactor--key-binding-prefixes)
                (dolist (r cljr--all-helpers)
                  (let* ((binding (car r))
                         (func (car (cdr r))))
                    (when (not (string-prefix-p "hydra" (symbol-name func)))
                      (spacemacs/set-leader-keys-for-major-mode m
                        (concat "r" binding) func))))))))

;;; packages.el ends here
