;;; keybindings.el --- clj: keybindings        -*- lexical-binding: t; -*-

;;; Commentary:

;; Clojure key bindings.

;;; Code:

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
