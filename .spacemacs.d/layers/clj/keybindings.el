;;; keybindings.el --- clj: keybindings        -*- lexical-binding: t; -*-

;;; Commentary:

;; Clojure key bindings.

;;; Code:

(setq clojure/key-binding-prefixes '(("mb" . "babel")
                                     ("mj" . "etags")
                                     ("ms" . "repl")
                                     ("mh" . "help")
                                     ("ml" . "start repl")
                                     ("mr" . "refactor")
                                     ("mra" . "add")
                                     ("mrc" . "cycle/clean")
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
                                     ("mru" . "unwind/update")
                                     ))

(dolist (mode '(clojure-mode
                clojurec-mode
                clojurescript-mode
                clojurex-mode))
  (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                     mode (car x) (cdr x)))
        clojure/key-binding-prefixes))

(dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
  (spacemacs/set-leader-keys-for-major-mode m
    ;; eval
    "eb" 'inf-clojure-eval-buffer
    "ee" 'inf-clojure-eval-last-sexp
    "ef" 'inf-clojure-eval-defun
    "er" 'inf-clojure-eval-region
    "en" 'inf-clojure-set-ns
    ;; "ew" 'cider-eval-last-sexp-and-replace
    ;; repl
    ;; "sb" 'cider-load-buffer
    ;; "sB" 'spacemacs/cider-send-buffer-in-repl-and-focus
    ;; "sc" 'cider-connect
    ;; "se" 'spacemacs/cider-send-last-sexp-to-repl
    ;; "sE" 'spacemacs/cider-send-last-sexp-to-repl-focus
    ;; "sf" 'spacemacs/cider-send-function-to-repl
    ;; "sF" 'spacemacs/cider-send-function-to-repl-focus
    "se" 'clj-insert-last-sexp-in-repl
    "sn" 'reload-current-clj-ns
    "sr" 'clj-load-current-cljs-ns
    "si" 'inf-clojure
    ;; "sI" 'cider-jack-in-clojurescript
    ;; "sn" 'spacemacs/cider-send-ns-form-to-repl
    ;; "sN" 'spacemacs/cider-send-ns-form-to-repl-focus
    ;; "sq" 'cider-quit
    ;; "sr" 'spacemacs/cider-send-region-to-repl
    ;; "sR" 'spacemacs/cider-send-region-to-repl-focus
    "ss" 'inf-clojure-switch-to-repl
    ;; "sx" 'cider-refresh
  ;; start repl
  "ll"  'clj-run-lisp
  "lb"  'clj-run-boot
  "lf"  'clj-run-figwheel
  ;; tags
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




(spacemacs/set-leader-keys-for-major-mode 'inferior-lisp-mode
  "sk" 'spacemacs/comint-clear-buffer
  "sr" 'clj-reimport
    "r?"  'cljr-describe-refactoring
    "rap" 'cljr-add-project-dependency
    "ras" 'cljr-add-stubs
    "rcc" 'cljr-cycle-coll
    "rci" 'cljr-cycle-if
    "rcp" 'cljr-cycle-privacy
    "rdk" 'cljr-destructure-keys
    "rel" 'cljr-expand-let
    "rfu" 'cljr-find-usages
    "rhd" 'cljr-hotload-dependency
    "ril" 'cljr-introduce-let
    "rml" 'cljr-move-to-let
    "rpc" 'cljr-project-clean
    "rrl" 'cljr-remove-let
    "rsp" 'cljr-sort-project-dependencies
    "rsc" 'cljr-show-changelog
    "rtf" 'cljr-thread-first-all
    "rth" 'cljr-thread
    "rtl" 'cljr-thread-last-all
    "rua" 'cljr-unwind-all
    "rup" 'cljr-update-project-dependencies
    "ruw" 'cljr-unwind
  )

;;; global mode 
(spacemacs/declare-prefix "or" "repl")
(spacemacs/set-leader-keys
  "ork" 'spacemacs/comint-clear-buffer
  "ori" 'clj-switch-to-inf-lisp
  "orr" 'clj-reimport)

