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

(spacemacs|forall-clojure-modes m
  (mapc (lambda (x) (spacemacs/declare-prefix-for-mode m (car x) (cdr x))) clojure/key-binding-prefixes))

(spacemacs|forall-clojure-modes m
    (spacemacs/set-leader-keys-for-major-mode m
      ;; eval
      "eb" 'inf-clojure-eval-buffer
      "ee" 'inf-clojure-eval-last-sexp
      "ef" 'inf-clojure-eval-defun
      "er" 'inf-clojure-eval-region
      "en" 'inf-clojure-set-ns
      ;; help
      "hh" 'inf-clojure-show-var-documentation
      "ha" 'inf-clojure-apropos
      "hs" 'inf-clojure-show-var-source
      "hp" 'inf-clojure-show-arglists
      ;; repl
      "sc" 'inf-clojure-connect
      "se" 'clj-insert-last-sexp-in-repl
      "sg" 'clj-go
      "si" 'inf-clojure
      "sk" 'erase-inf-buffer
      "sn" 'reload-current-clj-ns
      "sr" 'clj-refresh
      "sR" 'clj-refresh-all
      "sq" 'inf-clojure-quit
      "ss" 'inf-clojure-switch-to-repl
      "sx" 'inf-clojure-restart
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
      "jr" 'tags-query-replace
      ;;
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
      "ruw" 'cljr-unwind))


(spacemacs/set-leader-keys-for-major-mode 'java-mode
  "se" 'clj-insert-last-sexp-in-repl
  "ss" 'inf-clojure-switch-to-repl
  )

(spacemacs/set-leader-keys-for-major-mode 'inferior-lisp-mode
  "sk" 'spacemacs/comint-clear-buffer
  "sr" 'clj-reimport
  )


(spacemacs/set-leader-keys-for-major-mode 'inf-clojure-mode
  ;; "sk" 'spacemacs/comint-clear-buffer
  "sg" 'clj-go
  "sk" 'inf-clojure-clear-repl-buffer
  )


;;; global mode
(spacemacs/declare-prefix "or" "repl")
(spacemacs/set-leader-keys
  "ork" 'spacemacs/comint-clear-buffer
  "ori" 'clj-switch-to-inf-lisp
  "orr" 'clj-reimport)
