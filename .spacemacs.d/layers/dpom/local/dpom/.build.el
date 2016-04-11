(add-to-list 'load-path user-emacs-directory)
(setq debug-on-error t)

(defun expand-dir-name (dir path)
  "Expand a local dir NAME using his PATH."
  (file-name-as-directory (expand-file-name dir path)))

(defvar init-dir (expand-dir-name "init" user-emacs-directory))
(add-to-list 'load-path init-dir)

(defvar contrib-dir (expand-dir-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path contrib-dir)

(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-ent)
(require 'init-org)



;; project settings
(setq ent-project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(setq ent-project-name "dpom")
(setq ent-clean-regexp "~$\\|dpom-.*\.elc$\\|semantic\.cache")                    
(setq ent-project-config-filename "Dpom.org")
(setq ent-elisp-src-dir ".")
(add-to-list 'load-path ent-elisp-src-dir)

;; local functions

;; tasks

(load (expand-file-name ent-init-file))

(task 'tags '(clean tangle) "generate tags" '(lambda (&optional x)
                                        "etags -R --totals=yes"))


(task 'install '(clean tangle genautoload elispbuild) "install the package")


;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:(defvar dotfiles-dir "" "emacs local configuration dir")
