(add-to-list 'load-path user-emacs-directory)
(setq debug-on-error t)

(setq ent-project-home user-emacs-directory)
(setq ent-project-name "EmacsConfig")
(setq ent-clean-regexp "~$\\|\\.tex$")
(setq ent-project-config-filename "EmacsConfig.org")

;; local functions


;; tasks

(load ent-init-file)

;; (task 'tangle '() "tangle config file" '(lambda (&optional x) (ent-emacs "tangle" (expand-file-name ent-file-name ent-project-home))))


;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
