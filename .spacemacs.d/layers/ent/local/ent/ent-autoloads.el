;;; ent-autoloads.el --- autoloads for ent
;;
;;; Code:

;;;### (autoloads (ent-find-file ent-visit-config-file ent-visit-build-file
;;;;;;  ent) "ent" "ent.el" (19730 52156))
;;; Generated autoloads from ent.el

(autoload 'ent "ent" "\
Main entry point for ent

\(fn &optional TASKNAME)" t nil)

(autoload 'ent-visit-build-file "ent" "\
Visit the ent project file.

\(fn)" t nil)

(autoload 'ent-visit-config-file "ent" "\
Visit the project specific config file, ent-project-config-filename.

\(fn)" t nil)

(autoload 'ent-find-file "ent" "\
Find a file from the project.

\(fn)" t nil)

(defvar ent-prefix-map (let ((map (make-sparse-keymap))) (define-key map "x" 'ent) (define-key map "c" 'ent-visit-config-file) (define-key map "b" 'ent-visit-build-file) (define-key map "f" 'ent-find-file) map))

(fset 'ent-prefix-map ent-prefix-map)

;;;***

(provide 'ent-autoloads)
;;; ent-autoloads.el ends here
;;
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

