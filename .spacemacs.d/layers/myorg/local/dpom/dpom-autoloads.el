;;; dpom-autoloads.el --- autoloads for dpom
;;
;;; Code:

;;;### (autoloads nil nil ("dpom-utils.el" "ob-asm.el" "ob-dtd.el"
;;;;;;  "ob-make.el" "ob-rb.el" "ob-xml.el" "org-e-contacts.el")
;;;;;;  (21646 57872 927461 0))

;;;***

;;;### (autoloads nil "dpom-commands" "dpom-commands.el" (21646 57870
;;;;;;  0 0))
;;; Generated autoloads from dpom-commands.el

(autoload 'dpom-insert-day "dpom-commands" "\
Insert a day using calendar

\(fn)" t nil)

(autoload 'dpom-comment-or-uncomment-current-line-or-region "dpom-commands" "\
Comments or uncomments current current line or whole lines in region.

\(fn)" t nil)

(autoload 'dpom-get-doc-file "dpom-commands" "\
Find a a doc file using.

\(fn)" t nil)

(autoload 'dpom-eshell-execute-current-line "dpom-commands" "\
Insert text of current line in eshell and execute.

\(fn)" t nil)

(autoload 'untabify-buffer "dpom-commands" "\


\(fn)" t nil)

(autoload 'indent-buffer "dpom-commands" "\


\(fn)" t nil)

(autoload 'cleanup-buffer "dpom-commands" "\
Perform a bunch of operations on the whitespace content of a buffer.

\(fn)" t nil)

(autoload 'pretty-lambdas "dpom-commands" "\


\(fn)" nil nil)

(autoload 'sudo-edit "dpom-commands" "\


\(fn &optional ARG)" t nil)

(autoload 'vc-git-annotate-command "dpom-commands" "\


\(fn FILE BUF &optional REV)" nil nil)

(autoload 'sm-try-smerge "dpom-commands" "\


\(fn)" nil nil)

(autoload 'dpom-create-tags "dpom-commands" "\
Create tags file.

\(fn DIR-NAME)" t nil)

(autoload 'dpom-org-surround-block "dpom-commands" "\


\(fn BEG END &optional TAG)" t nil)

(autoload 'dpom-org-surround-src "dpom-commands" "\


\(fn BEG END)" t nil)

(autoload 'dpom-babel "dpom-commands" "\


\(fn BEG END)" t nil)

(autoload 'dpom-org-surround-example "dpom-commands" "\


\(fn BEG END)" t nil)

(autoload 'bh/org-agenda-to-appt "dpom-commands" "\
Erase all reminders and rebuilt reminders for today from the agenda

\(fn)" t nil)

(autoload 'dpom-org-manual "dpom-commands" "\


\(fn)" t nil)

(autoload 'dpom-screenshot "dpom-commands" "\
Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file.

\(fn)" t nil)

(autoload 'dpom-insert-file-as-org-table "dpom-commands" "\
Insert a csv file (; separator) into the current buffer at point, and convert it to an org table.

\(fn FILENAME)" t nil)

(autoload 'dpom-org-babel-detangle "dpom-commands" "\
Propagate changes in source file back original to Org-mode file.
This requires that code blocks were tangled with link comments
which enable the original code blocks to be found.

\(fn &optional SOURCE-CODE-FILE)" t nil)

(autoload 'dpom-check-region-parens "dpom-commands" "\
Check if parentheses in the region are balanced. Signals a
scan-error if not.

\(fn)" t nil)

(autoload 'dpom-remove-elc-on-save "dpom-commands" "\
If you're saving an elisp file, likely the .elc is no longer valid.

\(fn)" nil nil)

(autoload 'dpom-format-xml "dpom-commands" "\


\(fn ARG)" t nil)

(autoload 'dpom-complete-surround-tag "dpom-commands" "\


\(fn BEG END)" t nil)

(autoload 'dpom-get-xpath "dpom-commands" "\
Return all the elements in the ancestor axis of the current
    element.  If called interactively, show it in the echo area.

\(fn &optional PRINT-MESSAGE)" t nil)

(autoload 'dpom-validate-xml "dpom-commands" "\
Validate current xml buffer

\(fn &optional FILE)" t nil)

(autoload 'dpom-use-xsl-with-outfile "dpom-commands" "\
Use the actual xsl buffer or the FILE to transform an xml file.
The result is saved in a file.

\(fn &optional FILE)" t nil)

(autoload 'dpom-use-xsl "dpom-commands" "\
Use the actual xsl buffer or the FILE to transform an xml file.

\(fn &optional FILE)" t nil)

;;;***

(provide 'dpom-autoloads)
;;; dpom-autoloads.el ends here
;;
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

