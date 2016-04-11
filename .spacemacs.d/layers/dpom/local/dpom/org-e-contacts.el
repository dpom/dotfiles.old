(require 'org-export)
(require 'org-e-ascii)

(org-export-define-backend e-contacts
  (
   (headline . org-e-contacts-headline)
   (property-drawer . org-e-contacts-property-drawer)
   (section . org-e-contacts-section)
   (template . org-e-contacts-template)
   :export-block "ASCII"
   :filters-alist ((:filter-headline . org-e-contacts-filter-headline-blank-lines)
                   (:filter-section . org-e-contacts-filter-section-blank-lines))
   ))

(defun org-e-contacts-section (section contents info)
  "Transcode a SECTION element from Org to csv.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


(defun org-e-contacts-headline (headline contents info)
  "Transcode an HEADLINE element from Org to csv.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let ((level (org-element-property :level headline)))
    (concat
     (if (= level 2) (concat (org-element-property :raw-value headline) ","))
     contents)))

(defun org-e-contacts-template (contents info)
  "Return complete document string after contacts conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (org-element-normalize-string
   (concat
    "name,E-mail Address,Home Address,Mobile Phone\n"
    ;; Document's body.
     contents)))

(defun org-e-contacts-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((props (org-element-property :properties property-drawer)))
    (concat (cdr (assoc "EMAIL" props)) ","
            (cdr (assoc "ADDRESS" props)) ","
            (cdr (assoc "PHONE_MOBILE" props)) "\n")))


(defun org-e-contacts-filter-headline-blank-lines (headline back-end info)
  "Filter controlling number of blank lines after an headline.
This function only applies to `e-contacts' back-end.
For any other back-end, HEADLINE is returned as-is."
  (if (not (eq back-end 'e-contacts)) headline
    (replace-regexp-in-string "\n\\(?:\n[ \t]*\\)*\\'" ?\n headline)))

(defun org-e-contacts-filter-section-blank-lines (section back-end info)
  "Filter controlling number of blank lines after an headline.
This function only applies to `e-contacts' back-end.
For any other back-end, HEADLINE is returned as-is."
  (if (not (eq back-end 'e-contacts)) section
    (replace-regexp-in-string "\n\\(?:\n[ \t]*\\)*\\'" "" section)))



(defun org-e-contacts-export-as-csv
  (&optional subtreep visible-only body-only ext-plist)
  "Export current buffer to a csv buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org E-CONTACTS Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (let ((outbuf (org-export-to-buffer
                 'e-contacts "*Org E-CONTACTS Export*"
                 subtreep visible-only body-only ext-plist)))
    (with-current-buffer outbuf (text-mode))
    (when org-export-show-temporary-export-buffer
      (switch-to-buffer-other-window outbuf))))

(defun org-e-contacts-export-to-csv
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to a csv file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".csv" subtreep pub-dir)))
    (org-export-to-file
     'e-contacts outfile subtreep visible-only body-only ext-plist)))




(provide 'org-e-contacts)

;; test

;; (progn
;;  (setq org-export-show-temporary-export-buffer t)
;;  (set-buffer (find-file dpom-org-contacts-file))
;;  (goto-char (point-min))
;;  (org-e-contacts-export-as-csv))
