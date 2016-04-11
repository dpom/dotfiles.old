(require 'dpom-utils)
(require 'esh-mode)
(require 'eshell)
(require 'org)
(require 'imenu)
(require 'thingatpt)

  (defgroup dpom nil
    "My custom variables"
    :version "1.0"
    :group 'tool)

  (defcustom  dpom-doc-dir "~/pers/doc/"
    "Documentation repository"
    :type '(string)
    :group 'dpom)
  ;;;###autoload
  (defun dpom-insert-day ()
    "Insert a day using calendar"
    (interactive)
    (require 'org)
    (insert (format "%s" (org-read-date nil nil nil "Day"))))

  ;;;###autoload
  (defun dpom-comment-or-uncomment-current-line-or-region ()
    "Comments or uncomments current current line or whole lines in region."
    (interactive)
    (save-excursion
      (let (min max)
        (if (and transient-mark-mode mark-active)
            (setq min (region-beginning) max (region-end))
          (setq min (point) max (point)))
        (comment-or-uncomment-region
         (progn (goto-char min) (line-beginning-position))
         (progn (goto-char max) (line-end-position))))))

  ;;;###autoload
  (defun dpom-get-doc-file ()
    "Find a a doc file using."
    (interactive)
    (let ((doc (completing-read "Choose document: "
                                    (mapcar  'file-name-sans-extension  (directory-files dpom-doc-dir nil ".*\.org$"))
                                    nil t)))
      (when doc
        (find-file (expand-file-name (concat doc ".org") dpom-doc-dir)))))

  ;;;###autoload
  (defun dpom-eshell-execute-current-line ()
    "Insert text of current line in eshell and execute."
    (interactive)
    (require 'eshell)
    (let ((command (buffer-substring
                    (save-excursion
                      (beginning-of-line)
                      (point))
                    (save-excursion
                      (end-of-line)
                      (point)))))
      (let ((buf (current-buffer)))
        (unless (get-buffer eshell-buffer-name)
          (eshell))
        (display-buffer eshell-buffer-name t)
        (switch-to-buffer-other-window eshell-buffer-name)
        (goto-char (point-max))
        (eshell-kill-input)
        (insert command)
        (eshell-send-input)
        (goto-char (point-max))
        (switch-to-buffer-other-window buf))))

    ;;;###autoload
  (defun untabify-buffer ()
    (interactive)
    (untabify (point-min) (point-max)))

    ;;;###autoload
  (defun indent-buffer ()
    (interactive)
    (indent-region (point-min) (point-max)))

    ;;;###autoload
  (defun cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a buffer."
    (interactive)
    (indent-buffer)
    (untabify-buffer)
    (delete-trailing-whitespace))

  (defun eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))

  ;; (defun eval-and-replace (value)
  ;;   "Evaluate the sexp at point and replace it with its value"
  ;;   (interactive (list (eval-last-sexp nil)))
  ;;   (kill-sexp -1)
  ;;   (insert (format "%S" value)))

  ;; Cosmetic

    ;;;###autoload
  (defun pretty-lambdas ()
    (font-lock-add-keywords
     nil `(("(?\\(lambda\\>\\)"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(make-char 'greek-iso8859-7 107))
                      nil))))))


    ;;;###autoload
  (defun sudo-edit (&optional arg)
    (interactive "p")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  ;; A monkeypatch to cause annotate to ignore whitespace
    ;;;###autoload
  (defun vc-git-annotate-command (file buf &optional rev)
    (let ((name (file-relative-name file)))
      (vc-git-command buf 0 name "blame" "-w" rev)))


    ;;;###autoload
  (defun sm-try-smerge ()
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))
  ;;;###autoload
  (defun dpom-create-tags (dir-name)
      "Create tags file."
      (interactive "DDirectory: ")
      (shell-command
       (format "%s -f %s/TAGS -e -R %s" "/usr/bin/ctags"  dir-name (directory-file-name dir-name)))
    )
  ;;;###autoload
(defun dpom-org-surround-block (beg end &optional tag)
  (interactive "r")
  (if (not tag) (setq tag (read-string "type: ")))
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (insert "#+begin_" tag "\n")
    (goto-char (point-max))
    (insert "#+end_" tag "\n")))

  ;;;###autoload
(defun dpom-org-surround-src (beg end)
  (interactive "r")
  (dpom-org-surround-block beg end "src"))

  ;;;###autoload
(defun dpom-babel (beg end)
  (interactive "r")
  (babel-region beg end t))

  ;;;###autoload
(defun dpom-org-surround-example (beg end)
  (interactive "r")
  (dpom-org-surround-block beg end "example"))

  ;;;###autoload
(defun bh/org-agenda-to-appt ()
  "Erase all reminders and rebuilt reminders for today from the agenda"
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

  ;;;###autoload
(defun dpom-org-manual ()
  (interactive)
  (info "~/.emacs.d/info/org"))

  ;;;###autoload
(defun dpom-screenshot ()
  "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
  (interactive)
  (let ((filename (concat (make-temp-name (file-name-directory (buffer-file-name))) ".jpg" )))
    (call-process "import" nil nil nil filename)
    (insert (concat "[[" filename "]]"))
    (org-display-inline-images)))


;;;###autoload
(defun dpom-insert-file-as-org-table (filename)
  "Insert a csv file (; separator) into the current buffer at point, and convert it to an org table."
  (interactive (list (read-file-name "csv file: ")))
  (let* ((start (point))
         (end (+ start (nth 1 (insert-file-contents filename)))))
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (search-forward ";" nil t) (replace-match "|"))
      )
    (org-table-convert-region start end)
    ))
;;;###autoload
(defun dpom-org-babel-detangle (&optional source-code-file)
  "Propagate changes in source file back original to Org-mode file.
This requires that code blocks were tangled with link comments
which enable the original code blocks to be found."
  (interactive)
  (save-excursion
    (when source-code-file (find-file source-code-file))
    (message "detangled %d code blocks" (dpom-org-babel-detangle-region (point-min) (point-max)))))


(defun dpom-org-babel-detangle-region (beg end)
  (save-excursion
    (goto-char beg)
    (let ((counter 0) new-body start next)
      (while (re-search-forward org-bracket-link-analytic-regexp end t)
        (setq start (match-end 0))
        (when (re-search-forward (concat " " (regexp-quote (match-string 5)) " ends here"))
          (setq next (match-end 0))
          (forward-line -1)
          (if (dpom-org-babel-contain-embedded-src-p start next)
              (setq counter (+ counter (dpom-org-babel-detangle-region start next)))
            (save-excursion
              (when (setq new-body (org-babel-tangle-jump-to-org))
                (org-babel-update-block-body new-body)
                (setq counter (+ 1 counter))))))
          (goto-char next))
      counter)))


(defun dpom-org-babel-contain-embedded-src-p (beg end)
  "Return true if there are source blocks embedded."
  (save-excursion
      (goto-char beg)
      (if (re-search-forward org-bracket-link-analytic-regexp end t) t nil)))

(defun dpom-test-org-babel-contain-embedded-src-p (beg end)
  (interactive "r")
  (message "dpom-org-babel-contain-embedded-src-p result: %s" (dpom-org-babel-contain-embedded-src-p beg end)))
;(require 'slime-autoloads)

;;;###autoload
(defun dpom-check-region-parens ()
  "Check if parentheses in the region are balanced. Signals a
scan-error if not."
  (interactive)
  (save-restriction
    (save-excursion
    (let ((deactivate-mark nil))
      (condition-case c
          (progn
            (narrow-to-region (region-beginning) (region-end))
            (goto-char (point-min))
            (while (/= 0 (- (point)
                            (forward-list))))
            t)
        (scan-error (signal 'scan-error '("Region parentheses not balanced"))))))))


;;;###autoload
(defun dpom-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(defcustom dpom-xml-schema-list ()
  "Validation schema list"
  :type 'list
  :group 'dpom)

(defvar dpom-log-buffer (concat "*Dpom-Log*")
  "Log buffer")

;;;###autoload
(defun dpom-format-xml (arg)
  (interactive "P")
  "Entry point for xml formating commands. Without an argument
nice indent the xml file, with argument compact it"
  (if (null arg) (dpom-indent-xml) (dpom-compact-xml)))

(defun dpom-compact-xml (&optional file)
  "Suppress all white spaces between > and < from the current buffer"
  (goto-char (point-min))
  (while (re-search-forward ">[ \n\t]*<" nil t)
    (replace-match "><" nil nil)))


(defun dpom-indent-xml (&optional file)
  "Prety indent the current xml buffer"
  (goto-char (point-min))
  (while (re-search-forward "><" nil t)
    (replace-match ">\n<" nil nil))
  (goto-char (point-min))
  (while (not (= (point) (point-max)))
    (indent-for-tab-command)
    (forward-line)))

;;;###autoload
(defun dpom-complete-surround-tag (beg end)
  (interactive "r")
  (let ((tag (read-string "tag: ")))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (insert "<" tag ">\n")
      (goto-char (point-max))
      (insert "</" tag ">\n"))))

;;;###autoload
(defun dpom-get-xpath (&optional print-message)
  "Return all the elements in the ancestor axis of the current
    element.  If called interactively, show it in the echo area."
  (interactive "p")
  (nxml-ensure-scan-up-to-date)
  (let ((path (mapconcat #'(lambda (x) x) (xml-remove-complete-elements (xml-get-tags (current-buffer) (point))) "/")))
    (when print-message
      (message "%s" path))
    (kill-new path)
    path))


(defun xml-get-tags (buf pos)
  "Get all start/end tags from begining of BUF till POS."
    (set-buffer buf)
    (goto-char pos)
    (setq pos (search-forward ">"))
    (goto-char (point-min))
    (let ((ellist ()))
      (while (and (re-search-forward "<\\([^?]/?[a-z-A-Z]*\\)" nil t)
                  (< (point) pos))
        (setq ellist (cons (substring-no-properties (match-string 1)) ellist)))
      ellist))

(defun xml-remove-complete-elements (lst)
  "Remove complete elements start/end tags from LST."
  (let ((acc ())
        (tag nil))
    (mapc #'(lambda (x)
              (if (null tag)
                  (if (equal ?/ (elt x 0)) (setq tag (substring x 1))(setq acc (cons x acc)))
                (if (string= tag x) (setq tag nil))))
          lst)
    acc))



;; add msv to the systems list for the error alist creation
(defvar compilation-error-regexp-alist-alist)
(defvar compilation-error-regexp-alist)
(defvar msv-error-regexp
  '(msv "^\\(Fatal \\)?[Ee]rror at line:\\([0-9,]+\\), column:\\([0-9]+\\) of file:\\(.+\\)" 4 2 3))

(unless (assq 'msv  compilation-error-regexp-alist-alist)
  (setq compilation-error-regexp-alist-alist (cons msv-error-regexp compilation-error-regexp-alist-alist)))

(setq compilation-error-regexp-alist (mapcar 'car compilation-error-regexp-alist-alist))



;;;###autoload
(defun dpom-validate-xml (&optional file)
  "Validate current xml buffer"
  (interactive)
  (if (null file) (setq file (file-name-nondirectory buffer-file-name)))
  (compile (concat "xmlValidate"
                   " " (expand-file-name (cdr (assoc (completing-read "Schema: " dpom-xml-schema-list)
                                                     dpom-xml-schema-list)))
                   " " (expand-file-name file)) t))

;;;###autoload
(defun dpom-use-xsl-with-outfile (&optional file)
  "Use the actual xsl buffer or the FILE to transform an xml file.
The result is saved in a file."
  (interactive)
  (if (null file) (setq file (file-name-nondirectory buffer-file-name)))
  (compile (concat "xmlTransform"
                   " " (expand-file-name file)
                   " " (expand-file-name (read-file-name "Xml file: "))
                   " " (expand-file-name (read-file-name "Output file: ")))
                    t))

;;;###autoload
(defun dpom-use-xsl (&optional file)
  "Use the actual xsl buffer or the FILE to transform an xml file."
  (interactive)
  (if (null file) (setq file (file-name-nondirectory buffer-file-name)))
  (compile (concat "xmlTransform"
                   " " (expand-file-name file)
                   " " (expand-file-name (read-file-name "Xml file: ")))
                    t))



(defun dpom-log (log &rest args)
  "Log a message or the contents of a buffer.
If LOG is a string and there are more args, it is formatted with
those ARGS.  Usually the LOG string ends with a \n.  End each
bunch of errors with (dpom-log t): this inserts the current time
and buffer at the start of the page, and \f (formfeed) at the
end."
  (let ((obuf (current-buffer)))
    (with-current-buffer (get-buffer-create dpom-log-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (cond ((stringp log)
               (insert (if args
                           (apply (function format) log args)
                         log)))
              ((bufferp log)
               (insert-buffer-substring log))
              ((eq t log)
               (goto-char (point-max))
               (insert "\n\n")
               (insert (current-time-string) "\n")))
          (sit-for 0)))))

(provide 'dpom-commands)
