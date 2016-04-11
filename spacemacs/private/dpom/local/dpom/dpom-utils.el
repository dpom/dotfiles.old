(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-hl-line-mode ()
  (if window-system (hl-line-mode t)))

(defun turn-on-save-place-mode ()
  (setq save-place t))

(defun turn-on-whitespace ()
  (whitespace-mode t))

(defun turn-off-tool-bar ()
  (tool-bar-mode -1))
  (defun dpom-clock-in-to-started (kw)
  "Switch task from TODO to STARTED when clocking in.
  Skips capture tasks and tasks with subtasks"
  (if (and (string-equal kw "TODO")
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (has-subtask nil))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-not-done-keywords)
              (setq has-subtask t))))
        (when (not has-subtask)
          "STARTED"))))
(defun dpom-csv-buffer-to-list ()
  "Return a list wich elements are current buffer lines starting from point"
  (let (result start)
    (while (not (eobp))
      (setq start (line-beginning-position))
      (end-of-line)
      (setq result (append result (list
                                   (buffer-substring-no-properties
                                    start (point)))))
      (forward-line 1))
    result))


(defun dpom-csv-to-list (&optional sep)
  "Convert a csv buffer in a list. Each element of the list is a
  list containing csv fields."
  (mapcar #'(lambda (x)
              (split-string x (or sep ",")))
          (dpom-csv-buffer-to-list)))


(defun dpom-csv-file-to-list (file &optional separator)
    "Convert a csv file FILE in a list. Each element of the list is a
  list containing csv fields."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (dpom-csv-to-list separator)))


(defsubst dpom-csv-string-trim (string seq)
  "Lose leading and trailing whitespace.  Also remove all properties
from string."
  (if (string-match (concat "\\`[" seq "]+") string)
      (setq string (substring string (match-end 0))))
  (if (string-match (concat "[" seq "]+\\'") string)
      (setq string (substring string 0 (match-beginning 0))))
  (set-text-properties 0 (length string) nil string)
  string)

(provide 'dpom-utils)
