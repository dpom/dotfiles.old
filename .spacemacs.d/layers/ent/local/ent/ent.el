;;; ent.el --- emacs build tool
;; Copyright (C) 2009 Dan Pomohaci (dpom)

;; Author: Dan Pomohaci <dan.pomohaci@gmail.com>
;; Version: 0.3
;; Keywords: build ant task 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License.

;;; Commentary:


;;; Code:

(require 'bytecomp)
(require 'dired)

(eval-when-compile (require 'cl))


;;;; Custom variables

(defgroup ent nil
  "Emacs build tool"
  :version "1.0"
  :group 'tools
  :group 'processes)


(defcustom ent-emacs-exec "emacs"
  "Emacs executable"
  :type '(string)
  :group 'ent)

(defcustom ent-file-name ".build.el"
  "Local ent file name"
  :type '(string)
  :group 'ent)


(defcustom ent-init-file "~/.emacs.d/init-ent.el"
  "Global ent init file name"
  :type '(string)
  :group 'ent)

(defcustom ent-default-task-number 80
  "Default task number"
  :type '(integer)
  :group 'ent)


(defcustom ent-default-task 'clean
  "Default task"
  :type '(symbol)
  :group 'ent)


(defcustom ent-dirclean-default-regexp "target"
  "Default regular expression to match garbage directories"
  :type '(string)
  :group 'ent)


(defcustom ent-clean-default-regexp "~$"
  "Default regular expression to match garbage files"
  :type '(string)
  :group 'ent)


(defcustom ent-elisp-default-src-dir "src/main/lisp/"
  "Default lisp source directory"
  :type '(string)
  :group 'ent)

(defcustom ent-project-config-filename "project.clj"
  "Project specific config file name"
  :type '(string)
  :group 'ent)


(defcustom ent-default-exclude-dir-list (list "*.git" "*target")
  "The list of directories to be excluded in file search."
  :type '(list)
  :group 'ent)

;;;; Global variables

(defvar ent-project-name "" "Project name, must be set in the project init file.")

(defvar ent-project-home "" "Project home directory")

(defvar ent-tasks () "Local tasks list")

(defvar ent-clean-regexp "" "Regular expression to match garbage files")

(defvar ent-dirclean-regexp "" "Regular expression to match garbage directories")

(defvar ent-elisp-src-dir "" "Lisp source directory")

(defvar ent-mcopy-list () "Multiple copy list each element is \(SRC DEST PATTERN\).The SRC and DEST must be absolute path.")

(defvar ent-generated-autoload-file "")

(defvar ent-exclude-dir-list () "Directories to be excluded from file search.")

;;;; Utility functions
(defun expand-dir-name (dir &optional path)
  "Expand a local dir NAME using his PATH."
  (file-name-as-directory (expand-file-name dir path)))

(defun ent-walk
 (dir regexp function)
  "Walk DIR recursively and execute FUNCTION for REGEXP match files"
  (cond
   ((file-regular-p dir) (and (string-match regexp dir) (funcall function dir)))
   ((file-directory-p dir) (mapc #'(lambda (x) (ent-walk x regexp function))
                                 (directory-files (expand-file-name dir) t "[^.]$")))))

(defun ent-dir-walk (dir regexp function)
  "Walk DIR recursively and execute FUNCTION for REGEXP match directories"
  (if (file-directory-p dir)
      (if (string-match regexp dir) (funcall function dir)
        (mapc #'(lambda (x) (ent-dir-walk x regexp function))
              (directory-files (expand-file-name dir) t "[^.]$")))))

(defun ent-rcopy (src dest regexp)
  "Recursive copy REGEXP files from SRC to DEST"
  (ent-walk src regexp '(lambda (x)
                          (message "copy %s to %s" x dest)
                          (make-directory dest t)
                          (copy-file x dest t))))

(defun ent-mcopy ()
  "Multiple recursive copy using ent-mcopy-list."
  (mapc #'(lambda (y)
           (message "%s" y)
           (apply 'ent-rcopy y))
        ent-mcopy-list))

(defsubst ent-get-name (tsk) (symbol-name tsk))
(defsubst ent-get-dependencies (tsk) (get tsk :dependencies))
(defsubst ent-get-description (tsk) (get tsk :description))
(defsubst ent-get-function (tsk) (symbol-function tsk))
(defsubst ent-put-dependencies (tsk val) (put tsk :dependencies val))
(defsubst ent-put-description (tsk val) (put tsk :description val))
(defsubst ent-put-function (tsk val) (fset tsk val))
(defsubst ent-get-task (name) (intern-soft name ent-tasks))
(defsubst ent-has-function (tsk) (fboundp tsk))
(defsubst ent-remove-function (tsk) (fmakunbound tsk))

(defun flatten (x)
  (cl-labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun ent-parse-dep (dep subprojects dir)
  "Parse DEP and return a dependency item \(TASK PROJECT-DIR\)"
  (if (listp dep)
      (list (car dep) (expand-file-name (cdr (assoc (cdr dep) subprojects)) dir))
    (list dep dir)))

(defun ent-get-dep-list (name)
  "create a complete TSK task dependencies list"
  (let ((tsk (intern (symbol-name name) ent-tasks)))
    (delete-dups (flatten (append (mapcar 'ent-get-dep-list  (ent-get-dependencies tsk)) name)))))

(defun ent-batch (dir name tasks &optional file)
  "Run the NAME task dep list in DIR directory using FILE as init file"
  (compile (concat "cd " dir ";"
                   (mapconcat #'(lambda (x)
                                 (let ((tsk (intern (symbol-name x) tasks)))
                                   (if (ent-has-function tsk)
                                       (funcall tsk file))))
                              (ent-get-dep-list name)
                              ";"))))

(defun ent-emacs (tsk file)
  "Run a batch emacs loading FILE and evaluating TSK function"
  (concat ent-emacs-exec " --batch -u dan --kill -l " file " -f " tsk))

(defun task (name depends desc &optional func)
  "Insert NAME task in ent-tasks-list"
  (let ((tsk (intern (symbol-name name) ent-tasks)))
    (ent-put-dependencies tsk depends)
    (ent-put-description tsk desc)
    (if func (ent-put-function tsk func)
      (ent-remove-function tsk))))

(defun ent-find-project-file ()
  "Return the project file absolute path of the current project."
  (expand-file-name ent-file-name (locate-dominating-file default-directory ent-file-name)))

;;;; Global tasks

;; clean
(defun ent-clean-task  (&optional regexp)
  "Remove all files matching ent-clean-regexp from current dir recursively."
  (let ((acc 0)
        (dir default-directory)
        (regexp (or regexp ent-clean-regexp ent-clean-default-regexp)))
    (displaying-byte-compile-warnings
     (message "\nClean: %s from %s" regexp dir)
     (ent-walk dir regexp #'(lambda (x)
                              (delete-file (expand-file-name x))
                              (setq acc (+ acc 1))
                              (message "%s deleted" (expand-file-name x))))
     (message "Clean: command terminated %d files removed" acc)
     acc)))

(defun ent-clean-init ()
  (task 'clean ()  (documentation 'ent-clean-task) '(lambda (&optional file)
                                                      (ent-emacs "ent-clean-task" file))))

;; dirclean
(defun ent-dirclean-task  (&optional regexp)
  "Remove all directories matching ent-dirclean-regexp from current dir recursively."
  (let ((acc 0)
        (dir default-directory)
        (regexp (or regexp ent-dirclean-regexp ent-dirclean-default-regexp)))
    (displaying-byte-compile-warnings
     (message "\nDirClean: %s from %s" regexp dir)
     (ent-dir-walk dir regexp #'(lambda (x)
                                  (dired-delete-file (expand-file-name x) 'always)
                                  (setq acc (+ acc 1))
                                  (message "%s deleted" (expand-file-name x))))
     (message "DirClean: command terminated %d directories removed" acc)
     acc)))

(defun ent-dirclean-init ()
  (task 'dirclean ()  (documentation 'ent-dirclean-task) '(lambda (&optional file)
                                                            (ent-emacs "ent-dirclean-task" file))))


;; help
(defun ent-help-task (&optional arg)
  "Display tasks description"
  (displaying-byte-compile-warnings
   (message "\nHelp:")
   (mapatoms #'(lambda (x) (message "%s\t- %s" (ent-get-name x) (ent-get-description x)))
             ent-tasks)))

(defun ent-help-init ()
  (task 'help ()  (documentation 'ent-help-task) '(lambda (&optional file)
                                                    (ent-emacs "ent-help-task" file))))

;; elispbuild
(defun ent-elispbuild-task (&optional arg)
  "Build an elisp project"
  (let ((dir (expand-file-name (or ent-elisp-src-dir ent-elisp-default-src-dir))))
    (displaying-byte-compile-warnings
     (message "Project %s\n" ent-project-name)
     (message "Build: compile el files found in %s" dir)
     (byte-recompile-directory dir 0)
     (message "\nBuild: bin-compile command terminated"))))

(defun ent-elispbuild-init ()
  (task 'elispbuild () (documentation 'ent-elispbuild-task) '(lambda (&optional file)
                                                               (ent-emacs "ent-elispbuild-task" file))))

;; mcopy
(defun ent-mcopy-task  (&optional regexp)
  "Multiple copy using ent-mcopy-list  recursively."
  (let ((dir default-directory))
    (displaying-byte-compile-warnings
     (message "\nMCopy")
     (ent-mcopy)
     (message "\nMCcopy: command terminated"))))

(defun ent-mcopy-init ()
  (task 'mcopy ()  (documentation 'ent-mcopy-task) '(lambda (&optional file)
                                                      (ent-emacs "ent-mcopy-task" file))))

;;;; Tasks initialization
(defun ent-init (&optional maxtasks)
  "Initialize the global variables with default values"
  (if (not maxtasks) (setq maxtasks ent-default-task-number))
  (setq ent-tasks (make-vector maxtasks 0))
  (ent-clean-init)
  (ent-dirclean-init)
  (ent-help-init)
  (ent-elispbuild-init)
  (ent-mcopy-init))

;;;; Commands

;;;###autoload
(defun ent (&optional taskname)
  "Main entry point for ent"
  (interactive)
  (let ((initfile)
        (dir (locate-dominating-file default-directory ent-file-name)))
    (if dir
        (load (setq initfile (expand-file-name ent-file-name dir)))
      (progn
        (load (setq initfile ent-init-file))
        (setq dir default-directory)))
    (if (not taskname)
        (setq taskname (ido-completing-read  "Command: "
                                             (mapcar 'ent-get-name (remove-if-not 'symbolp ent-tasks))
                                             nil t)))
    (ent-batch dir (ent-get-task taskname) ent-tasks initfile)))

;;;###autoload
(defun ent-visit-build-file ()
  "Visit the ent project file."
  (interactive)
  (find-file-other-window (ent-find-project-file)))

(defmacro ent-in-project (&rest body)
  "Execute body for the curent project."
  `(if (file-exists-p (ent-find-project-file))
       (progn
         (load (ent-find-project-file))
         ,@body)
     (message "Not in a project!")))

;;;###autoload
(defun ent-visit-config-file ()
  "Visit the project specific config file, ent-project-config-filename."
  (interactive)
  (ent-in-project
   (find-file-other-window (expand-file-name ent-project-config-filename ent-project-home))))

;;;###autoload
(defun ent-find-file ()
  "Find a file from the project."
  (interactive)
  (ent-in-project
   (let (project-files tbl)
     (setq project-files 
           (split-string 
            (shell-command-to-string 
             (concat "find "
                     ent-project-home
                     " \\( "
                     (mapconcat (function (lambda (x) (concat "-name \"" x "\""))) (or ent-exclude-dir-list ent-default-exclude-dir-list) " -o  ")
                     " \\) -prune -o -type f -print | grep -v \"" (or ent-clean-regexp ent-clean-default-regexp) "\""))
            "\n"))
     ;; populate hash table (display repr => path)
     (setq tbl (make-hash-table :test 'equal))
     (let (ido-list)
       (mapc (lambda (path)
               (let (key)
                 ;; format path for display in ido list
                 (setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" path))
                 ;; strip project root
                 (setq key (replace-regexp-in-string ent-project-home "" key))
                 ;; remove trailing | or /
                 (setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
                 (puthash key path tbl)
                 (push key ido-list)))
             project-files)
       (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl))))))

;; Key map

;;;###autoload
(defvar ent-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "x" 'ent)
    (define-key map "c" 'ent-visit-config-file)
    (define-key map "b" 'ent-visit-build-file)
    (define-key map "f" 'ent-find-file)
    map))

;;;###autoload
(fset 'ent-prefix-map ent-prefix-map)



(provide 'ent)

;;; ent.el ends here

