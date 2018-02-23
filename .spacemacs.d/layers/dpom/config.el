  ;;; config.el --- dpom Layer configuration File for Spacemacs
  ;;
  ;; Copyright (c) 2012-2014 Sylvain Benner
  ;; Copyright (c) 2014-2015 Dan Pomohaci & Contributors
  ;;
  ;; Author: Dan Pomohaci <dan.pomohaci@gmail.com>
  ;; URL: https://github.com/syl20bnr/spacemacs
  ;;
  ;; This file is not part of GNU Emacs.
  ;; 
  ;;; License: GPLv3
(defvar dpom/pers-dir "~/pers/")
(defvar dpom/org-dir (expand-dir-name "plan/" dpom/pers-dir))
(defvar dpom/dpom-layer (expand-dir-name "dpom/" (expand-file-name "layers/" dotspacemacs-directory)))
(defvar dpom/org-todo-file (expand-file-name "todo.org" dpom/org-dir))
(defvar dpom/org-notes-file (expand-file-name "notes.org" dpom/org-dir))
(defvar dpom/org-contacts-file (expand-file-name "contacts.org" dpom/pers-dir))
;; dpom-config ends here
