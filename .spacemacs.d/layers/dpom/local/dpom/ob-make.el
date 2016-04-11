;;; ob-make.el --- org-babel functions for make evaluation

;; Copyright (C) Dan Pomohaci

;; Author: Dan Pomohaci
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:


;;; Requirements:


;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'make-mode)


;; declare default header arguments for this language
(defvar org-babel-default-header-args:make '())

(defun org-babel-execute:make (body params)
  "Execute a block of MAKE code.
This function is called by `org-babel-execute-src-block'."
 body)


(defun org-babel-prep-session:make (session params)
  "Return an error if the :session header argument is set."
  (error "MAKE sessions are nonsensical"))


(provide 'ob-make)
;;; ob-make.el ends here
