;;; funcs.el --- myediting Layer functions File for Spacemacs
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


(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))


(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))


(defun dpom/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))



(defun dpom/insert-char (c)
  (delete-char 1)
  (insert c))

(defun dpom/insert-rom-char (c)
  (interactive "cchar:")
  (case c
    ((?i)
     (dpom/insert-char "î"))
    ((?s)
     (dpom/insert-char "ş"))
    ((?t)
     (dpom/insert-char "ţ"))
    ((?a)
     (dpom/insert-char "ă"))
    ((?q)
     (dpom/insert-char "â"))
    ))
