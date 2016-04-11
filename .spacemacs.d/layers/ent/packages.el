;;; packages.el --- ent Layer packages File for Spacemacs
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

(setq ent-packages
  '((ent :location local)))

(defun ent/init-ent ()
  (use-package ent
    :commands ent
    :init (spacemacs/set-leader-keys "pz" 'ent)
    :config (progn
              (setq-default
               ent-default-task-number 100))
  ))
