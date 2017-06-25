
;;; packages.el --- template layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Dan Pomohaci <dan.pomohaci@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;;
;;
;;; Code:

(defconst template-packages
  '(
    (template :location local)
    )
  "The list of Lisp packages required by the template layer.")

(defun template/init-template ()
  (use-package template
    :defer t
    :init
    :config
    :commands (template )
    :mode "\.el"
    )
  )



;;; packages.el ends here
