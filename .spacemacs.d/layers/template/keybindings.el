;;; keybindings.el --- template: keybindings        -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2017 Dan Pomohaci & Contributors
;;
;; Author: Dan Pomohaci <dan.pomohaci@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; Template key bindings.

;;; Code:
(spacemacs/declare-prefix "o" "personal")

(spacemacs/set-leader-keys
  "ot" 'template
  )

(spacemacs/declare-prefix-for-mode 'template-mode "mv" "conda")

(spacemacs/set-leader-keys-for-major-mode 'template-mode
  "vl" 'conda-env-list
  )
