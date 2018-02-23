;;; keybindings.el --- dpom: keybindings        -*- lexical-binding: t; -*-
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

;;; Commentary:

;; My personal key bindings.

;;; Code:
(spacemacs/declare-prefix "o" "personal")
(spacemacs/declare-prefix "ob" "babel")
(spacemacs/declare-prefix "om" "math")

(spacemacs/set-leader-keys
  "oa" 'org-agenda
  "og" 'helm-org-agenda-files-headings
  "oo" 'org-clock-out
  "oc" 'org-capture
  "op" 'org-pomodoro 
  "oC" 'helm-org-capture-templates ;requires templates to be defined.
  "ol" 'org-store-link
  "obd" 'org-babel-detangle
  "obt" 'org-babel-tangle
  "omi" 'cdlatex-math-symbol
  "omv" 'org-toggle-latex-fragment
  "os" 'fixup-whitespace
  "oi" 'dpom/insert-rom-char)

(spacemacs/set-leader-keys "xf" 'dpom/cleanup-buffer)

(spacemacs/set-leader-keys-for-major-mode 'web-mode
  "mrk" 'web-mode-element-kill)



