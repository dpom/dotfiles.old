;;; keybindings.el --- myediting: keybindings        -*- lexical-binding: t; -*-
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

;; My personal key bindings.

;;; Code:

(spacemacs/declare-prefix "xi" "string-inflection")

(spacemacs/set-leader-keys
  "xii" 'string-inflection-all-cycle
  "xiu" 'string-inflection-underscore
  "xiU" 'string-inflection-upcase
  "xik" 'string-inflection-kebab-case
  "xic" 'string-inflection-lower-camelcase
  "xiC" 'string-inflection-camelcase)

(spacemacs/declare-prefix "od" "doxygen")
(spacemacs/set-leader-keys
  "od?" 'doxymacs-lookup
  "odr" 'doxymacs-rescan-tags
  "odf" 'doxymacs-insert-function-comment
  "odi" 'doxymacs-insert-file-comment
  "od;" 'doxymacs-insert-member-comment
  "odm" 'doxymacs-insert-blank-multiline-comment
  "ods" 'doxymacs-insert-blank-singleline-comment
  "od@" 'doxymacs-insert-grouping-comments)

(spacemacs/declare-prefix "om" "math")
(spacemacs/set-leader-keys
  "omi" 'cdlatex-math-symbol
  "omv" 'org-toggle-latex-fragment
  "os" 'fixup-whitespace
  "oi" 'dpom/insert-rom-char)

(spacemacs/set-leader-keys "xf" 'dpom/cleanup-buffer)

(spacemacs/set-leader-keys-for-major-mode 'web-mode
  "mrk" 'web-mode-element-kill)

