;;; config.el --- ent Layer configuration File for Spacemacs
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

(defvar maven-cmd "~/bin/m3 " "maven command")
(defvar java-home "/usr/lib/jvm/default-java/" "java home dir" )
(defvar generated-autoload-file "" "generated autoload file name")
(defvar info-dir "~/.emacs.d/info/"  "local info directory")

(defvar ent-cli () "(template . script file) pair alist")

(defvar ent-init-file (expand-file-name "ent-tasks.el" "~/pers/dotfiles/.emacs.d/private/ent/"))
