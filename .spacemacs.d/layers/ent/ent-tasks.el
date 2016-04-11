;;; ent-tasks.el --- ent default tasks File for Spacemacs
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

(require 'ent)
(ent-init)

(task 'env () "env init" '(lambda (&optional x)
                            (concat "cd " ent-project-home ";"
                                    "export JAVA_HOME=" java-home)))

(task 'genautoload () "generate project autoloads file" '(lambda (&optional x)
                                                           (ent-emacs "generate-autoload" x)))

(task 'info () "install info file" '(lambda (&optional x)
                                      (ent-emacs "install-info" x)))


(task 'tangle '() "tangle config file" '(lambda (&optional x) (ent-emacs "ent-tangle" (expand-file-name ent-file-name ent-project-home))))
