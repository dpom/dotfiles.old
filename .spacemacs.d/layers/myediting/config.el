  ;;; config.el --- myediting Layer configuration File for Spacemacs
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
(when (configuration-layer/package-usedp 'dpom)
  (defvar dpom/bibliography-dir (expand-dir-name "bibliography/" dpom/pers-dir))
  (defvar dpom/default-bibliography (expand-file-name "dpom.bib" dpom/bibliography-dir))
  (defvar dpom/bibliography-pdfs-dir (expand-dir-name "pdfs/" dpom/bibliography-dir))
  )
;; myediting-config ends here
