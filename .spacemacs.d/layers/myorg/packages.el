;;; packages.el --- myorg Layer packages File for Spacemacs
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

(setq myorg-packages
      '(
        cdlatex
        (ox-reveal :location local)
        ox-rst
        org-ref
        (org-config      :location local)
        ))

(defun myorg/init-ox-rst ()
  "Initialize ox-rst package."
  (use-package ox-rst))

(defun myorg/post-init-ox-reveal ()
  "Initialize ox-reveal package."
  (use-package ox-reveal
    :config (progn
              (setq org-reveal-external-plugins  '((menu . "{ src: './plugins/menu/menu.js', async: true}")
                                                   (toolbar . "{ src: './plugins/toolbar/toolbar.js', async: true}")))
              ;; (setq org-reveal-root "")
              )))

(defun myorg/init-cdlatex ()
  "Initialize cdlatex extension"
  (use-package cdlatex))


(defun myorg/init-org-config ()
  (use-package org-config
    :after org macros))

(defun myorg/post-init-org-ref ()
  "Initialize org-ref extension"
  (use-package org-ref
    :config (progn
              (setq reftex-default-bibliography (list dpom/default-bibliography)
                    org-ref-bibliography-notes dpom/org-notes-file
                    org-ref-default-bibliography (list dpom/default-bibliography)
                    org-ref-pdf-directory dpom/bibliography-pdfs-dir)
              (setq helm-bibtex-bibliography dpom/default-bibliography
                    helm-bibtex-library-path dpom/bibliography-pdfs-dir)

              (unless (file-exists-p org-ref-pdf-directory)
                (make-directory org-ref-pdf-directory t))
              ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode
              ;;   "rr" 'org-ref-helm-insert-cite-link
              ;;   "rl" 'org-ref-helm-insert-label-link
              ;;   "rf" 'org-ref-helm-insert-ref-link
              ;;   )
              (setq bibtex-autokey-year-length 4
                    bibtex-autokey-name-year-separator "-"
                    bibtex-autokey-year-title-separator "-"
                    bibtex-autokey-titleword-separator "-"
                    bibtex-autokey-titlewords 2
                    bibtex-autokey-titlewords-stretch 1
                    bibtex-autokey-titleword-length 5)
              (require 'dash)
              (setq org-latex-default-packages-alist
                    (-remove-item
                     '("" "hyperref" nil)
                     org-latex-default-packages-alist))

              ;; Append new packages
              ;; (add-to-list 'org-latex-default-packages-alist '("" "natbib" "") t)
              (add-to-list 'org-latex-default-packages-alist
                           '("linktocpage,pdfstartview=FitH,colorlinks,
linkcolor=blue,anchorcolor=blue,
citecolor=blue,filecolor=blue,menucolor=blue,urlcolor=blue"
                             "hyperref" nil))

              ;; (require 'org-re-pdf)
              ;; (require 'org-ref-url-utils)
              ;; (require 'doi-utils)
              ))
  )




;; myorg-package ends here
