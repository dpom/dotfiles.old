;;; packages.el --- myediting layer packages file for Spacemacs.
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
;; Many evil-mode motions/operators will have slightly different behavior while
;; evil-multiedit is active or the cursor is in an iedit region:
;;
;; D: clear the region
;; C: clear to end-of-region and go into insert mode
;; A: go into insert mode at end-of-region
;; I: go into insert mode at start-of-region
;; V: select the region
;; $: go to end-of-region
;; 0/^: go to start-of-region
;; gg/G: go to the first/last region
;;
;;; Code:

(defconst myediting-packages
  '(evil-multiedit
    org
    cdlatex
    (ox-reveal :location (recipe :fetcher github :repo "yjwen/org-reveal"))
    ox-rst
    (doxymacs :location local)
    string-inflection)
  "The list of Lisp packages required by the myediting layer.")


(defun myediting/init-evil-multiedit ()
  (use-package evil-multiedit
    :bind (:map evil-normal-state-map
             ("M-d" . evil-multiedit-match-and-next)
             ("M-D" . evil-multiedit-match-and-prev)
           :map evil-visual-state-map
             ("M-d" . evil-multiedit-match-and-next)
             ("M-D" . evil-multiedit-match-and-prev)
             ("R" . evil-multiedit-match-all)
             ("C-M-D" . evil-multedit-restore)
           :map evil-multiedit-state-map
             ("C-n" . evil-multiedit-next)
             ("C-p" . evil-multiedit-prev)
             ("RET" . evil-multiedit-toggle-or-restrict-region)
           :map evil-multiedit-insert-state-map
             ("C-n" . evil-multiedit-next)
             ("C-p" . evil-multiedit-prev)
            )
    :config (progn
              ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
              (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match))
    ))

(defun myediting/init-string-inflection ()
    (use-package string-inflection))

(defun myediting/init-doxymacs ()
  (use-package doxymacs
    :init
    (progn
      (defun my-doxymacs-font-lock-hook ()
        (if (or (eq major-mode 'c-mode)
                (eq major-mode 'python-mode)
                (eq major-mode 'c++-mode))
            (doxymacs-font-lock)))
      (add-hook 'prog-mode-hook '(lambda () (doxymacs-mode)))
      (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      (defun my-doxymacs-font-lock-hook ()
        (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
            (doxymacs-font-lock)))
      (setq doxymacs-doxygen-style "Python")
      )))

(defun myediting/init-ox-rst ()
  "Initialize ox-rst package."
  (use-package ox-rst))

(defun myediting/post-init-ox-reveal ()
  "Initialize ox-reveal package."
  (use-package ox-reveal
    :config (progn
              ;; (setq org-reveal-root "")
              )))

(defun myediting/init-cdlatex ()
  "Initialize cdlatex extension"
  (use-package cdlatex))




(defun myediting/post-init-org ()
  ;; Explicitly load required exporters
  (require 'ox-html)
  (require 'ox-latex)
  (require 'ox-ascii)
  (require 'ox-md)
  ;; (require 'ox-bibtex)

  (setq org-table-export-default-format 'orgtbl-to-csv
        org-md-headline-style 'atx)

  (setq org-latex-listings 'listings)
  (setq org-latex-listings-options
        '(("frame" "single")
          ("basicstyle" "\\small\\sffamily")
          ("numbers" "left")
          ("numberstyle" "\\tiny")
          ("columns" "fullflexible")
          ("backgroundcolor" "\\color{lightgray}")
          ))
  (setq org-export-latex-listings 'minted)
  (setq org-export-latex-custom-lang-environments
        '(
          (emacs-lisp "common-lispcode")
          ))
  (setq org-export-latex-minted-options
        '(("frame" "lines")
          ("fontsize" "\\scriptsize")
          ("linenos" "")
          ))

  ;; ;; ;; (setq org-latex-pdf-process '("latexmk -pdflatex=xelatex -pdf -bibtex -quiet -latexoption=-shell-escape %f"))
  ;; ;; ;; (setq org-latex-pdf-process '("latexmk -xelatex -bibtex -diagnostics -f -latexoption=-shell-escape %f"))
  ;; ;; ;; (setq org-latex-pdf-process '("latexmk -xelatex -g -bibtex -latexoption=-shell-escape -f %f"))
  ;; ;; ;; (setq org-latex-pdf-process '("latexmk -g -xelatex -bibtex -shell-escape %f"))
  ;; ;; ;; (setq org-latex-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f"))
  ;; ;; (setq org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode %f"
  ;; ;;                               "bibtex %b"
  ;; ;;                               "xelatex -shell-escape -interaction nonstopmode %f"
  ;; ;;                               "xelatex -shell-escape -interaction nonstopmode %f"))
  ;; ;; ;; (setq org-latex-pdf-process '("latexmk -xelatex -g -gg -bibtex -latexoption=-shell-escape -f %f"))
  ;; ;; ;; (setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
  
  ;; (setq org-latex-pdf-process
  ;;       '("pdflatex -interaction nonstopmode -output-directory %o -shell-escape %f"
  ;;         "bibtex %b"
  ;;         "pdflatex -interaction nonstopmode -output-directory %o -shell-escape %f"
  ;;         "pdflatex -interaction nonstopmode -output-directory %o -shell-escape %f"))

  (setq bibtex-dialect 'biblatex)
  
  (setq  org-latex-pdf-process
         '("latexmk -shell-escape -bibtex -pdf %f"))

  ;; (setq org-latex-to-pdf-process
  ;;       '("texi2dvi -p -b -c -V %f"))

  (setq org-latex-image-default-width nil)
  (setq org-image-actual-width nil)

  ;;Specify default packages to be included in every tex file, whether pdflatex or xelatex
  (setq org-latex-packages-alist
        '(("" "graphicx" t)
          ("" "listings" nil)
          ("" "longtable" nil)
          ("" "color" nil)
          ("" "float" nil)))


  (defun org-export-latex-no-toc (depth)
    (when depth
      (format "%% Org-mode is exporting headings to %s levels.\n"
              depth)))
  (setq org-export-latex-format-toc-function 'org-export-latex-no-toc)


  (add-to-list 'org-latex-classes
               '("dpom-spec"
                 "\\documentclass[12pt,a4paper]{article}
      [NO-DEFAULT-PACKAGES]
      \\usepackage[T1]{fontenc}
      \\usepackage[utf8]{inputenc}
      \\usepackage[AUTO]{babel}
      \\usepackage{minted}
      \\usepackage{tgtermes}
      \\usepackage[scale=.85]{tgheros}
      \\usepackage{tgcursor}
      \\usemintedstyle{emacs}
      \\newminted{common-lisp}{fontsize=10}
      \\usepackage[hmargin=2cm,top=4cm,headheight=65pt,footskip=65pt]{geometry}
      \\usepackage{fancyhdr}
      \\usepackage{lastpage}
      \\usepackage{xcolor}
      \\usepackage{array}
      \\usepackage[parfill]{parskip}
      \\usepackage{titlesec}
      \\usepackage{hyperref}
      \\pagestyle{fancy}
      \\fancyhead{}
      \\fancyfoot{}
      \\renewcommand{\\headrulewidth}{0pt}
      \\renewcommand{\\footrulewidth}{0pt}
      \\addtolength{\\headsep}{5pt}
      \\setlength{\\parindent}{0pt}
      \\setlength{\\headsep}{50pt}

      \\setcounter{secnumdepth}{4}
\\titleformat{\\paragraph}
{\\normalfont\\normalsize\\bfseries}{\\theparagraph}{1em}{}
\\titlespacing*{\\paragraph}
{0pt}{3.25ex plus 1ex minus .2ex}{1.5ex plus .2ex}


       \\rfoot{\\thepage/\\pageref{LastPage}}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  
  (add-to-list 'org-latex-classes
               '("letter"
                 "\\documentclass[12pt]{letter}
      [NO-DEFAULT-PACKAGES]
      \\usepackage[english,romanian]{babel}
      \\usepackage{ucs}
      \\usepackage[utf8x]{inputenc}
      \\usepackage[T1]{fontenc}
      \\usepackage{hyperref}
      \\usepackage{helvetica}
      % \\oddsidemargin=.2in
      % \\evensidemargin=.2in
      % \\textwidth=5.9in
      % \\topmargin=-.5in
      % \\textheight=9in
      \\pagestyle{empty}
      \\signature{{{{author}}}}
      \\address{{{{address}}}}
      "
                 ("\\section{%s}" . "\\section{%s}")
                 ("\\subsection{%s}" . "\\subsection{%s}")
                 ))

  (add-to-list 'org-latex-classes
               '("dpom-org-article"
                 "\\documentclass[12pt,a4paper]{article}
      [NO-DEFAULT-PACKAGES]
      \\usepackage[hmargin=2cm,top=4cm,headheight=65pt,footskip=65pt]{geometry}
      \\usepackage{fancyhdr}
      \\usepackage{lastpage}
      \\usepackage{xcolor}
      \\usepackage[parfill]{parskip}
      \\usepackage{hyperref}
      \\usepackage[romanian]{babel}
      \\defaultfontfeatures{Mapping=tex-text}
      \\pagestyle{empty}
            "
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("plain"
                 "\\documentclass[12pt,a4paper]{article}
      [NO-DEFAULT-PACKAGES]
      \\usepackage{xcolor}
      \\usepackage{hyperref}
      \\usepackage[romanian]{babel}
      \\usepackage[parfill]{parskip}
            "
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (defun org-export-latex-remove-references-heading (contents backend info)
    (if (not (eq backend 'latex))
        contents
      (replace-regexp-in-string "\\\\section\\*?{References}\\s-*\\\\label{.*?}" "" contents)
      ))

  (add-hook 'org-export-filter-final-output-functions 'org-export-latex-remove-references-heading)

)


;;; packages.el ends here
