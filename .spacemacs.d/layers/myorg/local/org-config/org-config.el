(require 'org)
(require 'org-contacts)
(require 'org-bullets)
(require 'bibtex)

(provide 'org-config)

;;; Basic settings
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-modules (quote (org-docview org-info org-mac-message org-protocol
                                      org-w3m  org-mac-link-grabber org-man org2rem
                                      org-timer org-bibtex org-docview org-info))
      org-agenda-ndays 7
      org-agenda-repeating-timestamp-show-all nil
      org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
      org-agenda-restore-windows-after-quit t
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-sorting-strategy (quote ((agenda time-up priority-down tag-up) (todo tag-up)))
      org-agenda-start-on-weekday nil
      org-reverse-note-order t
      org-agenda-window-setup 'other-window
      org-deadline-warning-days 14
      org-timeline-show-empty-dates t
      org-insert-mode-line-in-empty-file t
      org-log-into-drawer t
      org-cycle-emulate-tab t
      org-outline-path-complete-in-steps nil)

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 5))))

(setq org-bullets-bullet-list '("○" "◙" "✸" "▲"))


;;; Custom Commands
(setq org-agenda-custom-commands
      '(
        ("c" todo "DONE|DEFERRED|CANCELLED" nil)
        ("w" todo "WAITING" nil)
        ("u" "Unscheduled" alltodo ""
         ((org-agenda-todo-ignore-scheduled t)))

        ("W" agenda "" ((org-agenda-ndays 21)))

        ("A" agenda ""
         ((org-agenda-skip-function
           (lambda nil
             (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
          (org-agenda-ndays 1)
          (org-agenda-overriding-header "Today's Priority #A tasks: ")))

        ("p" "Projects"
         ((tags "PROJECT" ((org-tags-match-list-sublevels nil)
                           (org-use-tag-inheritance t)))))

        ("H" "Office and Home Lists"
         ((agenda)
          (tags-todo "WORK")
          (tags-todo "HOME")
          (tags-todo "COMPUTER")
          (tags-todo "DVD")
          (tags-todo "READING")))

        ("d" "Daily Action List"
         (
          (agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy (quote ((agenda time-up priority-down tag-up))))
                      (org-deadline-warning-days 0)
                      ))))
        )
      )

;; keywords
(setq org-todo-keywords (quote
                         ((sequence "TODO(t)" "STARTED(s)" "APPT(a)" "|" "DONE(d!/!)")
                          (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)"))))
(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "red"          :weight bold)
              ("STARTED"      :foreground "deep sky blue"         :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("WAITING"   :foreground "yellow"       :weight bold)
              ("APPT"   :foreground "goldenrod"    :weight bold)
              ("CANCELLED" :foreground "orangered"    :weight bold))))



;;; Bindings and Hooks

(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'org-toggle-blocks)

(spacemacs/set-leader-keys "aof" 'org-open-at-point-global)

(define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)

(evil-define-key '(normal visual motion) org-mode-map
  "gh" 'outline-up-heading
  "gj" 'outline-forward-same-level
  "gk" 'outline-backward-same-level
  "gl" 'outline-next-visible-heading
  "gu" 'outline-previous-visible-heading)

;; Quick refile of project tasks
;; (setq org-refile-targets '((nil :regexp . "Week of")))

(spacemacs/set-leader-keys-for-major-mode 'org-mode "r" 'org-refile)

;;; Theming

(setq org-priority-faces '((65 :inherit org-priority :foreground "red")
                           (66 :inherit org-priority :foreground "brown")
                           (67 :inherit org-priority :foreground "blue")))

(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                             :weight normal
                             :strike-through t))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon" :strike-through t)))))


;;; Templates

(setq org-capture-templates
      '(("t" "todo" entry
         (file+headline dpom/org-notes-file "Tasks")
         "* TODO %?\n  %u\n%a\n\n\n" :clock-resume t :prepend t)
        ("n" "note" entry
         (file+headline dpom/org-notes-file "Notes")
         "* %u %?\n%a\n\n\n" :clock-resume t :prepend t)))

(setq
 org-structure-template-alist
 '(("a" . "export ascii")
   ("c" . "src clojure")
   ("C" . "comment")
   ("d" . "src dot")
   ("e" . "src elisp")
   ("E" . "example")
   ("h" . "export html")
   ("j" . "src js")
   ("l" . "export latex")
   ("n" . "notes")
   ("q" . "quote")
   ("s" . "src shell")
   ("v" . "verse")
   ("x" . "src xml")))
(require 'org-tempo)

;;; Org Blocks

;; Hide all org-blocks, including src, quote, etc. blocks, on buffer load
(defvar org-blocks-hidden nil)
(defun org-toggle-blocks ()
  (interactive)
  (if org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq-local org-blocks-hidden (not org-blocks-hidden)))

;;; Clocking
(setq org-clock-persistence-insinuate t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
(setq org-clock-history-length 28)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change task state to NEXT when clocking in
(setq org-clock-in-switch-to-state (quote dpom/clock-in-to-started))
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;;; Reminders

;; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'dpom/org-agenda-to-appt 'append)

(dpom/org-agenda-to-appt)

;; Activate appointments so we get notifications
(appt-activate t)

;; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "00:01" nil 'dpom/org-agenda-to-appt)

(setq appt-display-format 'window)
(setq appt-disp-window-function (function dpom/appt-disp-window))

(defun dpom/appt-disp-window (min-to-app new-time msg)
  (save-window-excursion
    (shell-command
     (concat "~/bin/org-appt-notify '" msg "'  '" min-to-app "'&")
     nil nil)
    )
  )


(setq org-alphabetical-lists t)

;;; Crypt

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)
                                        ;  (require 'org-mime)
                                        ;  (add-hook 'message-mode-hook
                                        ;            (lambda ()
                                        ;              (local-set-key "\C-c\M-o" 'org-mime-htmlize)))


;;; Export

;; Explicitly load required exporters
(require 'ox-html)
(require 'ox-latex)
(require 'ox-ascii)
(require 'ox-md)
(require 'ox-bibtex)
(require 'ox-extra)
(require 'ox-confluence)

(setq org-table-export-default-format 'orgtbl-to-csv
      org-md-headline-style 'atx)

(ox-extras-activate '(ignore-headlines))

;; (when linux?
;;   (setq org-file-apps '((auto-mode . emacs)
;;                         ("\\.mm\\'" . default)
;;                         ("\\.x?html?\\'" . "/usr/bin/firefox %s")
;;                         ("\\.pdf\\'" . default))))

;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;; (setq org-latex-listings 'minted)
;; (setq org-latex-minted-options '(("frame" "lines")
;;                                  ("fontsize" "\\scriptsize")
;;                                  ("xleftmargin" "\\parindent")
;;                                  ("linenos" "")))
;; (setq
;;  org-latex-pdf-process
;;  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; (setq org-latex-pdf-process
;;       '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))

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


;;; Babel

(require 'ob-clojure)
(require 'ob-org)
(require 'ob-shell)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   ;; (asm . t)
   (clojure . t)
   (css . t)
   (ditaa . t)
   (dot . t)
   ;; (dtd . t)
   (js . t)
   ;; (make . t)
   ;; (perl . t)
   (plantuml . t)
   (python . t)
   (org . t)
   ;; (rb . t)
   (sql . t)
   ;; (xml . t)
   (shell . t)
   ))

(setq org-confirm-babel-evaluate nil)
(setq org-src-window-setup 'current-window)

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
(add-to-list 'org-src-lang-modes '("make" . makefile))
;; (add-to-list 'org-src-lang-modes '("rb" . vbnet))
(add-to-list 'org-src-lang-modes '("plantuml" . text))
(setq org-plantuml-jar-path (expand-file-name "plantuml.jar" (expand-dir-name "scripts" dpom/myorg-layer))

      org-ditaa-jar-path (expand-file-name "ditaa.jar" (expand-dir-name "scripts" dpom/myorg-layer)))

(setq plantuml-jar-path org-plantuml-jar-path)
(setq org-id-link-to-org-use-id nil)

(require 'org-id)
(setq org-confirm-babel-evaluate nil)
(setq org-id-link-to-org-use-id nil)

;; The following displays the contents of code blocks in Org-mode files
;; using the major-mode of the code.  It also changes the behavior of
;; =TAB= to as if it were used in the appropriate major mode.  This means
;; that reading and editing code form inside of your Org-mode files is
;; much more like reading and editing of code using its major mode.
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)
(setq org-src-tab-acts-natively t)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
;; Now you just create a =begin-src= block for the appropriate tool, edit
;; the text, and build the pictures with =C-c C-c=.  After evaluating the
;; block results are displayed.  You can toggle display of inline images
;; with =C-c C-x C-v=


;; In order to generate sql statements from org table you must enable:
;; (require 'org-table)
;; (require 'orgtbl-sqlinsert)

(org-babel-lob-ingest (expand-file-name "library-of-babel.org" dpom/myorg-layer))
