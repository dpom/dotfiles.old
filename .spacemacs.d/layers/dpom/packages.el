;;; packages.el --- dpom Layer packages File for Spacemacs
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

(setq dpom-packages
      '(org
        org-ref
        (org-redmine :location local)
        (mu4e :location local)
        cdlatex
        (ox-reveal :location (recipe :fetcher github :repo "yjwen/org-reveal"))
        (ox-rst :location (recipe :fetcher github :repo "masayuko/ox-rst"))
        (conda :location (recipe :fetcher github :repo "necaris/conda.el"))
        (python-django :location (recipe :fetcher github :repo "fgallina/python-django.el"))
        (org-jira)
        ))


(defun dpom/post-init-org ()
  ;; org-basic-settings
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
  (setq org-bullets-bullet-list '("○" "◙" "▲" "▶"))

  ;; org-custom-commands
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

  (setq org-fontify-done-headline t)
  (custom-set-faces
   '(org-done ((t (:foreground "PaleGreen"
                               :weight normal
                               :strike-through t))))
   '(org-headline-done
     ((((class color) (min-colors 16) (background dark))
       (:foreground "LightSalmon" :strike-through t)))))

  ;; Capture
  (setq org-capture-templates
        '(("t" "todo" entry
           (file+headline dpom/org-notes-file "Tasks")
           "* TODO %?\n  %u\n%a\n\n\n" :clock-resume t :prepend t)
          ("n" "note" entry
           (file+headline dpom/org-notes-file "Notes")
           "* %u %?\n%a\n\n\n" :clock-resume t :prepend t)))

  ;; Clocking
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

  ;; Reminders
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


  ;; Code blocks (babel)
  ;; (require 'ob-xml)
  ;; (add-to-list 'org-src-lang-modes '("xml" . nxml))

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
     ;; (rb . t)
     (sql . t)
     ;; (xml . t)
     (sh . t)
     ))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (add-to-list 'org-src-lang-modes '("make" . makefile))
  ;; (add-to-list 'org-src-lang-modes '("rb" . vbnet))
  (add-to-list 'org-src-lang-modes '("plantuml" . text))
  (setq org-plantuml-jar-path (expand-file-name "plantuml.jar" (expand-dir-name "scripts" dpom/dpom-layer))

        org-ditaa-jar-path (expand-file-name "ditaa.jar" (expand-dir-name "scripts" dpom/dpom-layer)))

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

  (org-babel-lob-ingest (expand-file-name "library-of-babel.org" dpom/dpom-layer))

  ;; Exports
  (setq org-alphabetical-lists t)

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
                                        ;
  )


(defun dpom/post-init-org-ref ()
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
              (spacemacs/set-leader-keys-for-major-mode 'org-mode
                "rr" 'org-ref-helm-insert-cite-link
                "rl" 'org-ref-helm-insert-label-link
                "rf" 'org-ref-helm-insert-ref-link
                )
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

(defun dpom/init-org-redmine ()
  "Initialize org-redmine extension"
  (use-package org-redmine
    :config (progn
              (setq org-redmine-template-header "TODO %t_n%%i% - %s% :%p_n%: \nSCHEDULED: <%s_date%>")
              (setq org-redmine-template-property '(( "assigned" . "%as_n%" )
                                                    ( "version" . "%v_n%" )
                                                    ( "project" . "%p_n%")))
              (defun dpom-helm-redmine (&optional me)
                "Display recent issues using `helm'"
                (interactive "P")
                (push-mark)
                (helm
                 `(((name . "Issues")
                    (candidates . ,(org-redmine-get-issue-all me))
                    (candidate-transformer . org-redmine-transformer-issues-source)
                    (volatile)
                    (action . (("Insert Subtree"
                                . (lambda (issue) (org-redmine-insert-subtree issue))))))))
                (while (re-search-forward "/" nil t)
                  (replace-match "-")))
              (spacemacs/set-leader-keys  "oR" 'dpom-helm-redmine))))

(defun dpom/init-mu4e ()
  "Initialize mu4e extension"
  (use-package mu4e
    :config (progn
              (setq mu4e-maildir "~/Maildir"
                    mu4e-drafts-folder "/[Gmail].Drafts"
                    mu4e-sent-folder   "/[Gmail].Sent Mail"
                    mu4e-trash-folder  "/[Gmail].Trash"
                    mu4e-sent-messages-behavior 'delete
                    mu4e-get-mail-command "offlineimap"
                    mu4e-attachment-dir  "~/Downloads"
                    mu4e-view-show-images t
                    mu4e-maildir-shortcuts
                    '( ("/INBOX"               . ?i)
                       ("/new.emacs"           . ?e)
                       ("/new.debian"          . ?d)
                       ("/new.clojure"         . ?c)
                       ("/new.realsoftware"    . ?r)
                       ("/new.arm"             . ?m)
                       ("/cogito.letec"        . ?l)
                       ("/[Gmail].Sent Mail"   . ?s)
                       ("/[Gmail].Trash"       . ?t)
                       )
                    )
              ;; use imagemagick, if available
              (when (fboundp 'imagemagick-register-types)
                (imagemagick-register-types))

              ;; message view action
              (defun mu4e-msgv-action-view-in-browser (msg)
                "View the body of the message in a web browser."
                (interactive)
                (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
                      (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
                  (unless html (error "No html part for this message"))
                  (with-temp-file tmpfile
                    (insert
                     "<html>"
                     "<head><meta http-equiv=\"content-type\""
                     "content=\"text/html;charset=UTF-8\">"
                     html))
                  (browse-url (concat "file://" tmpfile))))

              (add-to-list 'mu4e-view-actions
                           '("View in browser" . mu4e-msgv-action-view-in-browser) t)

              (require 'smtpmail)

              ;; org interaction
              (require 'org-contacts)
              (require 'org-mu4e)
              (require 'mu4e-actions)
              (setq mu4e-org-contacts-file  dpom/org-contacts-file)
              (add-to-list 'mu4e-headers-actions
                           '("org-contact-add" . mu4e-action-add-org-contact) t)
              (add-to-list 'mu4e-view-actions
                           '("org-contact-add" . mu4e-action-add-org-contact) t)
              (setq mu4e-compose-complete-only-personal t)
              (setq message-kill-buffer-on-exit t)
              (defalias 'org-mail 'org-mu4e-compose-org-mode))))

(defun dpom/init-cdlatex ()
  "Initialize cdlatex extension"
  (use-package cdlatex))


(defun dpom/post-init-ox-reveal ()
  "Initialize ox-reveal package."
  (use-package ox-reveal
    :config (progn
              (setq org-reveal-root ""))))

(defun dpom/init-ox-rst ()
  "Initialize ox-rst package."
  (use-package ox-rst))

(defun dpom/init-conda ()
  "Initialize conda package"
  (use-package conda
    :config (progn
              (setq conda-anaconda-home "~/anaconda3")
              ;; if you want interactive shell support, include:
              (conda-env-initialize-interactive-shells)
              ;; if you want eshell support, include:
              (conda-env-initialize-eshell)
              ;; if you want auto-activation (see below for details), include:
              (conda-env-autoactivate-mode t)
              ))
  )

(defun dpom/init-python-django ()
  "Initialize python-django package"
  (use-package python-django))


(defun dpom/init-org-jira ()
  "Initialize conda package"
  (use-package org-jira
    :config (progn
              (setq jiralib-url  "http://jira.emag.local:8080/secure/")
              ))
  )

;; dpom-package ends here
