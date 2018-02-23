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
        ;; (org-redmine :location local)
        ;; (org-jira)
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

  (require 'ob-clojure)
  (require 'ob-org)

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


(defun dpom/post-init-anaconda-mode ()
  (defun python-send-line ()
    (interactive)
    (move-beginning-of-line nil)
    (let ((beg (point)))
      (forward-line 1)
      (python-shell-send-region beg (point))))

  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "ss" 'python-start-or-switch-repl
    "sl" 'python-send-line)
)


;; dpom-package ends here
