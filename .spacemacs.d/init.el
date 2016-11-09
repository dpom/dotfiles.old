;; -*- mode: dotspacemacs -*-
(defun dotspacemacs/layers ()
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path (list (expand-file-name "layers/" dotspacemacs-directory))
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers '((auto-completion :variables
                                                        auto-completion-enable-snippets-in-popup t
                                                        auto-completion-return-key-behavior nil
                                                        auto-completion-tab-key-behavior 'complete
                                                        auto-completion-complete-with-key-sequence "ff"
                                                        auto-completion-complete-with-key-sequence-delay 0.5
                                                        auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/"
                                                        auto-completion-enable-company-help-tooltip 'manual
                                                        auto-completion-enable-sort-by-usage t
                                                        company-show-numbers t)
                                       bibtex
                                       (c-c++ :variables
                                              c-c++-enable-clang-support t
                                              c-default-style "linux"
                                              c-basic-offset 2)
                                       ;; common-lisp
                                       emacs-lisp
                                       ;; (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
                                       (erc :variables
                                            erc-server-list
                                            '(("irc.freenode.net" 
                                               :port "6697"
                                               :ssl t
                                               :nick "dpom")
                                              ))
                                       (git :variables
                                            git-enable-github-support t
                                            git-gutter-use-fringe t)
                                       (github :variables gh-profile-default-profile "dpom")
                                       gtags
                                       html
                                       java
                                       javascript
                                       latex
                                       markdown
                                       (org :variables
                                            org-enable-github-support t
                                            org-enable-reveal-js-support t)
                                       ;; (ranger :variables ranger-override-dired t)
                                       ;; (rcirc :variables
                                       ;;        rcirc-enable-authinfo-support t
                                       ;;        rcirc-server-alist
                                       ;;        '(("irc.freenode.net"
                                       ;;           :user "dpom"
                                       ;;           :channels ("#emacs #yetibot")))
                                       ;;        )
                                       semantic
                                       (shell :variables
                                              shell-default-shell 'eshell
                                              shell-default-height 30
                                              shell-default-position 'bottom)
                                       (shell-scripts :variables
                                                      sh-indentation 2
                                                      sh-basic-offset 2)
                                       smex
                                       (spell-checking :variables spell-checking-enable-by-default nil)
                                       sql
                                       (syntax-checking :variables syntax-checking-enable-by-default nil)
                                       (version-control :variables version-control-diff-tool 'diff-hl)
                                       ;; specific
                                       ;; clojure
                                       dpom
                                       ent
                                       clj
                                       multiedit
                                       ;; osx  ; for mac
                                       )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))
;; spacemacs-layers ends here

(defun dotspacemacs/init ()
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner nil
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         leuven
                         monokai)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.2)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
    ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil)
  )

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
  It is called immediately after `dotspacemacs/init'.  You are free to put any
  user code."
  (setq-default evil-escape-delay 0.5)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (defun expand-dir-name (dir path)
    "Expand a local dir NAME using his PATH."
    (file-name-as-directory (expand-file-name dir path)))
  )

(defun dotspacemacs/user-config ()
  (setq projectile-tags-command "ctags --exclude=log --exclude=doc --exclude=target --exclude=tmp -Re -f \"%s\" %s")
  (defun ao/expand-completion-table (orig-fun &rest args)
    "Extract all symbols from COMPLETION-TABLE before calling projectile--tags."
    (let ((completion-table (all-completions "" (car args))))
      (funcall orig-fun completion-table)))
  (advice-add 'projectile--tags :around #'ao/expand-completion-table)


  (global-company-mode)
  (define-key company-active-map [tab] 'company-complete-common)
                                        ;(global-set-key (kbd "TAB") 'hippie-expand)
  (setq spacemacs-mode-line-org-clock-current-taskp t)
  (setq dired-dwim-target t)
  (auto-fill-mode)
  (toggle-truncate-lines)
  (menu-bar-mode 1)
  (spacemacs/toggle-mode-line-org-clock)
  (spacemacs|defvar-company-backends sh-mode)
  (spacemacs|add-company-hook sh-mode)
  (setq eclim-eclipse-dirs "~/eclipse"
        eclim-executable "~/eclipse/eclim")
  (load (expand-file-name ".config.el" "~/pers/.private/"))
  ;;(setq yas-snippet-dirs '("")
  (eval-after-load 'yasnippet '(yas-reload-all))

  ;; Add yasnippet support for all company backends
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-offset 'case-label '+)))

  (setq org-ref-default-bibliography '("~/pers/bibliography/dpom.bib")
        org-ref-pdf-directory "~/pers/bibliography/pdfs/"
        org-ref-bibliography-notes "~/pers/bibliography/notes.org")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
;; spacemacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-date-style (quote european))
 '(safe-local-variable-values (quote ((ggtags-process-environment)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there îs more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t)))))
