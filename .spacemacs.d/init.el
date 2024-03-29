;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'nil
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
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
                                       ivy
                                       bibtex
                                       (c-c++ :variables
                                              c-c++-enable-clang-support t
                                              c-default-style "linux"
                                              c-basic-offset 2)
                                       ;; common-lisp
                                       emacs-lisp
                                       ;; (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
                                       ;(erc :variables
                                       ;     erc-server-list
                                        ;    '(("irc.freenode.net" 
                                         ;      :port "6697"
                                          ;     :ssl t
                                          ;     :nick "dpom")
                                          ;    ))
                                       (git :variables
                                            git-enable-github-support t
                                            git-gutter-use-fringe t)
                                       (github :variables gh-profile-default-profile "dpom")
                                       ;; gtags
                                       ;; cscope 
                                       html
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
                                       ;; semantic
                                       (shell :variables
                                              shell-default-shell 'eshell
                                              shell-default-height 30
                                              shell-default-position 'bottom)
                                       (shell-scripts :variables
                                                      sh-indentation 2
                                                      sh-basic-offset 2)
                                       ;; smex
                                       (spell-checking :variables spell-checking-enable-by-default nil)
                                       sql
                                       (syntax-checking :variables syntax-checking-enable-by-default nil)
                                       (version-control :variables version-control-diff-tool 'diff-hl)
                                       ;; specific
                                       clojure
                                       dpom
                                       ent
                                       ;; clj
                                       (python :variables
                                               python-test-runner 'pytest
                                               python-enable-yapf-format-on-save nil)
                                       myediting
                                       yaml
                                       docker
                                       plantuml
                                       octave
                                       ;; osx  ; for mac
                                       )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         leuven
                         monokai)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state t
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
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
  It is called immediately after `dotspacemacs/init'.  You are free to put any
  user code."
  (setq-default evil-escape-delay 0.5)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  ;; Load emacs packages and activate them
  ;; This must come before configurations of installed packages.
  ;; Don't delete this line.
  (package-initialize)
  ;; `package-initialize' call is required before any of the below
  ;; can happen

  (defun expand-dir-name (dir path)
    "Expand a local dir NAME using his PATH."
    (file-name-as-directory (expand-file-name dir path)))
  )

(defun dotspacemacs/user-config ()
  ;; (setq projectile-tags-command "ctags --exclude=log --exclude=doc --exclude=target --exclude=tmp -Re -f \"%s\" %s")
  ;; (require 'helm-bookmark)
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
  (spacemacs/toggle-truncate-lines-on)
  (spacemacs/toggle-auto-fill-mode-on)
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

  (setq calendar-date-style 'european)
  (spacemacs/set-leader-keys "SPC" 'avy-goto-char-timer)
  ;; (spacemacs/helm-gtags-define-keys-for-mode 'python-mode)


  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)

  (setq projectile-file-exists-local-cache-expire (* 5 60)
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action 'projectile-dired)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
;; spacemacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (wgrep ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper powerline pdf-tools key-chord ivy org-category-capture alert log4e gntp org-plus-contrib dash-functional parent-mode projectile request helm-bibtex parsebib gitignore-mode fringe-helper git-gutter+ git-gutter gh marshal logito pcache ht flx magit git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree magit-popup diminish company hydra highlight spinner pkg-info epl bind-map bind-key biblio biblio-core yasnippet packed anaconda-mode pythonic f dash s helm avy helm-core async auto-complete popup clojure-snippets clj-refactor inflections edn multiple-cursors paredit peg cider-eval-sexp-fu cider seq queue clojure-mode origami string-inflection org-jira helm-cscope xcscope helm-gtags ggtags web-mode tagedit sql-indent slim-mode scss-mode sass-mode ranger pug-mode plantuml-mode mmm-mode markdown-toc markdown-mode less-css-mode helm-css-scss haml-mode gh-md flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck emmet-mode dockerfile-mode docker json-mode tablist docker-tramp json-snatcher json-reformat company-web web-completion-data company-auctex auto-dictionary auctex-latexmk auctex zenburn-theme yapfify yaml-mode xterm-color ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline smex smeargle shell-pop restart-emacs rainbow-delimiters pyvenv python-django pytest pyenv-mode py-isort popwin pip-requirements persp-mode pcre2el paradox ox-rst ox-reveal ox-gfm orgit org-ref org-projectile org-present org-pomodoro org-download org-bullets open-junk-file neotree multi-term move-text magit-gitflow magit-gh-pulls macrostep lorem-ipsum live-py-mode linum-relative link-hint insert-shebang info+ inf-clojure indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist fuzzy flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-multiedit evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help elisp-slime-nav dumb-jump disaster diff-hl define-word cython-mode conda company-statistics company-shell company-c-headers company-anaconda column-enforce-mode cmake-mode clean-aindent-mode clang-format cdlatex auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "log" "doc" "tmp" "target")))
 '(safe-local-variable-values
   (quote
    ((python-shell-interpreter . "/home/dan/emag/emaproject/emashell")
     (python-shell-virtualenv-root . "/home/dan/anaconda3/envs/eMAG")
     (cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t)))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (symon sayid realgud test-simple loc-changes load-relative password-generator org-brain monokai-theme impatient-mode simple-httpd helm-purpose window-purpose imenu-list flycheck-bashate evil-org evil-lion editorconfig cmake-ide levenshtein browse-at-remote powerline pdf-tools key-chord ivy org-category-capture alert log4e gntp org-plus-contrib dash-functional parent-mode projectile request helm-bibtex parsebib gitignore-mode fringe-helper git-gutter+ git-gutter gh marshal logito pcache ht flx magit git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree magit-popup diminish company hydra highlight spinner pkg-info epl bind-map bind-key biblio biblio-core yasnippet packed anaconda-mode pythonic f dash s helm avy helm-core async auto-complete popup clojure-snippets clj-refactor inflections edn multiple-cursors paredit peg cider-eval-sexp-fu cider seq queue clojure-mode origami string-inflection org-jira helm-cscope xcscope helm-gtags ggtags web-mode tagedit sql-indent slim-mode scss-mode sass-mode ranger pug-mode plantuml-mode mmm-mode markdown-toc markdown-mode less-css-mode helm-css-scss haml-mode gh-md flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck emmet-mode dockerfile-mode docker json-mode tablist docker-tramp json-snatcher json-reformat company-web web-completion-data company-auctex auto-dictionary auctex-latexmk auctex zenburn-theme yapfify yaml-mode xterm-color ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline smex smeargle shell-pop restart-emacs rainbow-delimiters pyvenv python-django pytest pyenv-mode py-isort popwin pip-requirements persp-mode pcre2el paradox ox-rst ox-reveal ox-gfm orgit org-ref org-projectile org-present org-pomodoro org-download org-bullets open-junk-file neotree multi-term move-text magit-gitflow magit-gh-pulls macrostep lorem-ipsum live-py-mode linum-relative link-hint insert-shebang info+ inf-clojure indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist fuzzy flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-multiedit evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help elisp-slime-nav dumb-jump disaster diff-hl define-word cython-mode conda company-statistics company-shell company-c-headers company-anaconda column-enforce-mode cmake-mode clean-aindent-mode clang-format cdlatex auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "log" "doc" "tmp" "target")))
 '(safe-local-variable-values
   (quote
    ((python-shell-interpreter . "/home/dan/emag/emaproject/emashell")
     (python-shell-virtualenv-root . "/home/dan/miniconda3/envs/eMAG")
     (cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t)))))
)
