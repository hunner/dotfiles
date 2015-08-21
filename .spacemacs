;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     emacs-lisp
     git
     markdown
     org
     osx
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     syntax-checking
     version-control
     puppet
     themes-megapack
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

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         smyx
                         subatomic256
                         inkpot
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   ;dotspacemacs-default-font '("Source Code Pro"
   ;                            :size 13
   ;                            :weight normal
   ;                            :width normal
   ;                            :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   ;dotspacemacs-fullscreen-use-non-native nil
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
   ;; emphasis the current one).
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  (setq-default
   ns-use-native-fullscreen nil
  ;;   ;; If non nil pressing 'jk' in insert state, ido or helm will activate the
  ;;   ;; evil leader.
  ;;   dotspacemacs-feature-toggle-leader-on-jk nil
  ;;   ;; Paradox github integration
  ;;   paradox-github-token "..."
  ;;   ;; Always show trailing whitespace
  ;;   show-trailing-whitespace t
  ;;   ;; Fix paren/quote matching
  ;;   sp-cancel-autoskip-on-backward-movement nil

  ;; Custom font
  dotspacemacs-default-font '("Droid Sans Mono Dotted for Powerline"
                              :size 13
                              :weight normal
                              :width normal
                              :powerline-scale 1.1)
  )
  ;; Set to the location of your Org files on your local system
  ;;  (setq org-directory "~/org")
  ;;  (setq org-mobile-inbox-for-pull "~/org/inbox.org")
  ;;  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  ;;  (setq org-mobile-files '("~/org"))

  ;;  ;; Stuff for eshell
  ;;  (require 'em-smart)
  ;;  (setq eshell-where-to-jump 'begin)
  ;;  (setq eshell-review-quick-commands nil)
  ;;  (setq eshell-smart-space-goes-to-end t)
  ;;  (setq eshell-scroll-to-bottom-on-input t)
  ;; Configure colors for the powerline
  ;(setq ns-use-srgb-colorspace t)
  ;; undo-tree history across restarts
  (setq undo-tree-history-directory-alist
   `((".*" . ,(concat spacemacs-cache-directory "undo-tree-history"))))
  (setq undo-tree-auto-save-history t)

  ;; Don't use the clipboard please
  (setq select-enable-clipboard nil)
  ;; Don't show the menu bar on the terminal
  (setq menu-bar-mode nil)
  ;;  ;; Bind to the correct mac keys
  ;;  ;(setq mac-option-modifier 'meta)
  ;;  ;(setq mac-command-modifier 'super)
  ;;  ;(setq mac-pass-control-to-system nil)
  ;;  ;(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
  ;;  ;(global-set-key (kbd "s-v") 'yank)
  ;;  ;(global-set-key (kbd "s-c") 'copy-region-as-kill)

  ;;  ;; Set transparency of emacs
  ;;  (defun transparency (value)
  ;;   "Sets the transparency of the frame window. 0=transparent/100=opaque"
  ;;   (interactive "nTransparency Value 0 - 100 opaque:")
  ;;   (set-frame-parameter (selected-frame) 'alpha value))

  )

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  ;; Make C-g work like <esc>
  ;(define-key evil-normal-state-map "\C-g" 'evil-normal-state)
  ;(define-key evil-visual-state-map "\C-g" 'evil-normal-state)
  ;(define-key evil-insert-state-map "\C-g" 'evil-normal-state)
  (define-key evil-visual-state-map "\C-c" 'evil-normal-state)
  (define-key evil-insert-state-map "\C-c" 'evil-normal-state)
  (define-key evil-normal-state-map "\C-h" 'evil-window-left)
  (define-key evil-normal-state-map "\C-j" 'evil-window-down)
  (define-key evil-normal-state-map "\C-k" 'evil-window-up)
  (define-key evil-normal-state-map "\C-l" 'evil-window-right)
  ;(define-key global-map "\\" nil)
  ;(define-key global-map "\\f" 'ido-find-file)
  ;(define-key global-map "\\v" 'helm-projectile-find-file)
  ;(define-key global-map "\\c" 'helm-projectile-switch-project)
  ;(define-key global-map "\\b" 'helm-mini)
  (setq powerline-default-separator nil)
  ;;(smartparens-global-mode f)
  ;; Relative line numbers by default
  ;(linum-relative-toggle)
  ;(global-linum-mode t)
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (pbcopy launchctl zonokai-theme zenburn-theme zen-and-art-theme window-numbering volatile-highlights vi-tilde-fringe underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spray spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle shell-pop seti-theme rfringe reverse-theme rainbow-delimiters purple-haze-theme puppetfile-mode puppet-mode professional-theme powerline popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el pastels-on-dark-theme paradox page-break-lines organic-green-theme org-repo-todo org-present org-pomodoro org-bullets open-junk-file oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc magit-gitflow macrostep lush-theme linum-relative light-soap-theme leuven-theme jazz-theme ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete htmlize highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-unicode helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flyspell helm-descbinds helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md gandalf-theme fringe-helper flycheck-pos-tip flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-terminal-cursor-changer evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu espresso-theme eshell-prompt-extras esh-help elisp-slime-nav django-theme diff-hl define-word darktooth-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-statistics company-quickhelp colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme buffer-move bubbleberry-theme birds-of-paradise-plus-theme auto-yasnippet auto-highlight-symbol auto-dictionary apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-mode ac-ispell auto-complete avy names pos-tip company anzu iedit smartparens highlight flx flycheck popup yasnippet request gitignore-mode projectile helm helm-core async parent-mode magit magit-popup git-commit with-editor markdown-mode alert log4e gntp spinner pkg-info epl evil-leader evil which-key quelpa package-build use-package bind-key s dash spacemacs-theme)))
 '(ring-bell-function (quote ignore)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#1c1c1c" :foreground "#d7d7d7"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
