;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-nord)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! puppet-mode
  :mode "\\.pp\\'")

(setq doom-leader-key ",")
(setq doom-leader-alt-key "M-,")
(setq doom-localleader-key ", m")
(setq doom-localleader-alt-key "M-, m")
(setq org-todo-keywords '((sequence "SWEEP(s)" "TODO(t)" "|" "DONE(d)")
                          (sequence "[ ](T)" "[-](p)" "[?](m)" "|" "[X](D)")
                          (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)")))

(map! :n   "SPC" #'evil-ex
      :gin "C-u" #'universal-argument
      :n   "C-h" #'evil-window-left
      :n   "C-j" #'evil-window-down
      :n   "C-k" #'evil-window-up
      :n   "C-l" #'evil-window-right
      :n   "-"   #'flycheck-next-error
      :n   "_"   #'flycheck-previous-error)


;; Eshell stuff
(setenv "SSH_AUTH_SOCK" (concat (getenv "HOME") "/.gnupg/S.gpg-agent.ssh"))

;(eshell/addpath "/opt/puppetlabs/pdk/bin")

;; Where to find rubocop
(setq flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")

(eval-after-load "flymake-json"
  '(defun flymake-json-command (filename)
     "Construct a command that flymake can use to check json source in FILENAME."
     (list "jsonlint" "-V" "/Users/hunner/.doom.d/draft-07.json" "-c" "-q" filename)))
