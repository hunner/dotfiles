(mapcar (lambda (x) (add-to-list 'load-path (expand-file-name x)))
        '("~/.emacs.d"
          "/usr/share/emacs/site-lisp/clojure-mode"
          "/usr/share/emacs/site-lisp/slime"
          "/usr/share/emacs/site-lisp/swank-clojure"))

(defun require-all (packages)
    (mapcar #'require packages))

(require-all '(
               linum
               ido
               color-theme
               gentooish
               parenface
               bar-cursor
               ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL
(color-theme-initialize)

(if window-system
    (color-theme-gentooish)
    (color-theme-dark-laptop))

(bar-cursor-mode 1)

(tool-bar-mode 0)
(menu-bar-mode 0)
(global-linum-mode)
(setq linum-format "%3d ")
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(winner-mode t)

(tooltip-mode nil)
(setq line-number-mode nil)
(setq column-number-mode nil)
(setq size-indication-mode nil)
(setq mode-line-position nil)
(ido-mode 1)

(global-set-key "\C-m" 'reindent-then-newline-and-indent)  ;No tabs
(global-set-key "\C-a" 'beginning-of-line-text)

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
  point."
  (interactive "*P")
  (if (and
        (or (bobp) (= ?w (char-syntax (char-before))))
        (or (eobp) (not (= ?w (char-syntax (char-after))))))
    (dabbrev-expand arg)
    (indent-according-to-mode)))
(global-set-key [C-tab] 'indent-according-to-mode)

;; Prevent Emacs from stupidly auto-changing my working directory
(defun find-file-save-default-directory ()
    (interactive)
    (setq saved-default-directory default-directory)
    (ido-find-file)
    (setq default-directory saved-default-directory))
(global-set-key "\C-x\C-f" 'find-file-save-default-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell mode
;;
(load "~/.emacs.d/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic Lisp / Emacs Lisp
;; from http://www.emacswiki.org/emacs/AutoIndentation

(defadvice yank (after indent-region activate)
  (if (member major-mode '(clojure-mode emacs-lisp-mode lisp-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil)))) 

(defun tab-fix ()
  (local-set-key [tab] 'indent-or-expand))
(defun slime-tab-fix ()
  (local-set-key [tab] 'slime-complete-symbol))
(add-hook 'emacs-lisp-mode-hook 'tab-fix)
(add-hook 'lisp-mode-hook       'slime-tab-fix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure / SLIME

(require 'swank-clojure-autoload)
(setq swank-clojure-binary "~/local/bin/clj-server")

(require-all '(
               slime
               clojure-mode
               ))

;(setq slime-net-coding-system 'utf-8-unix)

(setq auto-mode-alist
      (cons '("\\.clj$" . clojure-mode)
            auto-mode-alist))

;(set-language-environment "UTF-8")
;(setq slime-net-coding-system 'utf-8-unix)
;;(slime-setup '(slime-fancy))
(slime-setup)
(define-key clojure-mode-map (kbd "<tab>") 'indent-or-expand)
(add-hook 'slime-connected-hook 'slime-redirect-inferior-output)

(defun lisp-enable-paredit-hook () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)

(defmacro defclojureface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

(defclojureface clojure-parens       "DimGrey"   "Clojure parens")
(defclojureface clojure-braces       "#49b2c7"   "Clojure braces")
(defclojureface clojure-brackets     "SteelBlue" "Clojure brackets")
(defclojureface clojure-keyword      "khaki"     "Clojure keywords")
(defclojureface clojure-namespace    "#c476f1"   "Clojure namespace")
(defclojureface clojure-java-call    "#4bcf68"   "Clojure Java calls")
(defclojureface clojure-special      "#b8bb00"   "Clojure special")
(defclojureface clojure-double-quote "#b8bb00"   "Clojure special" (:background "unspecified"))

(defun tweak-clojure-syntax ()
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-brackets))
            (("\\[\\|\\]"          . 'clojure-braces))
            ((":\\w+"              . 'clojure-keyword))
            (("#?\""               0 'clojure-double-quote prepend))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1 'clojure-java-call))
            )))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)

;;(add-to-list 'slime-lisp-implementations '(sbcl ("/usr/bin/sbcl")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(case-fold-search t)
 '(comint-scroll-to-bottom-on-input t)
 '(fancy-splash-image "")
 '(global-linum-mode t)
 '(ido-decorations (quote ("" "" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
 '(ido-everywhere t)
 '(inhibit-startup-screen t)
 '(line-number-mode nil)
 '(lisp-loop-forms-indentation 6)
 '(lisp-loop-keyword-indentation 6)
 '(lisp-simple-loop-indentation 6)
 '(mode-line-format (quote ("%e--[" mode-line-buffer-identification "]" (vc-mode vc-mode) "  " mode-line-modes global-mode-string " %-")))
 '(mode-line-in-non-selected-windows t)
 '(mode-line-modes (quote ("%[" "(" (:propertize ("" mode-name)) ("" mode-line-process) (:propertize ("" minor-mode-alist)) "%n" ")" "%]")))
 '(require-final-newline t)
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 100000)
 '(scroll-down-aggressively 0.0)
 '(scroll-margin 0)
 '(scroll-step 1)
 '(scroll-up-aggressively 0.0)
 '(show-paren-mode t nil (paren))
 '(slime-compilation-finished-hook nil)
 '(swank-clojure-extra-classpaths (quote ("/usr/share/emacs/site-lisp/swank-clojure/src/main/clojure"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#171717" :foreground "#c0c0c0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "bitstream" :family "Bitstream Vera Sans Mono")))))
