(mapcar (lambda (x) (add-to-list 'load-path (expand-file-name x)))
        '("~/.emacs.d"
          ))

(defun require-all (packages)
    (mapcar #'require packages))

(require-all '(
               linum
               ido
               color-theme
               gentooish
               irblack
               parenface
               bar-cursor
               ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL
(color-theme-initialize)

;(if window-system
;    (color-theme-gentooish)
;    (color-theme-dark-laptop))
(color-theme-irblack)

(bar-cursor-mode 1)
(menu-bar-mode 0)
(global-linum-mode)
(setq linum-format "%3d ")
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq make-backup-files nil)
(set-language-environment "UTF-8")
(winner-mode t)

(tooltip-mode nil)
(setq line-number-mode nil)
(setq column-number-mode nil)
(setq size-indication-mode nil)
(setq mode-line-position nil)
(ido-mode t)

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

;; Proxy for ssh tunnel + privoxy
(setq url-proxy-services '(("no_proxy" . "localhost")
                           ("http" . "localhost:8118")))

(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; get the package

;; Make % work like vi
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Prevent Emacs from stupidly auto-changing my working directory
(defun find-file-save-default-directory ()
    (interactive)
    (setq saved-default-directory default-directory)
    (ido-find-file)
    (setq default-directory saved-default-directory))
(global-set-key "\C-x\C-f" 'find-file-save-default-directory)

;; Give killing lines advice
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

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

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;(define-key clojure-mode-map (kbd "<tab>") 'indent-or-expand)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translation
(autoload 'babel "babel"
  "Use a web translation service to translate the message MSG." t)
(autoload 'babel-region "babel"
  "Use a web translation service to translate the current region." t)
(autoload 'babel-as-string "babel"
  "Use a web translation service to translate MSG, returning a string." t)
(autoload 'babel-buffer "babel"
  "Use a web translation service to translate the current buffer." t)

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
 '(scroll-conservatively 100000)
 '(scroll-down-aggressively 0.0)
 '(scroll-margin 4)
 '(scroll-step 1)
 '(scroll-up-aggressively 0.0)
 '(show-paren-mode t nil (paren))
 )


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
