(require 'cl)

(mapcar (lambda (x) (add-to-list 'load-path (expand-file-name x)))
        '("~/.emacs.d"
          ))

(defun require-all (packages)
    (mapcar #'require packages))

(require-all '(
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

(if window-system
;    (color-theme-gentooish)
    (color-theme-dark-laptop))
(color-theme-irblack)

(bar-cursor-mode 1)
(menu-bar-mode 0)
(global-linum-mode)
(setq linum-format "%3d ")
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq make-backup-files nil)
(set-language-environment "UTF-8")
(set-input-method "japanese-ascii")
(winner-mode t)
(display-battery-mode t)
(setq display-time-24hr-format t)
(display-time-mode t)

(tooltip-mode nil)
(setq midnight-mode t)
(setq column-number-mode nil)
(setq size-indication-mode nil)
(setq mode-line-position nil)
(mouse-avoidance-mode 'animate)
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


;; Enable ergoemacs layout
;; (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "dv") ; US Dvorak layout
;; (load "~/.emacs.d/ergoemacs-keybindings-5.1/ergoemacs-mode")
;; (ergoemacs-mode 1)

;; Highlight bad whitespace
(global-whitespace-mode t)
(setq whitespace-style (quote (tabs tab-mark)))

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

;; Tip of the day!
(defun totd ()
  (interactive)
  (random t) ;; seed with time-of-day
  (with-output-to-temp-buffer "*Tip of the day*"
    (let* ((commands (loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip for the day is:\n"
               "========================\n\n"
               (describe-function command)
               "\n\nInvoke with:\n\n"
               (with-temp-buffer
                 (where-is command t)
                 (buffer-string)))))))

;; Set my location sunrise-sunset
;; (setq calendar-latitude 40.1)
;; (setq calendar-longitude -88.2)
;; (setq calendar-location-name "Portland, OR")
(setq calendar-latitude 17.5)
(setq calendar-longitude 78.5)
(setq calendar-location-name "Hyderabad, India")

;; Start the server for emacsclient
;(server-start)

;; Custom key maps
(defun set-keys (commands)
  (mapcar (lambda (x)
            (global-set-key (read-kbd-macro (first x)) (second x)))
          commands))
(set-keys '(
            ("C-c t" totd)
            ("C-c s p" (lambda () (interactive)
                         (if (shellfm-running-p)
                             (shellfm-pause)
                           ((shellfm 1) (shellfm-station-recommended 1)))))
            ("C-c s n" shellfm-skip-track)
            ("C-c s r" shellfm-station-recommended)
            ("C-c s s" shellfm-station-artist)
            ("C-c s m" shellfm-station-playlist)
            ("C-c s l" shellfm-love-track)
            ("C-c s a" shellfm-add-to-playlist)
            ("C-c s q" shellfm 0)
            ("C-c s i" shellfm-track-info)
            ("M-s" save-buffer)
            ("M-N" make-frame)
            ("M-W" delete-frame)
            ("M-w" ido-kill-buffer)
            ("M-1" delete-other-windows)
            ("M-!" delete-window)
            ("M-2" split-window-horizontally)
            ("M-@" split-window-vertically)
            ("C-c C-a" beginning-of-line)
            ("M-o" other-window)
            ("M-O" other-window)
            ("M-`" switch-to-next-frame)
            ("M-~" switch-to-previous-frame)
            ))

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (find 'alpha (frame-parameters nil) :key #'car))
       10)
      (set-frame-parameter nil 'alpha '(10 10))
    (set-frame-parameter nil 'alpha '(85 85))))
(global-set-key (kbd "C-c C-t") 'toggle-transparency)

;; (global-set-key (kbd "<XF86AudioPlay>")
;;                 (lambda () (interactive)
;;                   (if (shellfm-running-p)
;;                       (shellfm-skip-track)
;;                     (shellfm 1))))

;; ERC stuff
;; (setq erc-encoding-coding-alist (quote (("#lisp" . utf-8)
;;                                         ("#nihongo" . iso-2022-jp) ("#truelambda" . iso-latin-1)
;;                                         ("#bitlbee" . iso-latin-1))))

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
 '(lisp-loop-forms-indentation 6)
 '(lisp-loop-keyword-indentation 6)
 '(lisp-simple-loop-indentation 6)
 '(mode-line-format (quote ("%e--[" mode-line-buffer-identification "]" (vc-mode vc-mode) " " mode-line-modes " " global-mode-string " %-")))
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
  (package-initialize)
  (require 'starter-kit-elpa))
