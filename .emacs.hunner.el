;; (require 'cl)

(mapc (lambda (x) (add-to-list 'load-path (expand-file-name x)))
        '("~/.emacs.d"
          ))

(defun require-all (packages)
    (mapcar #'require packages))

(require-all '(
               color-theme
               irblack
               parenface
               bar-cursor
               tls
               erc
               ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL
(color-theme-initialize)

;; (if window-system
;;     (set-background-color "black")
;;     ())
(color-theme-irblack)
;(if window-system
;    (color-theme-gentooish)
;    (color-theme-dark-laptop))
;(load-file "~/.emacs.d/color-theme-twilight.el")
;(color-theme-twilight)
;(load-file "~/.emacs.d/color-theme-inkpot.el")
;(color-theme-inkpot)

;; OS X settings
;; (setq mac-option-key-is-meta nil)
;; (setq mac-command-key-is-meta t)
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier nil)

(bar-cursor-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode 0)
(setq linum-format "%3d ")
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq make-backup-files nil)
(set-language-environment "UTF-8")
(set-input-method "japanese-ascii") ; C-x C-m C-\
(winner-mode t)
;;(display-battery-mode t) ;; broken on 10.9.2
(setq display-time-24hr-format t)
(display-time-mode t)
(line-number-mode 1)
(column-number-mode 1)

(tooltip-mode nil)
(setq midnight-mode t)
(setq column-number-mode nil)
(setq size-indication-mode nil)
(setq mode-line-position nil)
(mouse-avoidance-mode 'animate)
(ido-mode t)

;; Ido and uniquify options from http://curiousprogrammer.wordpress.com/2009/07/13/my-emacs-defaults/
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

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

(autoload 'copy-from-above-command "misc"
  "Copy characters from previous nonblank line, starting just above point.

  \(fn &optional arg)"
  'interactive)
(global-set-key [up] 'copy-from-above-command)
(global-set-key [down] (lambda ()
                         (interactive)
                         (forward-line 1)
                         (open-line 1)
                         (copy-from-above-command)))
(global-set-key [right] (lambda ()
                          (interactive)
                          (copy-from-above-command 1)))
(global-set-key [left] (lambda ()
                         (interactive)
                         (copy-from-above-command -1)
                         (forward-char -1)
                         (delete-char -1)))

;; Proxy for ssh tunnel + privoxy
;; (setq url-proxy-services '(("no_proxy" . "localhost")
;;                            ("http" . "localhost:8118")))

(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; Need to require after setq

(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/backups/" t)))

;; Enable ergoemacs layout
;; (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "dv") ; US Dvorak layout
;; (load "~/.emacs.d/ergoemacs-keybindings-5.1/ergoemacs-mode")
;; (ergoemacs-mode 1)

;; Highlight bad whitespace
(global-whitespace-mode t)
(setq whitespace-style (quote (tabs tab-mark)))
(setq-default show-trailing-whitespace t)

;; Make % work like vi
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Prevent Emacs from stupidly auto-changing my working directory
;; (defun find-file-save-default-directory ()
;;     (interactive)
;;     (setq saved-default-directory default-directory)
;;     (ido-find-file)
;;     (setq default-directory saved-default-directory))
;; (global-set-key "\C-x\C-f" 'find-file-save-default-directory)

;; Enable mit-scheme
(setq scheme-program-name
      "/opt/boxen/homebrew/bin/mit-scheme")
(require 'xscheme)

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
(setq calendar-latitude 45.4)
(setq calendar-longitude -122.6)
(setq calendar-location-name "Portland, OR")
;; (setq calendar-latitude 17.5)
;; (setq calendar-longitude 78.5)
;; (setq calendar-location-name "Hyderabad, India")

;; Start the server for emacsclient
;(server-start)

;; Custom key maps
(defun set-keys (commands)
  (mapcar (lambda (x)
            (global-set-key (read-kbd-macro (first x)) (second x)))
          commands))
(set-keys '(
            ("C-c t" totd)
            ("C-c n" global-linum-mode)
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
            ("C-c e"   ido-erc-buffer)
            ("C-S-<left>"  shrink-window-horizontally)
            ("C-S-<right>" enlarge-window-horizontally)
            ("C-S-<down>"  shrink-window)
            ("C-S-<up>"    enlarge-window)
            ("M-s" save-buffer)
            ("M-p" ctrl-y-in-vi)
            ("M-n" ctrl-e-in-vi)
            ("M-N" make-frame)
            ("M-W" delete-frame)
            ("M-w" ido-kill-buffer)
            ("M-1" delete-other-windows)
            ("M-!" delete-window)
            ("M-2" split-window-horizontally)
            ("M-@" split-window-vertically)
            ("M-a" beginning-of-line)
            ("M-o" other-window)
            ("M-O" other-window)
            ("M-`" next-window)
            ("M-~" previous-window)
            ("M-RET" toggle-fullscreen)
            ))

;;toggle full-screen
(defun toggle-fullscreen ()
(interactive)
(set-frame-parameter
 nil
 'fullscreen
 (if (frame-parameter nil 'fullscreen)
     nil
   'fullboth)))

;; (global-set-key [(meta return)] 'toggle-fullscreen)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))
;; (set-frame-font "Droid Sans Mono Dotted-12")
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (find 'alpha (frame-parameters nil) :key #'car))
       40)
      (set-frame-parameter nil 'alpha '(40 40))
    (set-frame-parameter nil 'alpha '(85 85))))
(global-set-key (kbd "C-c T") 'toggle-transparency)

;; Vim-like scrolling
(defun ctrl-e-in-vi (n)
  (interactive "p")
  (scroll-up n))

(defun ctrl-y-in-vi (n)
  (interactive "p")
  (scroll-down n))

;; (global-set-key (kbd "<XF86AudioPlay>")
;;                 (lambda () (interactive)
;;                   (if (shellfm-running-p)
;;                       (shellfm-skip-track)
;;                     (shellfm 1))))

;; ERC stuff
;; (setq erc-encoding-coding-alist (quote (("#lisp" . utf-8)
;;                                         ("#nihongo" . iso-2022-jp)
;;                                         ("#" . iso-latin-1)
;;                                         ("#" . iso-latin-1))))
;; (autoload 'erc "erc")
;; (add-hook 'erc-mode-hook
;;           '(lambda ()
;;              (setq scroll-margin 0)
;;              (setq erc-scrolltobottom-mode 1)))
;; (load "~/.emacs.d/erc-bip") ;; Passwords here
;; (defun ido-erc-buffer()
;;   (interactive)
;;   (switch-to-buffer
;;    (ido-completing-read "Channel:" 
;;                         (save-excursion
;;                           (delq
;;                            nil
;;                            (mapcar (lambda (buf)
;;                                      (when (buffer-live-p buf)
;;                                        (with-current-buffer buf
;;                                          (and (eq major-mode 'erc-mode)
;;                                               (buffer-name buf)))))
;;                                    (buffer-list)))))))


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
;; '(frame-background-mode (quote dark))
 '(ido-decorations (quote ("" "" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
 '(ido-everywhere t)
 '(inhibit-startup-screen t)
 '(lisp-loop-forms-indentation 6)
 '(lisp-loop-keyword-indentation 6)
 '(lisp-simple-loop-indentation 6)
 '(mode-line-format (quote ("%e--[" mode-line-buffer-identification "]" (vc-mode vc-mode) " " mode-line-modes " " global-mode-string " %-")))
 '(mode-line-in-non-selected-windows t)
 '(mode-line-modes (quote ("%[" "(" (:propertize ("" mode-name)) ("" mode-line-process) (:propertize ("" minor-mode-alist)) "%n" ")" "%]")))
 '(mumamo-background-colors nil)
 '(require-final-newline t)
 '(savehist-mode t nil (savehist))
 '(scroll-conservatively 100000)
 '(scroll-down-aggressively 0.0)
 '(scroll-margin 4)
 '(scroll-step 1)
 '(scroll-up-aggressively 0.0)
 '(show-paren-mode t nil (paren)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mumamo-border-face-in ((t nil)))
 '(mumamo-border-face-out ((t nil))))
