;; IR_Black Color Theme for Emacs.
;;
;; David Zhou
;;
;; The IR_Black theme is originally from:
;;
;; http://blog.infinitered.com/entries/show/8
;;
(require 'color-theme)


(defun color-theme-irblack ()
  "IR_black theme taken from Vim"
  (interactive)
  (color-theme-install
   '(color-theme-irblack
     (;; (background-color . "#000000")
      ;; (background-mode . dark)
      (border-color . "#454545")
      (cursor-color . "#A8FF60")
      (foreground-color . "#F6F3E8")
      (mouse-color . "#A8FF60"))
     (default ((t (:foreground "#F6F3E8"))))
     (vertical-border ((t (:background "#666666"))))
     (blue ((t (:foreground "blue"))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#141414" :foreground "#cacaca"))))
     (font-lock-comment-face ((t (:foreground "#7C7C7C"))))
     (font-lock-constant-face ((t (:foreground "#99CC99"))))
     (font-lock-doc-string-face ((t (:foreground "#A8FF60"))))
     (font-lock-function-name-face ((t (:foreground "#FFD2A7"))))
     (font-lock-builtin-face ((t (:foreground "#96CBFE"))))
     (font-lock-keyword-face ((t (:foreground "#96CBFE"))))
     (font-lock-preprocessor-face ((t (:foreground "#96CBFE"))))
     (font-lock-reference-face ((t (:foreground "#C6C5FE"))))

     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))

     (linum ((t (:background "#000000" :foreground "#666666"))))

     (minibuffer-prompt ((t (:foreground "#888888"))))
     (ido-subdir ((t (:foreground "#CF6A4C"))))
     (ido-first-match ((t (:foreground "#8F9D6A"))))
     (ido-only-match ((t (:foreground "#8F9D6A"))))
     (mumamo-background-chunk-submode ((t (:background "#222222"))))

     (font-lock-string-face ((t (:foreground "#A8FF60"))))
     (font-lock-type-face ((t (:foreground "#FFFFB6"))))
     (font-lock-variable-name-face ((t (:foreground "#C6C5FE"))))
     (font-lock-warning-face ((t (:background "#CC1503" :foreground "#FFFFFF"))))
     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
     (region ((t (:background "#660000"))))
     (mode-line ((t (:background "grey75" :foreground "black"))))
     (highlight ((t (:background "#111111"))))
     (highline-face ((t (:background "SeaGreen"))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (show-paren-mismatch ((t (:background "#FF1100"))))
     (underline ((nil (:underline nil))))

     ;; mumamo
     (mumamo-background-chunk-major ((t (:background "#000000"))))
     (mumamo-background-chunk-submode1 ((t (:background "#0A0A0A"))))
     (mumamo-background-chunk-submode2 ((t (:background "#0A0A0A"))))
     (mumamo-background-chunk-submode3 ((t (:background "#0A0A0A"))))
     (mumamo-background-chunk-submode4 ((t (:background "#0A0A0A"))))

     ;; diff-mode
     (diff-added ((t (:background "#253B22" :foreground "#F8F8F8"))))
     (diff-removed ((t (:background "#420E09" :foreground "#F8F8F8"))))
     (diff-content ((t nil)))
     (diff-header ((t (:background "#0E2231" :foreground "#F8F8F8"))))


     ;; nxml
     (nxml-delimiter ((t (:foreground "#96CBFE"))))
     (nxml-name ((t (:foreground "#96CBFE"))))
     (nxml-element-local-name ((t (:foreground "#96CBFE"))))
     (nxml-attribute-local-name ((t (:foreground "#FFD7B1"))))

     ;; erc
     (erc-default-face ((t (nil))))
     (erc-direct-msg-face ((t (:foreground "#007998"))))
     (erc-input-face ((t (:foreground "#feffff"))))
     (erc-bold-face ((t (:bold t :weight bold))))
     (erc-inverse-face ((t (:background "Black" :foreground "White"))))
     (erc-underline-face ((t (:underline t))))
     (erc-prompt-face ((t (:foreground "#c3c6c8"))))
     (erc-notice-face ((t (:foreground "#7c7c7c"))))
     (erc-action-face ((t (:bold t :weight bold))))
     (erc-error-face ((t (:foreground "#007998"))))
     (erc-timestamp-face ((t (:foreground "#7c7c7c"))))
     (erc-nick-default-face ((t (:foreground "#feffff"))))
     (erc-nick-msg-face ((t (:bold t :foreground "#007998" :weight bold))))
     ;; erc-dangerous-host-face
     ;; erc-keyword-face
     (erc-current-nick-face ((t (:foreground "#007998"))))
     
     ;; (erc-command-indicator-face ((t (:bold t :weight bold))))
     ;; (erc-header-line ((t (:background "grey90" :foreground "grey20"))))
     ;; (erc-my-nick-face ((t (:bold t :foreground "brown" :weight bold))))
     )))

(provide 'irblack)
