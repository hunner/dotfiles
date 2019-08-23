;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(require 'dap-go)

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
