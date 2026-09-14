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
(setq doom-font (font-spec :family "LiterationMono Nerd Font" :size 14))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Don't auto-insert matching quotes/parens/brackets — type only what I type.
;; Doom enables smartparens globally for delimiter auto-close; turn that off.
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(after! smartparens
  (smartparens-global-mode -1))

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
;; Personal kanbanflow: TODO backlog → NEXT (≤3) → DOING → WAITING/DONE
(setq org-todo-keywords '((sequence "SWEEP(s)" "TODO(t)" "NEXT(n)" "DOING(o)" "WAITING(w)" "LATER(l)" "|" "DONE(d)" "CANCELLED(c)")
                          (sequence "[ ](T)" "[-](p)" "[?](m)" "|" "[X](D)")))
(setq org-log-done 'time)

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

(after! org
  (add-to-list 'org-export-backends 'confluence))

;; for https://github.com/zerolfx/copilot.el
;; accept completion from copilot and fallback to company
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map company-active-map
         ("<tab>" . 'my-tab)
         ("TAB" . 'my-tab)
         :map company-mode-map
         ("<tab>" . 'my-tab)
         ("TAB" . 'my-tab)))

(use-package! ox-jira
  :after org)

;; From https://github.com/hlissner/doom-emacs/issues/581
(defun dlukes/ediff-doom-config (file)
  "ediff the current config with the examples in doom-emacs-dir

There are multiple config files, so FILE specifies which one to
diff.
"
  (interactive
    (list (read-file-name "Config file to diff: " doom-user-dir)))
  (let* ((stem (file-name-base file))
          (customized-file (format "%s.el" stem))
          (template-file-regex (format "^%s.example.el$" stem)))
    (ediff-files
      (concat doom-user-dir customized-file)
      (car (directory-files-recursively
             doom-emacs-dir
             template-file-regex
             nil
             (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d)))))))))

;; allow using a bookmarklet from firefox to save links to org-mode
;;  Create ~/.local/share/applications/org-protocol.desktop
;; ```
;; [Desktop Entry]
;; Name=Org Protocol
;; Exec=emacsclient %u
;; Type=Application
;; Terminal=false
;; MimeType=x-scheme-handler/org-protocol;
;; ```
;;
;; Then run: `xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol`
;; Create a Firefox bookmarklet:
;; `javascript:location.href='org-protocol://capture?template=L&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title);`
(require 'org-protocol)
(setq org-capture-templates
      '(("L" "Link" entry (file+headline "~/org/links.org" "Uncategorized")
         "** [[%:link][[%<%Y-%m-%d %a %H:%M>]]]\t:links:\n%:description\n" :immediate-finish t)
        ("t" "Board task" entry (file "~/org/board.org")
         "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n%a\n")))

(use-package! org-agenda-kanban
  :commands (org-agenda-kanban)
  :config
  (setq org-agenda-kanban-files (list (expand-file-name "board.org" org-directory))
        org-agenda-kanban-columns '("TODO" "NEXT" "DOING" "WAITING" "DONE")
        org-agenda-kanban-done-within-days 7)
  ;; Vim-style navigation instead of package defaults n/p/f/b.
  ;; k was capture (same as c) — steal it for "up".
  (define-key org-agenda-kanban-mode-map (kbd "h") #'org-agenda-kanban-backward-column)
  (define-key org-agenda-kanban-mode-map (kbd "j") #'org-agenda-kanban-next-card)
  (define-key org-agenda-kanban-mode-map (kbd "k") #'org-agenda-kanban-previous-card)
  (define-key org-agenda-kanban-mode-map (kbd "l") #'org-agenda-kanban-forward-column)
  (define-key org-agenda-kanban-mode-map (kbd "J") #'my/org-agenda-kanban-move-down)
  (define-key org-agenda-kanban-mode-map (kbd "K") #'my/org-agenda-kanban-move-up)
  (define-key org-agenda-kanban-mode-map (kbd "n") nil)
  (define-key org-agenda-kanban-mode-map (kbd "p") nil)
  (define-key org-agenda-kanban-mode-map (kbd "f") nil)
  (define-key org-agenda-kanban-mode-map (kbd "b") nil)
  ;; Within a column, equal-priority cards keep board.org document order.
  ;; J/K bubble the selected heading past same-level siblings with
  ;; org-move-subtree-up/down until it sits before/after its board neighbor.
  ;; (Hand cut/paste corrupted or duplicated entries; don't use those.)
  (defun my/org-agenda-kanban-move-in-column (delta)
    "Move selected card by DELTA (+1 down, -1 up) among same-priority peers.
Reorders the heading in the source file; board uses priority sort with
stable document order for ties."
    (let* ((card (org-agenda-kanban--selected-card))
           (_ (unless card (user-error "No card selected")))
           (col (org-agenda-kanban-card-todo card))
           (cards (org-agenda-kanban--cards-for-column
                   col (org-agenda-kanban--filtered org-agenda-kanban--cards)))
           (id (org-agenda-kanban-card-id card))
           (idx (cl-position id cards :key #'org-agenda-kanban-card-id
                             :test #'equal))
           (other-idx (and idx (+ idx delta)))
           (other (and other-idx (>= other-idx 0) (nth other-idx cards)))
           (loc-card (org-agenda-kanban--locate card))
           (loc-other (and other (org-agenda-kanban--locate other))))
      (unless other (user-error "No card in that direction"))
      (unless (= (org-agenda-kanban--priority-rank card)
                 (org-agenda-kanban--priority-rank other))
        (user-error "Neighbor has a different priority (use , or +/- first)"))
      (unless (and loc-card loc-other (eq (car loc-card) (car loc-other)))
        (user-error "Cannot locate both headings in the same file"))
      (with-current-buffer (car loc-card)
        (org-with-wide-buffer
         (let ((other-m (copy-marker (cdr loc-other) t))
               (guard 0))
           (goto-char (cdr loc-card))
           (org-back-to-heading t)
           (if (< delta 0)
               ;; Move up until this heading starts before the neighbor.
               (while (and (> (point) (marker-position other-m))
                           (< (cl-incf guard) 100))
                 (let ((p (point)))
                   (org-move-subtree-up 1)
                   (when (= p (point))
                     (user-error "Cannot move further up"))))
             ;; Move down until this heading starts after the neighbor.
             (while (and (< (point) (marker-position other-m))
                         (< (cl-incf guard) 100))
               (let ((p (point)))
                 (org-move-subtree-down 1)
                 (when (= p (point))
                   (user-error "Cannot move further down")))))
           (set-marker other-m nil))))
      (setq org-agenda-kanban--cards (org-agenda-kanban--collect))
      (org-agenda-kanban--apply-filters)
      (org-agenda-kanban--select id)
      (message "Reordered \"%s\"" (org-agenda-kanban-card-title card))))
  (defun my/org-agenda-kanban-move-up ()
    "Move selected card up within its priority group in the column."
    (interactive)
    (my/org-agenda-kanban-move-in-column -1))
  (defun my/org-agenda-kanban-move-down ()
    "Move selected card down within its priority group in the column."
    (interactive)
    (my/org-agenda-kanban-move-in-column 1))
  ;; Board uses single-letter keys on the major-mode map (h/j/k/l/t/…).
  ;; Evil normal-state steals those for motions — same problem org-agenda has
  ;; without evil-org-agenda. Prefer emacs-state so package keys work as-is;
  ;; C-z toggles back to vim if needed.
  (set-evil-initial-state! 'org-agenda-kanban-mode 'emacs)
  ;; If you land in normal somehow, still prefer the board keymap.
  (evil-make-overriding-map org-agenda-kanban-mode-map 'normal)
  (add-hook 'org-agenda-kanban-mode-hook #'evil-normalize-keymaps)
  ;; emacs-state has no evil C-w map; restore window motion without C-z.
  (evil-define-key* 'emacs org-agenda-kanban-mode-map
    (kbd "C-w") evil-window-map
    (kbd "C-h") #'evil-window-left
    (kbd "C-j") #'evil-window-down
    (kbd "C-k") #'evil-window-up
    (kbd "C-l") #'evil-window-right)
  ;; Don't treat *Kanban* as a Doom popup (avoids side/split display).
  (set-popup-rule! "^\\*Kanban\\*" :ignore t)
  ;; Open the board in the current window (or focus it if already visible).
  (defadvice! my/org-agenda-kanban-same-window-a (&rest _)
    :override #'org-agenda-kanban
    (interactive)
    (let ((buffer (get-buffer-create org-agenda-kanban-buffer-name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'org-agenda-kanban-mode)
          (org-agenda-kanban-mode))
        (org-agenda-kanban-refresh))
      (if-let ((win (get-buffer-window buffer)))
          (select-window win)
        (switch-to-buffer buffer))))
  ;; RET on a card: replace the board window (don't jump to/reuse another split
  ;; that already shows board.org — package uses reuse-window first).
  ;; Must switch current-buffer too: set-window-buffer alone leaves point/show
  ;; running in *Kanban*, so the heading jump is a no-op.
  (defadvice! my/org-agenda-kanban-visit-same-window-a (fn loc keep-board-focus)
    :around #'org-agenda-kanban--show-heading
    (if keep-board-focus
        (funcall fn loc keep-board-focus)
      (cl-letf (((symbol-function #'pop-to-buffer-same-window)
                 (lambda (buffer &optional norecord)
                   (let ((switch-to-buffer-obey-display-actions nil))
                     (switch-to-buffer buffer norecord)))))
        (funcall fn loc nil))))
  ;; Capture/edits write board.org but the package only re-collects on g/r or
  ;; its own move/todo commands. Refresh any live board after capture or save.
  (defun my/org-agenda-kanban-refresh-if-live ()
    (when-let ((buf (get-buffer org-agenda-kanban-buffer-name)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (derived-mode-p 'org-agenda-kanban-mode)
            (org-agenda-kanban-refresh))))))
  (defun my/org-agenda-kanban-refresh-after-capture ()
    (unless org-note-abort
      (my/org-agenda-kanban-refresh-if-live)))
  (defun my/org-agenda-kanban-refresh-on-source-save ()
    (when (and buffer-file-name
               (member (file-truename buffer-file-name)
                       (mapcar #'file-truename
                               (or org-agenda-kanban-files
                                   (org-agenda-files t)))))
      (my/org-agenda-kanban-refresh-if-live)))
  (add-hook 'org-capture-after-finalize-hook
            #'my/org-agenda-kanban-refresh-after-capture)
  (add-hook 'after-save-hook #'my/org-agenda-kanban-refresh-on-source-save))

;; Keep Org typing responsive: debounce Org indent refresh and disable org-appear.
(after! org
  (defvar-local my/org-indent-refresh-timer nil)
  (defvar-local my/org-indent-refresh-beg nil)
  (defvar-local my/org-indent-refresh-end nil)

  (defun my/org-indent-run-refresh (buffer)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq my/org-indent-refresh-timer nil)
        (when (and org-indent-mode my/org-indent-refresh-beg my/org-indent-refresh-end)
          (let ((beg my/org-indent-refresh-beg)
                (end my/org-indent-refresh-end))
            (setq my/org-indent-refresh-beg nil
                  my/org-indent-refresh-end nil)
            (org-indent-refresh-maybe beg end 0))))))

  (defun my/org-indent-refresh-debounced (beg end _len)
    (when org-indent-mode
      (setq my/org-indent-refresh-beg
            (if my/org-indent-refresh-beg
                (min my/org-indent-refresh-beg beg)
              beg)
            my/org-indent-refresh-end
            (if my/org-indent-refresh-end
                (max my/org-indent-refresh-end end)
              end))
      (when (timerp my/org-indent-refresh-timer)
        (cancel-timer my/org-indent-refresh-timer))
      (setq my/org-indent-refresh-timer
            (run-with-idle-timer 0.25 nil #'my/org-indent-run-refresh (current-buffer)))))

  (defun my/enable-debounced-org-indent ()
    (org-indent-mode 1)
    (remove-hook 'after-change-functions #'org-indent-refresh-maybe t)
    (add-hook 'after-change-functions #'my/org-indent-refresh-debounced nil t)
    (when (fboundp 'org-appear-mode)
      (org-appear-mode -1)))

  (setq org-startup-indented nil)
  (remove-hook 'org-mode-hook #'org-indent-mode)
  (remove-hook 'org-mode-hook #'org-appear-mode)
  (add-hook 'org-mode-hook #'my/enable-debounced-org-indent))
