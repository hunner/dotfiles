(defconst dot-emacs (concat (getenv "HOME") "/" ".emacs.hunner.el")
              "My dot emacs file")

(require 'bytecomp)
(setq compiled-dot-emacs (byte-compile-dest-file dot-emacs))

(if (or (not (file-exists-p compiled-dot-emacs))
            (file-newer-than-file-p dot-emacs compiled-dot-emacs)
                    (equal (nth 4 (file-attributes dot-emacs)) (list 0 0)))
      (load dot-emacs)
        (load compiled-dot-emacs))

(add-hook 'kill-emacs-hook
                    '(lambda () (and (file-newer-than-file-p dot-emacs compiled-dot-emacs)
                                                                (byte-compile-file dot-emacs))))
