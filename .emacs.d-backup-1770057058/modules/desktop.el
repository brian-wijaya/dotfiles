;;; desktop.el --- desktop save mode -*- lexical-binding: t; -*-
(setq desktop-dirname "~/.emacs.d/"
      desktop-base-file-name ".emacs.desktop"
      desktop-save t
      desktop-load-locked-desktop t
      desktop-restore-eager t
      desktop-auto-save-timeout 60)
(desktop-save-mode 1)

(setq initial-buffer-choice
      (lambda ()
        (or (cl-find-if #'buffer-file-name (buffer-list))
            (get-buffer "*scratch*"))))
