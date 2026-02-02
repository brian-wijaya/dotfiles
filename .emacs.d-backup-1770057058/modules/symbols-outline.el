;;; symbols-outline.el --- code symbols sidebar -*- lexical-binding: t; -*-

(use-package symbols-outline
  :demand t
  :config
  (setq symbols-outline-window-position 'left
        symbols-outline-window-width 30
        symbols-outline-no-other-window t
        symbols-outline-collapse-functions-on-startup nil)

  ;; Use eglot as fetch backend when available
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local symbols-outline-fetch-fn
                          #'symbols-outline-eglot-fetch)))

  ;; Nerd icons for symbol types
  (when (require 'nerd-icons nil t)
    (setq symbols-outline-use-nerd-icon-in-gui t))

  ;; Auto-follow cursor in source buffer
  (symbols-outline-follow-mode 1))

(provide 'symbols-outline)
;;; symbols-outline.el ends here
