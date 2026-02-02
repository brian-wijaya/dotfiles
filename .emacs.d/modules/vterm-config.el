;;; vterm-config.el --- vterm clipboard and evil integration -*- lexical-binding: t; -*-

(use-package vterm
  :demand t
  :config
  ;; Enable OSC52 clipboard manipulation for better copy/paste
  (setq vterm-enable-manipulate-selection-data-by-osc52 t)
  
  ;; Set shell to use bracketed paste mode
  (setq vterm-shell (executable-find "bash"))
  
  ;; Better kill-ring integration
  (setq vterm-copy-exclude-prompt t)
  
  ;; Evil integration - yank from vterm to kill-ring
  (with-eval-after-load 'evil
    (evil-define-key 'normal vterm-mode-map
      (kbd "yy") (lambda ()
                   (interactive)
                   (vterm-send-key "a" nil nil t)  ; C-a (beginning of line)
                   (vterm-send-key "k" nil nil t)  ; C-k (kill to end)
                   (when kill-ring
                     (gui-select-text (car kill-ring)))))))

(provide 'vterm-config)
;;; vterm-config.el ends here
