;;; eldoc-box.el --- floating documentation childframe -*- lexical-binding: t; -*-

(use-package eldoc-box
  :demand t
  :config
  ;; Use eldoc-box for eglot hover docs
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode)

  ;; Styling
  (setq eldoc-box-max-pixel-width 600
        eldoc-box-max-pixel-height 400
        eldoc-box-cleanup-interval 1
        eldoc-box-only-multi-line t)  ;; only show childframe for multi-line docs

  ;; Face for the childframe border
  (set-face-attribute 'eldoc-box-border nil :background "#51afef"))
