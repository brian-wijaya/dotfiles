;;; settings.el --- basic editor settings -*- lexical-binding: t; -*-
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq ring-bell-function 'ignore)
(setq use-short-answers t)

;; Disable confirmation prompts
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)

(setq select-enable-clipboard t)
(setq select-enable-primary nil)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-auto-revert-mode 1)
