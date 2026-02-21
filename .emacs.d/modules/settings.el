;;; settings.el --- basic editor settings -*- lexical-binding: t; -*-
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq ring-bell-function 'ignore)
(setq use-short-answers t)

;; Disable confirmation prompts
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)

;; Suppress GTK dialog popups — always use minibuffer
(setq use-dialog-box nil)
(setq use-file-dialog nil)

;; Never prompt on auto-revert — agent-edited files change frequently
(setq revert-without-query '("."))
(setq auto-revert-verbose nil)

(setq select-enable-clipboard t)
(setq select-enable-primary nil)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-auto-revert-mode 1)

;; Auto-create parent directories on save
(defun bw/auto-make-parent-dirs ()
  "Create parent directories if they don't exist."
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(add-hook 'before-save-hook #'bw/auto-make-parent-dirs)
