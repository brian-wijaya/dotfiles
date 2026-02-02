;;; bindings-doom-basic.el --- default-doom SPC keybindings -*- lexical-binding: t; -*-
;;
;; Compact keybindings for the default-doom profile.
;; Uses evil-define-key directly (no prefix keymap structure).

;; Top-level
(evil-define-key 'normal 'global (kbd "<leader>SPC") 'execute-extended-command)
(evil-define-key 'normal 'global (kbd "<leader>.") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>,") 'consult-buffer)
(evil-define-key 'normal 'global (kbd "<leader>/") 'consult-ripgrep)
(evil-define-key 'normal 'global (kbd "<leader>:") 'eval-expression)
(evil-define-key 'normal 'global (kbd "<leader>`") 'evil-switch-to-windows-last-buffer)

;; SPC b - buffers
(evil-define-key 'normal 'global (kbd "<leader>bb") 'consult-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bd") 'kill-current-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bi") 'ibuffer)
(evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-current-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bn") 'next-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bp") 'previous-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bs") 'save-buffer)

;; SPC f - files
(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>fr") 'consult-recent-file)
(evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>fS") 'save-some-buffers)
(evil-define-key 'normal 'global (kbd "<leader>fp") 'project-find-file)

;; SPC g - git
(evil-define-key 'normal 'global (kbd "<leader>gg") 'magit-status)
(evil-define-key 'normal 'global (kbd "<leader>gb") 'magit-blame)
(evil-define-key 'normal 'global (kbd "<leader>gl") 'magit-log)

;; SPC h - help
(evil-define-key 'normal 'global (kbd "<leader>hf") 'describe-function)
(evil-define-key 'normal 'global (kbd "<leader>hv") 'describe-variable)
(evil-define-key 'normal 'global (kbd "<leader>hk") 'describe-key)
(evil-define-key 'normal 'global (kbd "<leader>hm") 'describe-mode)
(evil-define-key 'normal 'global (kbd "<leader>hb") 'describe-bindings)
(evil-define-key 'normal 'global (kbd "<leader>hi") 'info)

;; SPC n - notes
(evil-define-key 'normal 'global (kbd "<leader>na") 'org-agenda)
(evil-define-key 'normal 'global (kbd "<leader>nc") 'org-capture)

;; SPC o - open
(evil-define-key 'normal 'global (kbd "<leader>ot") 'vterm)
(evil-define-key 'normal 'global (kbd "<leader>od") 'dired-jump)
(evil-define-key 'normal 'global (kbd "<leader>oa") 'org-agenda)

;; SPC p - project
(evil-define-key 'normal 'global (kbd "<leader>pp") 'project-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>pf") 'project-find-file)
(evil-define-key 'normal 'global (kbd "<leader>ps") 'consult-ripgrep)
(evil-define-key 'normal 'global (kbd "<leader>pb") 'project-switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>pk") 'project-kill-buffers)

;; SPC q - quit
(evil-define-key 'normal 'global (kbd "<leader>qq") 'save-buffers-kill-terminal)
(evil-define-key 'normal 'global (kbd "<leader>qQ") 'kill-emacs)

;; SPC s - search
(evil-define-key 'normal 'global (kbd "<leader>ss") 'consult-line)
(evil-define-key 'normal 'global (kbd "<leader>sp") 'consult-ripgrep)
(evil-define-key 'normal 'global (kbd "<leader>si") 'consult-imenu)
(evil-define-key 'normal 'global (kbd "<leader>sb") 'consult-line-multi)

;; SPC t - toggle
(evil-define-key 'normal 'global (kbd "<leader>tl") 'display-line-numbers-mode)
(evil-define-key 'normal 'global (kbd "<leader>tw") 'visual-line-mode)
(evil-define-key 'normal 'global (kbd "<leader>th") 'hl-line-mode)

;; SPC w - windows
(evil-define-key 'normal 'global (kbd "<leader>ww") 'other-window)
(evil-define-key 'normal 'global (kbd "<leader>wd") 'delete-window)
(evil-define-key 'normal 'global (kbd "<leader>ws") 'split-window-below)
(evil-define-key 'normal 'global (kbd "<leader>wv") 'split-window-right)
(evil-define-key 'normal 'global (kbd "<leader>wh") 'windmove-left)
(evil-define-key 'normal 'global (kbd "<leader>wj") 'windmove-down)
(evil-define-key 'normal 'global (kbd "<leader>wk") 'windmove-up)
(evil-define-key 'normal 'global (kbd "<leader>wl") 'windmove-right)
(evil-define-key 'normal 'global (kbd "<leader>w=") 'balance-windows)
(evil-define-key 'normal 'global (kbd "<leader>w1") 'delete-other-windows)

;; which-key labels
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "SPC b" "📄 buffers"
    "SPC f" "📂 files"
    "SPC g" "🌿 git"
    "SPC h" "❓ help"
    "SPC n" "📝 notes"
    "SPC o" "📖 open"
    "SPC p" "🗂️ project"
    "SPC q" "🚪 quit"
    "SPC s" "🔍 search"
    "SPC t" "🔀 toggle"
    "SPC w" "🪟 windows"))
