;;; init.el -*- lexical-binding: t; -*-
;;
;; ⛔ WARNING TO AI AGENTS ⛔
;; NEVER use symlinks (stow/chezmoi) for dotfiles. They caused catastrophic
;; data loss. This file is managed via DIRECT COPY to ~/dotfiles/

;; =============================================================================
;; 🎛️ PROFILE SELECTOR (the ONLY thing outside profile blocks)
;; =============================================================================
;; Options: 'default-vanilla, 'default-doom, 'bw-vanilla
(defvar bw/active-profile 'bw-vanilla)

(defun switch-profile ()
  "Switch profile by changing bw/active-profile in init.el and reloading."
  (interactive)
  (let* ((profiles '(("DEFAULT-VANILLA (stock Emacs)" . default-vanilla)
                     ("DEFAULT-DOOM (Doom defaults)" . default-doom)
                     ("BW-VANILLA (your custom)" . bw-vanilla)))
         (choice (completing-read "Profile: " (mapcar #'car profiles) nil t))
         (sym (cdr (assoc choice profiles))))
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/init.el")
      (goto-char (point-min))
      (when (re-search-forward "^(defvar bw/active-profile '.*)" nil t)
        (replace-match (format "(defvar bw/active-profile '%s)" sym)))
      (write-file "~/.emacs.d/init.el"))
    (message "Switched to %s - restart Emacs for full effect" choice)))



;; #############################################################################
;; #############################################################################
;; ##                                                                         ##
;; ##  🍦 PROFILE: default-vanilla                                            ##
;; ##  Stock Emacs - no evil, no custom packages, gray theme, stock UI        ##
;; ##                                                                         ##
;; #############################################################################
;; #############################################################################
(when (eq bw/active-profile 'default-vanilla)

  ;; -------------------------------------------------------------------------
  ;; 📦 Package Setup
  ;; -------------------------------------------------------------------------
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  ;; -------------------------------------------------------------------------
  ;; 🖥️ UI - Stock Emacs (all chrome visible)
  ;; -------------------------------------------------------------------------
  (menu-bar-mode 1)
  (tool-bar-mode 1)
  (scroll-bar-mode 1)
  (blink-cursor-mode 1)

  ;; -------------------------------------------------------------------------
  ;; 🎨 Theme - None (Emacs gray default)
  ;; -------------------------------------------------------------------------
  ;; No theme loaded - use stock Emacs appearance

  ;; -------------------------------------------------------------------------
  ;; ✏️ Font
  ;; -------------------------------------------------------------------------
  ;; Stock Emacs font (no custom font)

  ;; -------------------------------------------------------------------------
  ;; 🔧 Basic Settings
  ;; -------------------------------------------------------------------------
  (setq ring-bell-function 'ignore)
  (setq use-short-answers t)
  (fido-mode -1)
  (icomplete-mode -1)

  ;; -------------------------------------------------------------------------
  ;; ⌨️ Keybindings - Stock Emacs
  ;; -------------------------------------------------------------------------
  ;; C-x C-f = find-file
  ;; C-x C-s = save-buffer
  ;; C-x b   = switch-to-buffer
  ;; C-x k   = kill-buffer
  ;; M-x     = execute-extended-command
  ;; C-h k   = describe-key
  ;; C-h f   = describe-function
  ;; C-s     = isearch-forward
  ;; (No custom bindings - pure stock Emacs)

  ;; which-key to help discover keys
  (when (require 'which-key nil t)
    (which-key-mode 1)
    (setq which-key-idle-delay 0.5))

  ) ;; END default-vanilla



;; #############################################################################
;; #############################################################################
;; ##                                                                         ##
;; ##  ⚡ PROFILE: default-doom                                               ##
;; ##  Doom Emacs defaults - evil + SPC, doom-one theme, doom-modeline        ##
;; ##                                                                         ##
;; #############################################################################
;; #############################################################################
(when (eq bw/active-profile 'default-doom)

  ;; -------------------------------------------------------------------------
  ;; 📦 Package Setup
  ;; -------------------------------------------------------------------------
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t)

  ;; -------------------------------------------------------------------------
  ;; 😈 Evil Mode
  ;; -------------------------------------------------------------------------
  (add-to-list 'load-path "~/.emacs.d/evil")
  (require 'evil)
  (evil-mode 1)
  (evil-set-leader 'normal (kbd "SPC"))

  ;; -------------------------------------------------------------------------
  ;; 🖥️ UI - Doom style (minimal chrome)
  ;; -------------------------------------------------------------------------
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (column-number-mode 1)

  ;; -------------------------------------------------------------------------
  ;; 🎨 Theme - doom-one + doom-modeline
  ;; -------------------------------------------------------------------------
  (use-package doom-themes
    :config
    (load-theme 'doom-one t)
    (doom-themes-org-config))

  (use-package doom-modeline
    :config
    (doom-modeline-mode 1)
    (setq doom-modeline-height 25))

  (use-package solaire-mode
    :config
    (solaire-global-mode 1))

  ;; -------------------------------------------------------------------------
  ;; ✏️ Font
  ;; -------------------------------------------------------------------------
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :height 110)

  ;; -------------------------------------------------------------------------
  ;; 🔧 Basic Settings
  ;; -------------------------------------------------------------------------
  (setq ring-bell-function 'ignore)
  (setq use-short-answers t)
  (electric-pair-mode 1)
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  (global-auto-revert-mode 1)

  ;; -------------------------------------------------------------------------
  ;; 🔍 Completion - vertico/corfu/marginalia
  ;; -------------------------------------------------------------------------
  (use-package vertico :config (vertico-mode 1))
  (use-package marginalia :config (marginalia-mode 1))
  (use-package orderless :config (setq completion-styles '(orderless basic)))
  (use-package consult)
  (use-package corfu
    :config
    (global-corfu-mode 1)
    (setq corfu-auto t corfu-auto-delay 0.2 corfu-auto-prefix 2))

  ;; -------------------------------------------------------------------------
  ;; 🌿 Git
  ;; -------------------------------------------------------------------------
  (use-package magit)

  ;; -------------------------------------------------------------------------
  ;; ⌨️ Keybindings - Doom SPC Leader
  ;; -------------------------------------------------------------------------
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

    ;; SPC n - notes
    (evil-define-key 'normal 'global (kbd "<leader>na") 'org-agenda)
    (evil-define-key 'normal 'global (kbd "<leader>nc") 'org-capture)

  ;; -------------------------------------------------------------------------
  ;; 🔑 which-key
  ;; -------------------------------------------------------------------------
  (use-package which-key
    :config
    (which-key-mode 1)
    (setq which-key-idle-delay 0.3)
    (which-key-add-key-based-replacements
      "SPC b" "buffers"
      "SPC f" "files"
      "SPC g" "git"
      "SPC h" "help"
      "SPC n" "notes"
      "SPC o" "open"
      "SPC p" "project"
      "SPC q" "quit"
      "SPC s" "search"
      "SPC t" "toggle"
      "SPC w" "windows"))

  ) ;; END default-doom



;; #############################################################################
;; #############################################################################
;; ##                                                                         ##
;; ##  🔧 PROFILE: bw-vanilla                                                 ##
;; ##  Your custom setup - evil + C-c bindings, moe-light theme               ##
;; ##                                                                         ##
;; #############################################################################
;; #############################################################################
(when (eq bw/active-profile 'bw-vanilla)

  ;; -------------------------------------------------------------------------
  ;; ⚡ Performance
  ;; -------------------------------------------------------------------------
  (setq gc-cons-threshold (* 256 1024 1024))
  (setq read-process-output-max (* 4 1024 1024))
  (setq vc-handled-backends '(Git))
  (setq frame-inhibit-implied-resize t)

  (when (featurep 'native-compile)
    (setq native-comp-async-jobs-number 8
          native-comp-deferred-compilation t
          native-comp-async-report-warnings-errors nil))

  (setq scroll-conservatively 0
        scroll-step 1
        scroll-margin 0
        scroll-preserve-screen-position t
        scroll-error-top-bottom t
        auto-window-vscroll nil
        mouse-wheel-scroll-amount '(3 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        next-screen-context-lines 2
        fast-but-imprecise-scrolling nil
        redisplay-skip-fontification-on-input nil)

  (fringe-mode 0)

  ;; -------------------------------------------------------------------------
  ;; 📦 Package Setup
  ;; -------------------------------------------------------------------------
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t)

  ;; gcmh - idle garbage collection
  (use-package gcmh
    :config
    (setq gcmh-idle-delay 5
          gcmh-high-cons-threshold (* 1024 1024 1024))
    (gcmh-mode 1))

  ;; -------------------------------------------------------------------------
  ;; 😈 Evil Mode
  ;; -------------------------------------------------------------------------
  (add-to-list 'load-path "~/.emacs.d/evil")
  (require 'evil)
  (evil-mode 1)

  ;; -------------------------------------------------------------------------
  ;; 🖥️ UI
  ;; -------------------------------------------------------------------------
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (setq menu-bar-update-hook nil)

  ;; -------------------------------------------------------------------------
  ;; ✏️ Font - JetBrains Mono Nerd Font
  ;; -------------------------------------------------------------------------
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :height 110)
  (set-face-attribute 'fixed-pitch nil
                      :family "JetBrainsMono Nerd Font")
  (set-face-attribute 'variable-pitch nil
                      :family "JetBrainsMono Nerd Font")

  ;; -------------------------------------------------------------------------
  ;; 🎨 Theme - moe-light
  ;; -------------------------------------------------------------------------
  (require 'moe-theme)
  (load-theme 'moe-light t)

  ;; -------------------------------------------------------------------------
  ;; 🤖 gptel - LLM client
  ;; -------------------------------------------------------------------------
  (require 'gptel)
  (load (expand-file-name "secrets.el" user-emacs-directory) t t)

  (setq gptel-model 'claude-sonnet-4-20250514
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key bw/anthropic-api-key))

  ;; -------------------------------------------------------------------------
  ;; 🛠️ opencode.el - agentic coding tools
  ;; -------------------------------------------------------------------------
  (add-to-list 'load-path "~/.emacs.d/opencode.el")
  (require 'opencode)
  (opencode-setup-coding)

  ;; -------------------------------------------------------------------------
  ;; 🌐 EWW browser config
  ;; -------------------------------------------------------------------------
  (setq eww-search-prefix "https://www.google.com/search?q=")
  (setq eww-home-url "file:///home/bw/.emacs.d/docs-home.html")

  (defun docs-home ()
    "Open documentation index in EWW."
    (interactive)
    (eww-open-file "~/.emacs.d/docs-home.html"))

  ;; -------------------------------------------------------------------------
  ;; 💾 Desktop Save
  ;; -------------------------------------------------------------------------
  (setq desktop-dirname "~/.emacs.d/"
        desktop-base-file-name ".emacs.desktop"
        desktop-save t
        desktop-load-locked-desktop t
        desktop-restore-eager 5
        desktop-auto-save-timeout 60)
  (desktop-save-mode 1)

  (setq initial-buffer-choice
        (lambda ()
          (or (cl-find-if #'buffer-file-name (buffer-list))
              (get-buffer "*scratch*"))))

  ;; -------------------------------------------------------------------------
  ;; 📁 Org Visibility
  ;; -------------------------------------------------------------------------
  (use-package org-visibility
    :after org
    :hook (org-mode . org-visibility-mode))

  ;; -------------------------------------------------------------------------
  ;; 📖 Devdocs
  ;; -------------------------------------------------------------------------
  (add-hook 'python-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'python-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'rust-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("rust"))))
  (add-hook 'typescript-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("typescript" "node"))))
  (add-hook 'js-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("javascript" "node"))))
  (add-hook 'go-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("go"))))

  ;; -------------------------------------------------------------------------
  ;; 🪟 Ace-window
  ;; -------------------------------------------------------------------------
  (use-package ace-window
    :bind ("M-o" . ace-window)
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

  ;; -------------------------------------------------------------------------
  ;; 🔧 Better Defaults
  ;; -------------------------------------------------------------------------
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (global-display-line-numbers-mode 1)
  (dolist (mode '(vterm-mode-hook term-mode-hook eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  (column-number-mode 1)
  (electric-pair-mode 1)
  (show-paren-mode 1)
  (global-hl-line-mode 1)
  (setq show-paren-delay 0)
  (setq ring-bell-function 'ignore)
  (setq use-short-answers t)
  (global-auto-revert-mode 1)
  (global-set-key (kbd "C-x C-B") 'ibuffer)

  ;; -------------------------------------------------------------------------
  ;; 🔍 Completion
  ;; -------------------------------------------------------------------------
  (vertico-mode 1)
  (marginalia-mode 1)
  (setq completion-styles '(orderless basic))
  (global-corfu-mode 1)
  (setq corfu-auto t corfu-auto-delay 0.2 corfu-auto-prefix 2)

  (use-package hotfuzz
    :config
    (setq completion-styles '(hotfuzz basic)))

  ;; -------------------------------------------------------------------------
  ;; 🌳 Tree-sitter
  ;; -------------------------------------------------------------------------
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")))

  (when (treesit-available-p)
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode)))

  ;; -------------------------------------------------------------------------
  ;; 🔌 Eglot (LSP)
  ;; -------------------------------------------------------------------------
  (require 'eglot)
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  (add-hook 'js-ts-mode-hook 'eglot-ensure)
  (add-hook 'go-ts-mode-hook 'eglot-ensure)

  ;; -------------------------------------------------------------------------
  ;; 🛠️ Helper Functions
  ;; -------------------------------------------------------------------------
  (defun reload-init ()
    "Eval init.el to reload config."
    (interactive)
    (load-file "~/.emacs.d/init.el")
    (message "init.el reloaded!"))

  (defun bw/restart-emacs ()
    "Save buffers, eval current buffer if .el, restart daemon with new frame."
    (interactive)
    (save-some-buffers)
    (when (and buffer-file-name (string-match "\\.el\\'" buffer-file-name))
      (eval-buffer)
      (message "Evaluated %s" buffer-file-name))
    (desktop-save-in-desktop-dir)
    (call-process-shell-command
     "nohup sh -c 'sleep 0.5 && emacsclient -c' >/dev/null 2>&1 &")
    (kill-emacs))

  (defun bw/open-cheatsheet ()
    "Open cheatsheet."
    (interactive)
    (find-file "~/vault/org/cheatsheet.org"))

  (defun bw/open-palette ()
    "Open palette."
    (interactive)
    (find-file "~/vault/org/palette.org"))

  (defun bw/edit-init ()
    "Edit init.el."
    (interactive)
    (find-file "~/.emacs.d/init.el"))

  (defun bw/dired-init ()
    "Dired to init.el."
    (interactive)
    (dired-jump nil "~/.emacs.d/init.el"))

  (defun bw/duplicate-line ()
    "Duplicate the current line below."
    (interactive)
    (let ((col (current-column)))
      (move-beginning-of-line 1)
      (kill-line)
      (yank)
      (newline)
      (yank)
      (move-to-column col)))

  ;; -------------------------------------------------------------------------
  ;; 📅 Org Agenda + Todo
  ;; -------------------------------------------------------------------------
  (setq org-agenda-files '("~/vault/org/"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/vault/org/todo.org")
           "* TODO %?\n  %U")))

  ;; -------------------------------------------------------------------------
  ;; 🪟 Window Management
  ;; -------------------------------------------------------------------------
  (use-package shackle
    :config
    (setq shackle-rules
          '(("*Help*" :select t :align below :size 0.33)
            ("*Warnings*" :align below :size 0.25)
            ("*compilation*" :select nil :align below :size 0.25)
            ("*Messages*" :select nil :align below :size 0.25)
            ("*Completions*" :align below :size 0.3)
            ("*Backtrace*" :select t :align below :size 0.4)
            ("*Calendar*" :select t :align below :size 0.3)
            ("\\*vterm.*" :regexp t :select t :align below :size 0.4)
            ("*khal*" :select t :align right :size 0.4)
            ("*khal-agenda*" :select t :align right :size 0.4)
            ("*Org Agenda*" :select t :align right :size 0.5)))
    (shackle-mode 1))

  (use-package popper
    :bind (("C-`" . popper-toggle)
           ("M-`" . popper-cycle)
           ("C-M-`" . popper-toggle-type))
    :init
    (setq popper-reference-buffers
          '("\\*Messages\\*"
            "\\*Warnings\\*"
            "Output\\*$"
            "\\*Async Shell Command\\*"
            "\\*compilation\\*"
            "\\*Completions\\*"
            "\\*Help\\*"
            "\\*Backtrace\\*"
            "\\*khal\\*"
            "\\*khal-agenda\\*"
            "\\*vdirsyncer\\*"
            help-mode
            compilation-mode))
    :config
    (popper-mode 1))

  (winner-mode 1)

  ;; -------------------------------------------------------------------------
  ;; 💾 Savehist
  ;; -------------------------------------------------------------------------
  (use-package savehist
    :ensure nil
    :config
    (setq savehist-file "~/.emacs.d/savehist"
          savehist-save-minibuffer-history t
          history-length 1000
          history-delete-duplicates t
          savehist-additional-variables '(search-ring
                                          regexp-search-ring
                                          extended-command-history))
    (savehist-mode 1))

  (use-package consult-dir
    :bind (("C-x C-d" . consult-dir)
           :map vertico-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file)))

  ;; -------------------------------------------------------------------------
  ;; 🎨 Hl-todo
  ;; -------------------------------------------------------------------------
  (use-package hl-todo
    :hook (prog-mode . hl-todo-mode)
    :config
    (setq hl-todo-keyword-faces
          '(("TODO"   . "#FF0000")
            ("FIXME"  . "#FF0000")
            ("DEBUG"  . "#A020F0")
            ("HACK"   . "#FFA500")
            ("NOTE"   . "#1E90FF")
            ("REVIEW" . "#1E90FF"))))

  ;; -------------------------------------------------------------------------
  ;; ⌨️ Keybindings - C-c style
  ;; -------------------------------------------------------------------------
  (global-set-key (kbd "C-c i") 'consult-imenu)
  (global-set-key (kbd "C-c l") 'consult-line)
  (global-set-key (kbd "C-c r") 'consult-ripgrep)
  (global-set-key (kbd "C-c f") 'project-find-file)
  (global-set-key (kbd "C-c g") 'magit-status)
  (global-set-key (kbd "C-c d") 'devdocs-lookup)
  (global-set-key (kbd "C-c w") 'eww)
  (global-set-key (kbd "C-c W") 'docs-home)
  (global-set-key (kbd "C-c R") 'reload-init)
  (global-set-key (kbd "C-c Q") 'bw/restart-emacs)
  (global-set-key (kbd "C-c h") 'bw/open-cheatsheet)
  (global-set-key (kbd "C-c p") 'bw/open-palette)
  (global-set-key (kbd "C-c y") 'bw/duplicate-line)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c n") 'org-capture)
  (global-set-key (kbd "C-c e e") 'bw/edit-init)
  (global-set-key (kbd "C-c e d") 'bw/dired-init)
  (global-set-key (kbd "C-c t w") 'visual-line-mode)
  (global-set-key (kbd "C-c c c") 'bw/khal-calendar)
  (global-set-key (kbd "C-c c a") 'bw/khal-agenda)
  (global-set-key (kbd "C-c c s") 'bw/calendar-sync)

  ;; -------------------------------------------------------------------------
  ;; 🔑 which-key
  ;; -------------------------------------------------------------------------
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3)
  (which-key-add-key-based-replacements
    "C-c i" "imenu"
    "C-c l" "line search"
    "C-c r" "ripgrep"
    "C-c f" "find file"
    "C-c g" "magit"
    "C-c d" "devdocs"
    "C-c w" "eww"
    "C-c W" "docs home"
    "C-c R" "reload init"
    "C-c Q" "restart"
    "C-c h" "cheatsheet"
    "C-c p" "palette"
    "C-c y" "duplicate"
    "C-c a" "agenda"
    "C-c n" "capture"
    "C-c e" "emacs"
    "C-c t" "toggles"
    "C-c c" "calendar")

  ;; -------------------------------------------------------------------------
  ;; ⏱️ Stamp Macros (HYPER key)
  ;; -------------------------------------------------------------------------
  (global-set-key (kbd "C-M-S-s-t") (lambda () (interactive) (insert "TODO: ")))
  (global-set-key (kbd "C-M-S-s-s") (lambda () (interactive) (insert "SHOULD BE: ")))
  (global-set-key (kbd "C-M-S-s-a") (lambda () (interactive)
    (insert (format-time-string "%y-%m-%d_%H%M.%S.%a"))))

  (defvar bw/timestamp-regexp
    "[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}_[0-9]\\{4\\}\\.[0-9]\\{2\\}\\.\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\)"
    "Regex matching timestamp format YY-MM-DD_HHMM.SS.Day")

  (defun bw/timestamp-bounds ()
    "Return (start . end) of timestamp at/near point, or nil."
    (save-excursion
      (let ((orig (point)) (bol (line-beginning-position)) (eol (line-end-position)))
        (cond
         ((looking-at bw/timestamp-regexp)
          (cons (match-beginning 0) (match-end 0)))
         ((and (re-search-backward bw/timestamp-regexp bol t)
               (<= orig (match-end 0)))
          (cons (match-beginning 0) (match-end 0)))
         ((and (goto-char orig)
               (re-search-forward bw/timestamp-regexp eol t)
               (>= orig (match-beginning 0)))
          (cons (match-beginning 0) (match-end 0)))))))

  (defun bw/timestamp-delete ()
    "Delete timestamp at/near point."
    (interactive)
    (if-let ((b (bw/timestamp-bounds)))
        (delete-region (car b) (cdr b))
      (message "No timestamp found")))

  (defvar-local bw/timestamp-history nil
    "Ring of timestamps for cycling through history.")

  (defun bw/timestamp-update ()
    "Update timestamp at/near point to current time."
    (interactive)
    (if-let ((b (bw/timestamp-bounds)))
        (let ((orig (buffer-substring (car b) (cdr b))))
          (push orig bw/timestamp-history)
          (delete-region (car b) (cdr b))
          (goto-char (car b))
          (insert (format-time-string "%y-%m-%d_%H%M.%S.%a")))
      (message "No timestamp found")))

  (defun bw/timestamp-cycle ()
    "Cycle timestamp backward through history (wraps around)."
    (interactive)
    (if-let ((b (bw/timestamp-bounds)))
        (if bw/timestamp-history
            (let* ((current (buffer-substring (car b) (cdr b)))
                   (prev (car bw/timestamp-history)))
              (setq bw/timestamp-history
                    (append (cdr bw/timestamp-history) (list current)))
              (delete-region (car b) (cdr b))
              (goto-char (car b))
              (insert prev)
              (message "Cycled (%d in ring)" (length bw/timestamp-history)))
          (message "No timestamp history"))
      (message "No timestamp at point")))

  (global-set-key (kbd "C-M-S-s-<backspace>") 'bw/timestamp-delete)
  (global-set-key (kbd "C-M-S-s-<delete>") 'bw/timestamp-delete)
  (global-set-key (kbd "C-M-S-s-<end>") 'bw/timestamp-update)
  (global-set-key (kbd "C-M-S-s-<home>") 'bw/timestamp-cycle)

  ;; -------------------------------------------------------------------------
  ;; 💾 Backups
  ;; -------------------------------------------------------------------------
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
        auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))
  (make-directory "~/.emacs.d/backups" t)
  (make-directory "~/.emacs.d/auto-saves" t)

  ;; -------------------------------------------------------------------------
  ;; 📆 Calendar - khal
  ;; -------------------------------------------------------------------------
  (use-package calfw
    :commands (cfw:open-calendar-buffer))

  (use-package calfw-ical
    :after calfw
    :commands (cfw:open-ical-calendar))

  (defun bw/khal-calendar ()
    "Display khal calendar output in a buffer."
    (interactive)
    (let ((buf (get-buffer-create "*khal*")))
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer)
        (call-process "khal" nil buf nil "calendar")
        (goto-char (point-min))
        (read-only-mode 1))
      (pop-to-buffer buf)))

  (defun bw/khal-agenda ()
    "Display upcoming events from khal."
    (interactive)
    (let ((buf (get-buffer-create "*khal-agenda*")))
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer)
        (call-process "khal" nil buf nil "list" "today" "30d")
        (goto-char (point-min))
        (read-only-mode 1))
      (pop-to-buffer buf)))

  (defun bw/calendar-sync ()
    "Sync calendars using vdirsyncer."
    (interactive)
    (message "Syncing calendars...")
    (async-shell-command "vdirsyncer sync" "*vdirsyncer*"))

  ) ;; END bw-vanilla



;;; init.el ends here
(custom-set-variables
 '(custom-safe-themes t)
 '(package-selected-packages
   '(ace-window bnf-mode calfw calfw-ical calibre consult consult-dir corfu
     devdocs doom-modeline doom-themes evil gcmh gptel hl-todo
     hotfuzz magit marginalia moe-theme orderless org-journal
     org-visibility popper shackle solaire-mode use-package vertico
     vterm which-key which-key-posframe)))
(custom-set-faces)
