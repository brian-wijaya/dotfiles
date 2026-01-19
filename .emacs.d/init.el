;;; init.el
;;
;; ⛔ WARNING TO AI AGENTS ⛔
;; NEVER use symlinks (stow/chezmoi) for dotfiles. They caused catastrophic
;; data loss. This file is managed via DIRECT COPY: ~/dotfiles/sync.sh
;;

;; =============================================================================
;; Performance
;; =============================================================================
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
;; No fringes (removes arrow indicators)
(fringe-mode 0)
;; Word wrap toggle
(global-set-key (kbd "C-c t w") 'visual-line-mode)

;; =============================================================================
;; Package Setup
;; =============================================================================
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

;; =============================================================================
;; UI
;; =============================================================================
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq menu-bar-update-hook nil)


;; =============================================================================
;; Font - JetBrains Mono Nerd Font everywhere
;; =============================================================================
(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"
                    :height 110)
(set-face-attribute 'fixed-pitch nil
                    :family "JetBrainsMono Nerd Font")
(set-face-attribute 'variable-pitch nil
                    :family "JetBrainsMono Nerd Font")

;; =============================================================================
;; Theme - moe-light
;; =============================================================================
(require 'moe-theme)
(load-theme 'moe-light t)

;; gptel - LLM client
(require 'gptel)
(load (expand-file-name "secrets.el" user-emacs-directory) t t) ; API keys (git-crypt)

(setq gptel-model 'claude-sonnet-4-20250514
      gptel-backend (gptel-make-anthropic "Claude"
                      :stream t
                      :key bw/anthropic-api-key))


;; opencode.el - agentic coding tools
(add-to-list 'load-path "~/.emacs.d/opencode.el")
(require 'opencode)
(opencode-setup-coding)

;; Google Search, not DuckDuckGo as default EWW search
(setq eww-search-prefix "https://www.google.com/search?q=")
(global-set-key (kbd "C-c w") 'eww)  ;; quick access


;; =============================================================================
;; Desktop Save
;; =============================================================================
(setq desktop-dirname "~/.emacs.d/"
      desktop-base-file-name ".emacs.desktop"
      desktop-save t
      desktop-load-locked-desktop t
      desktop-restore-eager 5
      desktop-auto-save-timeout 60)
(desktop-save-mode 1)

;; Daemon mode: new frames show most recent file buffer (not scratch)
(setq initial-buffer-choice
      (lambda ()
        (or (cl-find-if #'buffer-file-name (buffer-list))
            (get-buffer "*scratch*"))))








;; =============================================================================
;; Devdocs - auto-select docs per language
;; =============================================================================
(add-hook 'python-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
(add-hook 'python-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
(add-hook 'rust-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("rust"))))
(add-hook 'typescript-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("typescript" "node"))))
(add-hook 'js-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("javascript" "node"))))
(add-hook 'go-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("go"))))
(global-set-key (kbd "C-c d") 'devdocs-lookup)

;; =============================================================================
;; Better Defaults
;; =============================================================================
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-display-line-numbers-mode 1)
;; Disable line numbers in terminals
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

;; =============================================================================
;; Completion
;; =============================================================================
(vertico-mode 1)
(marginalia-mode 1)
(setq completion-styles '(orderless basic))
(global-corfu-mode 1)
(setq corfu-auto t corfu-auto-delay 0.2 corfu-auto-prefix 2)

;; =============================================================================
;; Tree-sitter + file associations
;; =============================================================================
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

;; =============================================================================
;; Eglot (LSP)
;; =============================================================================
(require 'eglot)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)

;; =============================================================================
;; Which-key (uses minibuffer - can't be selected with C-x o)
;; =============================================================================
(which-key-mode 1)
(setq which-key-idle-delay 0.5
      which-key-popup-type 'minibuffer)

;; =============================================================================
;; Navigation (high-value bindings)
;; =============================================================================
(global-set-key (kbd "C-c i") 'consult-imenu)      ; jump to function
(global-set-key (kbd "C-c l") 'consult-line)       ; search buffer with preview
(global-set-key (kbd "C-c r") 'consult-ripgrep)    ; search project
(global-set-key (kbd "C-c f") 'project-find-file)  ; find file in project
(global-set-key (kbd "C-c g") 'magit-status)       ; git
(global-set-key (kbd "C-x C-B") 'ibuffer)          ; buffer manager (powerful)

;; Quick reference files
(global-set-key (kbd "C-c ?") (lambda () (interactive)
                                (find-file "~/.emacs.d/cheatsheet.org")))
(global-set-key (kbd "C-c p") (lambda () (interactive)
                                (find-file "~/.emacs.d/palette.org")))

;; =============================================================================
;; Stamp Macros (HYPER key = C-M-S-s on Planck EZ)
;; Oryx Hyper = Alt+Shift+Ctrl+Cmd = C-M-S-s- in Emacs
;; =============================================================================
(global-set-key (kbd "C-M-S-s-t") (lambda () (interactive) (insert "TODO: ")))
(global-set-key (kbd "C-M-S-s-s") (lambda () (interactive) (insert "SHOULD BE: ")))
(global-set-key (kbd "C-M-S-s-a") (lambda () (interactive)
  (insert (format-time-string "%y-%m-%d_%H%M.%S.%a"))))

;; Timestamp manipulation
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
            ;; Rotate: move first to end, add current to end
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

;; =============================================================================
;; Keep backups/autosaves out of working directories
;; =============================================================================
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/auto-saves" t)

;; =============================================================================
;; Calendar - khal integration via vdirsyncer
;; =============================================================================
(use-package calfw
  :commands (cfw:open-calendar-buffer))

(use-package calfw-ical
  :after calfw
  :commands (cfw:open-ical-calendar))

;; Show khal calendar in Emacs
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

;; Show khal agenda (upcoming events)
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

;; Sync calendar via vdirsyncer
(defun bw/calendar-sync ()
  "Sync calendars using vdirsyncer."
  (interactive)
  (message "Syncing calendars...")
  (async-shell-command "vdirsyncer sync" "*vdirsyncer*"))

;; Calendar keybindings (C-c c prefix)
(global-set-key (kbd "C-c c c") 'bw/khal-calendar)
(global-set-key (kbd "C-c c a") 'bw/khal-agenda)
(global-set-key (kbd "C-c c s") 'bw/calendar-sync)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(moe-light))
 '(custom-safe-themes t)
 '(package-selected-packages
   '(bnf-mode calfw calfw-ical calibre consult corfu devdocs gcmh gptel
              magit marginalia moe-theme orderless use-package vertico
              vterm which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
