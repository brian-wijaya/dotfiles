;;; bw-vanilla.el --- Custom setup with C-c bindings -*- lexical-binding: t; -*-
;;
;; Profile: bw-vanilla
;; Evil + C-c bindings, moe-light theme, full features.

(let ((mod (expand-file-name "modules/" user-emacs-directory)))

  ;; --- Foundation ---
  (load (expand-file-name "performance" mod))
  (load (expand-file-name "pkg-setup" mod))
  (load (expand-file-name "evil-core" mod))
  (load (expand-file-name "evil-collection" mod))

  ;; --- UI ---
  (load (expand-file-name "ui-minimal" mod))
  (load (expand-file-name "settings" mod))

  ;; Theme: moe-light (inline, not a module)
  (require 'moe-theme)
  (load-theme 'moe-light t)

  ;; --- Completion & Tools ---
  (load (expand-file-name "completion" mod))
  (load (expand-file-name "git" mod))
  (load (expand-file-name "gptel" mod))
  (load (expand-file-name "opencode" mod))
  (load (expand-file-name "eww" mod))
  (load (expand-file-name "devdocs" mod))
  (load (expand-file-name "desktop" mod))
  (load (expand-file-name "org" mod))
  (load (expand-file-name "window-mgmt" mod))
  (load (expand-file-name "savehist" mod))
  (load (expand-file-name "hl-todo" mod))
  (load (expand-file-name "treesitter" mod))
  (load (expand-file-name "eglot" mod))
  (load (expand-file-name "backups" mod))
  (load (expand-file-name "helpers" mod))
  (load (expand-file-name "calendar" mod))
  (load (expand-file-name "stamps" mod))
  (load (expand-file-name "email" mod))

  ;; --- Bindings ---
  (load (expand-file-name "bindings-vanilla" mod)))

;; which-key (basic config, inline — no posframe for vanilla)
(which-key-mode 1)
(setq which-key-idle-delay 0.3)
