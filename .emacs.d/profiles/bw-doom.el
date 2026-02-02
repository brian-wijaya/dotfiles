;;; bw-doom.el --- Doom defaults + full custom setup -*- lexical-binding: t; -*-
;;
;; Profile: bw-doom
;; Evil + SPC leader, doom-one theme, eager loading, all features.

(let ((mod (expand-file-name "modules/" user-emacs-directory)))

  ;; --- Foundation ---
  (load (expand-file-name "performance" mod))
  (load (expand-file-name "pkg-setup" mod))
  (load (expand-file-name "evil-core" mod))
  (load (expand-file-name "evil-collection" mod))
  (load (expand-file-name "mcp-server" mod))
  (load (expand-file-name "evil-leader" mod))

  ;; --- UI ---
  (load (expand-file-name "ui-minimal" mod))
  (load (expand-file-name "ui-doom" mod))
  (load (expand-file-name "keyhints" mod))
  (load (expand-file-name "settings" mod))

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
  (load (expand-file-name "kotlin-ts-mode" mod))
  (load (expand-file-name "vterm-config" mod))
  (load (expand-file-name "eglot" mod))
  (load (expand-file-name "backups" mod))
  (load (expand-file-name "helpers" mod))
  (load (expand-file-name "calendar" mod))
  (load (expand-file-name "stamps" mod))
  (load (expand-file-name "email" mod))
  (load (expand-file-name "neotree" mod))
  (load (expand-file-name "ibuffer" mod))

  ;; --- New features (specs 04-11) ---
  (load (expand-file-name "imenu" mod))
  (load (expand-file-name "dirvish" mod))
  (load (expand-file-name "vsnake" mod))
  (load (expand-file-name "which-key-explore" mod))
  (load (expand-file-name "eldoc-box" mod))
  (load (expand-file-name "doc-panel" mod))
  (load (expand-file-name "breadcrumb" mod))
  (load (expand-file-name "symbols-outline" mod))
  (load (expand-file-name "deepwiki" mod))
  (load (expand-file-name "elfeed" mod))
  (load (expand-file-name "vault-rag" mod))
  (load (expand-file-name "nov" mod))

  ;; --- Bindings (must come after all modules define their commands) ---
  (load (expand-file-name "bindings-doom-full" mod))

  ;; --- which-key posframe (must come last, after bindings set labels) ---
  (load (expand-file-name "which-key-posframe" mod)))
