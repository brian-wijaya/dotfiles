;; bindings-doom-full.el --- bw-doom SPC keybindings + which-key labels -*- lexical-binding: t; -*-
;;
;; All SPC leader bindings for the bw-doom profile.
;; Requires: evil-leader.el (keymap structure), all modules loaded.

;; =========================================================================
;; Top-level shortcuts
;; =========================================================================
(define-key bw/leader-map (kbd "SPC") 'execute-extended-command)
(define-key bw/leader-map (kbd "!") 'shell-command)
(define-key bw/leader-map (kbd "&") 'async-shell-command)
(define-key bw/leader-map (kbd "*") 'bw/search-symbol-at-point)
(define-key bw/leader-map (kbd ".") 'find-file)
(define-key bw/leader-map (kbd ",") 'consult-buffer)
(define-key bw/leader-map (kbd "/") 'consult-ripgrep)
(define-key bw/leader-map (kbd ":") 'eval-expression)
(define-key bw/leader-map (kbd ";") 'eval-expression)
(define-key bw/leader-map (kbd "'") 'vertico-repeat)
(define-key bw/leader-map (kbd "`") 'evil-switch-to-windows-last-buffer)
(define-key bw/leader-map (kbd "u") 'universal-argument)
(define-key bw/leader-map (kbd "x") 'scratch-buffer)
;; Spec 05: which-key explorer at top level
(define-key bw/leader-map (kbd "~") 'bw/which-key-explore-toggle)

;; =========================================================================
;; SPC a - applications
;; =========================================================================
(define-key bw/leader-a-map (kbd "r") 'elfeed)
;; Spec 05: explorer
(define-key bw/leader-a-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC a")))

;; =========================================================================
;; SPC b - buffers
;; =========================================================================
(define-key bw/leader-b-map (kbd "b") 'consult-buffer)
(define-key bw/leader-b-map (kbd "d") 'kill-current-buffer)
(define-key bw/leader-b-map (kbd "i") 'ibuffer)
(define-key bw/leader-b-map (kbd "k") 'kill-current-buffer)
(define-key bw/leader-b-map (kbd "n") 'next-buffer)
(define-key bw/leader-b-map (kbd "p") 'previous-buffer)
(define-key bw/leader-b-map (kbd "s") 'save-buffer)
(define-key bw/leader-b-map (kbd "l") 'evil-switch-to-windows-last-buffer)
(define-key bw/leader-b-map (kbd "N") 'bw/new-buffer)
(define-key bw/leader-b-map (kbd "r") 'revert-buffer)
(define-key bw/leader-b-map (kbd "R") 'rename-buffer)
(define-key bw/leader-b-map (kbd "x") 'scratch-buffer)
(define-key bw/leader-b-map (kbd "z") 'bury-buffer)
(define-key bw/leader-b-map (kbd "e") 'bw/editor)
;; Spec 05: explorer in buffer menu
(define-key bw/leader-b-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC b")))

;; =========================================================================
;; SPC c - code
;; =========================================================================
;; Existing
(define-key bw/leader-c-map (kbd "a") 'eglot-code-actions)
(define-key bw/leader-c-map (kbd "d") 'xref-find-definitions)
(define-key bw/leader-c-map (kbd "D") 'xref-find-references)
(define-key bw/leader-c-map (kbd "f") 'eglot-format)
(define-key bw/leader-c-map (kbd "r") 'eglot-rename)
(define-key bw/leader-c-map (kbd "y") 'bw/duplicate-line)
(define-key bw/leader-c-map (kbd "w") 'delete-trailing-whitespace)
(define-key bw/leader-c-map (kbd "x") 'flymake-show-diagnostics-buffer)
;; Spec 10: doc-panel
(define-key bw/leader-c-map (kbd "p") 'bw/doc-panel-toggle)
(define-key bw/leader-c-map (kbd "P") 'bw/doc-panel-pin-toggle)
(define-key bw/leader-c-map (kbd "k") 'eldoc-box-help-at-point)
(define-key bw/leader-c-map (kbd "h") 'eglot-show-call-hierarchy)
(define-key bw/leader-c-map (kbd "H") 'eglot-show-type-hierarchy)
;; Spec 11: symbols-outline, treesit explorer, deepwiki
(define-key bw/leader-c-map (kbd "o") 'symbols-outline-show)
(define-key bw/leader-c-map (kbd "t") 'treesit-explore-mode)
(define-key bw/leader-c-map (kbd "T") 'treesit-inspect-mode)
(define-key bw/leader-c-map (kbd "m") 'bw/deepwiki-view)
(define-key bw/leader-c-map (kbd "M") 'bw/deepwiki-ask)
(define-key bw/leader-c-map (kbd "W") 'bw/deepwiki-browse)
;; Spec 05: explorer in code menu
(define-key bw/leader-c-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC c")))

;; =========================================================================
;; SPC f - files
;; =========================================================================
(define-key bw/leader-f-map (kbd "f") 'find-file)
(define-key bw/leader-f-map (kbd "r") 'consult-recent-file)
(define-key bw/leader-f-map (kbd "s") 'save-buffer)
(define-key bw/leader-f-map (kbd "S") 'save-some-buffers)
(define-key bw/leader-f-map (kbd "p") 'project-find-file)
(define-key bw/leader-f-map (kbd "e") 'bw/dired-init)
(define-key bw/leader-f-map (kbd "E") 'bw/edit-init)
(define-key bw/leader-f-map (kbd "c") 'copy-file)
(define-key bw/leader-f-map (kbd "D") 'delete-file)
(define-key bw/leader-f-map (kbd "R") 'rename-file)
(define-key bw/leader-f-map (kbd "y") 'bw/yank-file-path)
;; Spec 05: explorer
(define-key bw/leader-f-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC f")))

;; =========================================================================
;; SPC g - git
;; =========================================================================
(define-key bw/leader-g-map (kbd "g") 'magit-status)
(define-key bw/leader-g-map (kbd "b") 'magit-blame)
(define-key bw/leader-g-map (kbd "l") 'magit-log)
(define-key bw/leader-g-map (kbd "c") 'magit-commit)
(define-key bw/leader-g-map (kbd "C") 'magit-clone)
(define-key bw/leader-g-map (kbd "d") 'magit-diff)
(define-key bw/leader-g-map (kbd "f") 'magit-fetch)
(define-key bw/leader-g-map (kbd "F") 'magit-pull)
(define-key bw/leader-g-map (kbd "i") 'magit-init)
(define-key bw/leader-g-map (kbd "p") 'magit-push)
(define-key bw/leader-g-map (kbd "r") 'magit-rebase)
(define-key bw/leader-g-map (kbd "s") 'magit-stage)
(define-key bw/leader-g-map (kbd "S") 'magit-stash)
(define-key bw/leader-g-map (kbd "t") 'magit-tag)
;; Spec 05: explorer
(define-key bw/leader-g-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC g")))

;; =========================================================================
;; SPC h - help
;; =========================================================================
(define-key bw/leader-h-map (kbd "f") 'describe-function)
(define-key bw/leader-h-map (kbd "v") 'describe-variable)
(define-key bw/leader-h-map (kbd "k") 'describe-key)
(define-key bw/leader-h-map (kbd "m") 'describe-mode)
(define-key bw/leader-h-map (kbd "b") 'describe-bindings)
(define-key bw/leader-h-map (kbd "i") 'info)
(define-key bw/leader-h-map (kbd "d") 'devdocs-lookup)
(define-key bw/leader-h-map (kbd "a") 'apropos)
(define-key bw/leader-h-map (kbd "c") 'describe-char)
(define-key bw/leader-h-map (kbd "e") 'view-echo-area-messages)
(define-key bw/leader-h-map (kbd "F") 'describe-face)
(define-key bw/leader-h-map (kbd "l") 'view-lossage)
(define-key bw/leader-h-map (kbd "M") 'man)
(define-key bw/leader-h-map (kbd "o") 'describe-symbol)
(define-key bw/leader-h-map (kbd "p") 'find-library)
(define-key bw/leader-h-map (kbd "P") 'describe-package)
(define-key bw/leader-h-map (kbd "t") 'help-with-tutorial)
(define-key bw/leader-h-map (kbd "w") 'where-is)
(define-key bw/leader-h-map (kbd "x") 'describe-command)
(define-key bw/leader-hr-map (kbd "R") 'bw/reload-init)
(define-key bw/leader-hr-map (kbd "t") 'bw/reload-theme)
;; Spec 05: explorer
(define-key bw/leader-h-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC h")))

;; =========================================================================
;; SPC i - insert
;; =========================================================================
(define-key bw/leader-i-map (kbd "t") 'bw/stamp-todo)
(define-key bw/leader-i-map (kbd "S") 'bw/stamp-should-be)
(define-key bw/leader-i-map (kbd "d") 'bw/stamp-timestamp)
(define-key bw/leader-i-map (kbd "e") 'emoji-search)
(define-key bw/leader-i-map (kbd "f") 'insert-file)
(define-key bw/leader-i-map (kbd "r") 'evil-show-registers)
(define-key bw/leader-i-map (kbd "s") 'yas-insert-snippet)
(define-key bw/leader-i-map (kbd "u") 'insert-char)
(define-key bw/leader-i-map (kbd "y") 'yank-from-kill-ring)
;; Spec 05: explorer
(define-key bw/leader-i-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC i")))

;; =========================================================================
;; SPC n - notes
;; =========================================================================
(define-key bw/leader-n-map (kbd "a") 'org-agenda)
(define-key bw/leader-n-map (kbd "c") 'org-capture)
;; Spec 05: explorer
(define-key bw/leader-n-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC n")))

;; =========================================================================
;; SPC o - open
;; =========================================================================
;; Spec 09: dirvish replaces dired-jump and dired
(define-key bw/leader-o-map (kbd "-") 'dirvish-dwim)
(define-key bw/leader-o-map (kbd "a") 'org-agenda)
(define-key bw/leader-o-map (kbd "A") 'bw/agenda)
(define-key bw/leader-o-map (kbd "b") 'bookmark-jump)
(define-key bw/leader-o-map (kbd "B") 'bookmark-set)
(define-key bw/leader-o-map (kbd "c") 'calendar)
(define-key bw/leader-o-map (kbd "C") 'bw/calendar)
(define-key bw/leader-o-map (kbd "d") 'dirvish)
(define-key bw/leader-o-map (kbd "E") 'bw/code-explorer)
(define-key bw/leader-o-map (kbd "e") 'bw/editor)
(define-key bw/leader-o-map (kbd "f") 'consult-recent-file)
(define-key bw/leader-o-map (kbd "g") 'gptel)
(define-key bw/leader-o-map (kbd "i") 'consult-imenu)
(define-key bw/leader-o-map (kbd "m") 'bw/email)
(define-key bw/leader-o-map (kbd "M") 'man)
(define-key bw/leader-o-map (kbd "n") 'org-capture)
(define-key bw/leader-o-map (kbd "o") 'opencode)
(define-key bw/leader-o-map (kbd "p") 'list-processes)
(define-key bw/leader-o-map (kbd "P") 'bw/open-palette)
(define-key bw/leader-o-map (kbd "r") 'consult-recent-file)
(define-key bw/leader-o-map (kbd "s") 'shell)
(define-key bw/leader-o-map (kbd "S") 'bw/calendar-sync)
(define-key bw/leader-o-map (kbd "t") 'vterm)
(define-key bw/leader-o-map (kbd "T") 'neotree-toggle)
(define-key bw/leader-o-map (kbd "F") 'neotree-find)
(define-key bw/leader-o-map (kbd "w") 'eww)
(define-key bw/leader-o-map (kbd ".") 'bw/edit-init)
(define-key bw/leader-o-map (kbd "D") 'bw/docs-home)
;; SPC o h - reference pages
(define-key bw/leader-oh-map (kbd "c") 'bw/open-cheatsheet)
(define-key bw/leader-oh-map (kbd "d") 'devdocs-peruse)
(define-key bw/leader-oh-map (kbd "h") 'info-emacs-manual)
(define-key bw/leader-oh-map (kbd "H") 'info)
;; Spec 05: explorer
(define-key bw/leader-o-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC o")))

;; =========================================================================
;; SPC p - project
;; =========================================================================
(define-key bw/leader-p-map (kbd "!") 'project-shell-command)
(define-key bw/leader-p-map (kbd "&") 'project-async-shell-command)
(define-key bw/leader-p-map (kbd "b") 'project-switch-to-buffer)
(define-key bw/leader-p-map (kbd "c") 'project-compile)
(define-key bw/leader-p-map (kbd "d") 'project-dired)
(define-key bw/leader-p-map (kbd "e") 'project-eshell)
(define-key bw/leader-p-map (kbd "f") 'project-find-file)
(define-key bw/leader-p-map (kbd "F") 'project-find-dir)
(define-key bw/leader-p-map (kbd "g") 'project-find-regexp)
(define-key bw/leader-p-map (kbd "k") 'project-kill-buffers)
(define-key bw/leader-p-map (kbd "p") 'project-switch-project)
(define-key bw/leader-p-map (kbd "r") 'project-query-replace-regexp)
(define-key bw/leader-p-map (kbd "s") 'consult-ripgrep)
(define-key bw/leader-p-map (kbd "x") 'project-execute-extended-command)
;; Spec 05: explorer
(define-key bw/leader-p-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC p")))

;; =========================================================================
;; SPC q - quit
;; =========================================================================
(define-key bw/leader-q-map (kbd "f") 'delete-frame)
(define-key bw/leader-q-map (kbd "K") 'save-buffers-kill-emacs)
(define-key bw/leader-q-map (kbd "q") 'save-buffers-kill-terminal)
(define-key bw/leader-q-map (kbd "Q") 'kill-emacs)
(define-key bw/leader-q-map (kbd "r") 'restart-emacs)
(define-key bw/leader-q-map (kbd "R") 'bw/restart-emacs)
;; Spec 05: explorer
(define-key bw/leader-q-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC q")))

;; =========================================================================
;; SPC s - search
;; =========================================================================
(define-key bw/leader-s-map (kbd "b") 'consult-line-multi)
(define-key bw/leader-s-map (kbd "d") 'consult-ripgrep)
(define-key bw/leader-s-map (kbd "f") 'consult-find)
(define-key bw/leader-s-map (kbd "g") 'consult-grep)
;; Spec 08: consult-imenu on SPC s i (already existed, keeping)
(define-key bw/leader-s-map (kbd "i") 'consult-imenu)
(define-key bw/leader-s-map (kbd "I") 'consult-imenu-multi)
(define-key bw/leader-s-map (kbd "l") 'consult-line)
(define-key bw/leader-s-map (kbd "m") 'consult-bookmark)
(define-key bw/leader-s-map (kbd "o") 'consult-outline)
(define-key bw/leader-s-map (kbd "p") 'consult-ripgrep)
(define-key bw/leader-s-map (kbd "r") 'consult-register)
(define-key bw/leader-s-map (kbd "s") 'consult-line)
(define-key bw/leader-s-map (kbd "S") 'bw/search-symbol-at-point)
;; Spec 05: explorer
(define-key bw/leader-s-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC s")))

;; =========================================================================
;; SPC t - toggle
;; =========================================================================
(define-key bw/leader-t-map (kbd "b") 'toggle-scroll-bar)
(define-key bw/leader-t-map (kbd "c") 'display-fill-column-indicator-mode)
(define-key bw/leader-t-map (kbd "f") 'flymake-mode)
(define-key bw/leader-t-map (kbd "F") 'toggle-frame-fullscreen)
(define-key bw/leader-t-map (kbd "h") 'hl-line-mode)
;; Spec 08: imenu-list takes SPC t i, indent-guide moves to SPC t I
(define-key bw/leader-t-map (kbd "i") 'imenu-list-smart-toggle)
(define-key bw/leader-t-map (kbd "I") 'indent-guide-mode)
(define-key bw/leader-t-map (kbd "l") 'display-line-numbers-mode)
(define-key bw/leader-t-map (kbd "r") 'read-only-mode)
(define-key bw/leader-t-map (kbd "s") 'flyspell-mode)
(define-key bw/leader-t-map (kbd "t") 'consult-theme)
(define-key bw/leader-t-map (kbd "v") 'visible-mode)
(define-key bw/leader-t-map (kbd "w") 'visual-line-mode)
(define-key bw/leader-t-map (kbd "z") 'bw/zen-mode)
;; Spec 05: explorer
(define-key bw/leader-t-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC t")))

;; =========================================================================
;; SPC w - windows
;; =========================================================================
(define-key bw/leader-w-map (kbd "+") 'enlarge-window)
(define-key bw/leader-w-map (kbd "-") 'shrink-window)
(define-key bw/leader-w-map (kbd "<") 'shrink-window-horizontally)
(define-key bw/leader-w-map (kbd ">") 'enlarge-window-horizontally)
(define-key bw/leader-w-map (kbd "=") 'balance-windows)
(define-key bw/leader-w-map (kbd "1") 'delete-other-windows)
(define-key bw/leader-w-map (kbd "2") 'split-window-below)
(define-key bw/leader-w-map (kbd "3") 'split-window-right)
(define-key bw/leader-w-map (kbd "0") 'delete-window)
(define-key bw/leader-w-map (kbd "d") 'delete-window)
(define-key bw/leader-w-map (kbd "h") 'windmove-left)
(define-key bw/leader-w-map (kbd "j") 'windmove-down)
(define-key bw/leader-w-map (kbd "k") 'windmove-up)
(define-key bw/leader-w-map (kbd "l") 'windmove-right)
(define-key bw/leader-w-map (kbd "H") 'windmove-swap-states-left)
(define-key bw/leader-w-map (kbd "J") 'windmove-swap-states-down)
(define-key bw/leader-w-map (kbd "K") 'windmove-swap-states-up)
(define-key bw/leader-w-map (kbd "L") 'windmove-swap-states-right)
(define-key bw/leader-w-map (kbd "m") 'maximize-window)
(define-key bw/leader-w-map (kbd "o") 'other-window)
(define-key bw/leader-w-map (kbd "q") 'quit-window)
(define-key bw/leader-w-map (kbd "r") 'winner-redo)
(define-key bw/leader-w-map (kbd "s") 'split-window-below)
(define-key bw/leader-w-map (kbd "u") 'winner-undo)
(define-key bw/leader-w-map (kbd "v") 'split-window-right)
(define-key bw/leader-w-map (kbd "w") 'ace-window)
(define-key bw/leader-w-map (kbd "x") 'ace-swap-window)
;; Spec 04: vsnake
(define-key bw/leader-w-map (kbd "V") 'bw/vsnake-toggle)
;; Spec 05: explorer
(define-key bw/leader-w-map (kbd "~") (lambda () (interactive) (bw/which-key-explore-prefix "SPC w")))

;; Repeatable window resize keys (2s timeout)
(repeat-mode 1)
(setq repeat-exit-timeout 2.0)

(defvar bw/window-resize-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "+" #'enlarge-window)
    (define-key map "-" #'shrink-window)
    (define-key map "<" #'shrink-window-horizontally)
    (define-key map ">" #'enlarge-window-horizontally)
    (define-key map "=" #'balance-windows)
    map)
  "Repeat map for window resize commands.")

(dolist (cmd '(enlarge-window shrink-window
               shrink-window-horizontally enlarge-window-horizontally
               balance-windows))
  (put cmd 'repeat-map 'bw/window-resize-repeat-map))


;; =========================================================================
;; which-key labels (loaded after which-key is available)
;; =========================================================================
(with-eval-after-load 'which-key

  ;; --- Leader prefix labels ---
  (which-key-add-keymap-based-replacements bw/leader-map
    "a" "📱+apps"
    "b" "📋+buffers"
    "c" "💻+code"
    "f" "📂+files"
    "g" "🌿+git"
    "h" "❓+help"
    "i" "✏️+insert"
    "n" "📝+notes"
    "o" "📖+open"
    "p" "🗂️+project"
    "q" "🚪+quit"
    "s" "🔍+search"
    "t" "🔀+toggle"
    "w" "🪟+windows"
    ;; Top-level commands
    "SPC" "🚀 M-x"
    "!" "💥 shell-cmd"
    "&" "⚡ async-cmd"
    "*" "🔍 symbol"
    "." "📄 find-file"
    "," "🔄 buffer"
    "/" "🔎 ripgrep"
    ":" "⚡ eval"
    ";" "⚡ eval"
    "'" "🔁 repeat"
    "`" "↩️ last-buf"
    "u" "🔢 universal"
    "x" "📝 scratch"
    "~" "🔭 explore")

  ;; --- Applications ---
  (which-key-add-keymap-based-replacements bw/leader-a-map
    "r" "📰 RSS feeds"
    "~" "🔭 explore")

  ;; --- Buffers ---
  (which-key-add-keymap-based-replacements bw/leader-b-map
    "b" "🔄 switch"
    "d" "❌ kill"
    "e" "📝 editor"
    "i" "📋 ibuffer"
    "k" "❌ kill"
    "l" "↩️ last"
    "n" "➡️ next"
    "N" "✨ new"
    "p" "⬅️ prev"
    "r" "🔄 revert"
    "R" "✏️ rename"
    "s" "💾 save"
    "x" "📝 scratch"
    "z" "📦 bury"
    "~" "🔭 explore")

  ;; --- Code (Spec 06: full command names for SPC c only) ---
  (which-key-add-keymap-based-replacements bw/leader-c-map
    "a" "🎯 eglot-code-actions"
    "d" "🔍 xref-find-definitions"
    "D" "📚 xref-find-references"
    "f" "🎨 eglot-format"
    "r" "✏️ eglot-rename"
    "w" "🧹 delete-trailing-whitespace"
    "x" "⚠️ flymake-diagnostics"
    "y" "📑 duplicate-line"
    ;; Spec 10: doc-panel
    "p" "📋 doc-panel"
    "P" "📌 doc-panel-pin"
    "k" "💡 quick-doc"
    "h" "📞 call-hierarchy"
    "H" "🏗️ type-hierarchy"
    ;; Spec 11: code intelligence
    "o" "🔍 symbols-outline"
    "t" "🌳 treesit-explore"
    "T" "🔬 treesit-inspect"
    "m" "📖 deepwiki-view"
    "M" "💬 deepwiki-ask"
    "W" "🌐 deepwiki-browse"
    "~" "🔭 explore")

  ;; --- Files ---
  (which-key-add-keymap-based-replacements bw/leader-f-map
    "c" "📋 copy"
    "D" "🗑️ delete"
    "e" "⚙️ dired-init"
    "E" "⚙️ edit-init"
    "f" "📄 find"
    "p" "🗂️ project"
    "r" "🕐 recent"
    "R" "✏️ rename"
    "s" "💾 save"
    "S" "💾 save-all"
    "y" "📋 yank-path"
    "~" "🔭 explore")

  ;; --- Git ---
  (which-key-add-keymap-based-replacements bw/leader-g-map
    "b" "👤 blame"
    "c" "💾 commit"
    "C" "📥 clone"
    "d" "📊 diff"
    "f" "📥 fetch"
    "F" "📥 pull"
    "g" "📊 status"
    "i" "🆕 init"
    "l" "📜 log"
    "p" "📤 push"
    "r" "🔀 rebase"
    "s" "➕ stage"
    "S" "📦 stash"
    "t" "🏷️ tag"
    "~" "🔭 explore")

  ;; --- Help ---
  (which-key-add-keymap-based-replacements bw/leader-h-map
    "a" "🔍 apropos"
    "b" "📖 bindings"
    "c" "🔤 char"
    "d" "📚 devdocs"
    "e" "📨 messages"
    "f" "🔧 function"
    "F" "🎨 face"
    "i" "ℹ️ info"
    "k" "⌨️ key"
    "l" "📜 lossage"
    "m" "📋 mode"
    "M" "📖 man"
    "o" "🔍 symbol"
    "p" "📚 library"
    "P" "📦 package"
    "r" "🔄+reload"
    "t" "📖 tutorial"
    "v" "📦 variable"
    "w" "❓ where-is"
    "x" "🔧 command"
    "~" "🔭 explore")

  ;; --- Help reload ---
  (which-key-add-keymap-based-replacements bw/leader-hr-map
    "r" "🔄 reload"
    "R" "🔄 reload-init"
    "t" "🎨 theme")

  ;; --- Insert ---
  (which-key-add-keymap-based-replacements bw/leader-i-map
    "d" "📅 timestamp"
    "e" "😀 emoji"
    "f" "📄 file"
    "r" "📋 register"
    "s" "📝 snippet"
    "S" "💭 SHOULD-BE"
    "t" "✅ TODO"
    "u" "🔤 unicode"
    "y" "📋 kill-ring"
    "~" "🔭 explore")

  ;; --- Notes ---
  (which-key-add-keymap-based-replacements bw/leader-n-map
    "a" "📅 agenda"
    "c" "✏️ capture"
    "~" "🔭 explore")

  ;; --- Open ---
  (which-key-add-keymap-based-replacements bw/leader-o-map
    ;; Spec 09: dirvish replaces dired labels
    "-" "📁 dirvish"
    "a" "📅 org-agenda"
    "A" "📅 bw-agenda"
    "b" "🔖 bookmark"
    "B" "🔖 set-bookmark"
    "c" "📆 calendar"
    "C" "📆 bw-calendar"
    "d" "📁 dirvish-open"
    "E" "🔭 code-explore"
    "D" "🏠 docs-home"
    "e" "📝 editor"
    "f" "🕐 recent"
    "F" "🌲 neotree-find"
    "g" "🤖 gptel"
    "h" "📚+refs"
    "i" "📍 imenu"
    "m" "📧 mail"
    "M" "📖 man"
    "n" "✏️ capture"
    "o" "🛠️ opencode"
    "p" "📊 processes"
    "P" "🎨 palette"
    "r" "🕐 recent"
    "s" "🐚 shell"
    "S" "🔄 cal-sync"
    "t" "💻 vterm"
    "T" "🌲 neotree"
    "w" "🌐 eww"
    "." "⚙️ init.el"
    "~" "🔭 explore")

  ;; --- Open refs ---
  (which-key-add-keymap-based-replacements bw/leader-oh-map
    "c" "📋 cheatsheet"
    "d" "📖 devdocs"
    "h" "📘 emacs-manual"
    "H" "ℹ️ info")

  ;; --- Project ---
  (which-key-add-keymap-based-replacements bw/leader-p-map
    "!" "💥 shell-cmd"
    "&" "⚡ async-cmd"
    "b" "🔄 buffer"
    "c" "🔨 compile"
    "d" "📁 dired"
    "e" "🐚 eshell"
    "f" "📄 find-file"
    "F" "📁 find-dir"
    "g" "🔎 grep"
    "k" "❌ kill-bufs"
    "p" "📂 switch"
    "r" "🔄 replace"
    "s" "🔎 ripgrep"
    "x" "🚀 run-cmd"
    "~" "🔭 explore")

  ;; --- Quit ---
  (which-key-add-keymap-based-replacements bw/leader-q-map
    "f" "🖼️ del-frame"
    "K" "💾 save-kill"
    "q" "💾 save-quit"
    "Q" "⚡ quit-now"
    "r" "🔄 restart"
    "R" "🔄 bw-restart"
    "~" "🔭 explore")

  ;; --- Search ---
  (which-key-add-keymap-based-replacements bw/leader-s-map
    "b" "📚 all-buffers"
    "d" "📁 directory"
    "f" "📄 find-file"
    "g" "🔎 grep"
    "i" "📍 imenu"
    "I" "📍 imenu-all"
    "l" "📄 line"
    "m" "🔖 bookmark"
    "o" "📑 outline"
    "p" "🔎 ripgrep"
    "r" "📋 register"
    "s" "📄 line"
    "S" "🔍 symbol"
    "~" "🔭 explore")

  ;; --- Toggle ---
  (which-key-add-keymap-based-replacements bw/leader-t-map
    "b" "📜 scrollbar"
    "c" "📏 fill-col"
    "f" "⚠️ flymake"
    "F" "🖥️ fullscreen"
    "h" "🎯 hl-line"
    ;; Spec 08: imenu-list on i, indent-guide on I
    "i" "🗂️ imenu-list"
    "I" "📐 indent"
    "l" "🔢 line-nums"
    "r" "🔒 read-only"
    "s" "📝 spell"
    "t" "🎨 theme"
    "v" "👁️ visible"
    "w" "↩️ word-wrap"
    "z" "🧘 zen"
    "~" "🔭 explore")

  ;; --- Windows ---
  (which-key-add-keymap-based-replacements bw/leader-w-map
    "+" "📈 taller"
    "-" "📉 shorter"
    "<" "◀️ narrower"
    ">" "▶️ wider"
    "=" "⚖️ balance"
    "0" "❌ delete"
    "1" "1️⃣ only"
    "2" "➖ split-h"
    "3" "➕ split-v"
    "d" "❌ delete"
    "h" "⬅️ left"
    "j" "⬇️ down"
    "k" "⬆️ up"
    "l" "➡️ right"
    "H" "⬅️ move-left"
    "J" "⬇️ move-down"
    "K" "⬆️ move-up"
    "L" "➡️ move-right"
    "m" "🔲 maximize"
    "o" "🔄 other"
    "q" "❌ quit"
    "r" "↪️ redo"
    "s" "➖ split-h"
    "u" "↩️ undo"
    "v" "➕ split-v"
    ;; Spec 04: vsnake
    "V" "📰 vsnake"
    "w" "🔄 ace"
    "x" "🔀 swap"
    "~" "🔭 explore"))
