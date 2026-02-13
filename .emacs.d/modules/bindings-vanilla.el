;;; bindings-vanilla.el --- bw-vanilla C-c keybindings -*- lexical-binding: t; -*-
;;
;; C-c style keybindings for the bw-vanilla profile.

;; C-c bindings
(global-set-key (kbd "C-c i") 'consult-imenu)
(global-set-key (kbd "C-c l") 'consult-line)
(global-set-key (kbd "C-c r") 'consult-ripgrep)
(global-set-key (kbd "C-c f") 'project-find-file)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c d") 'devdocs-lookup)
(global-set-key (kbd "C-c w") 'eww)
(global-set-key (kbd "C-c W") 'bw/docs-home)
(global-set-key (kbd "C-c R") 'bw/reload-init)
(global-set-key (kbd "C-c Q") 'bw/restart-emacs)
(global-set-key (kbd "C-c h") 'bw/open-cheatsheet)
(global-set-key (kbd "C-c p") 'bw/open-palette)
(global-set-key (kbd "C-c y") 'bw/duplicate-line)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c n") 'org-capture)
(global-set-key (kbd "C-c e e") 'bw/edit-init)
(global-set-key (kbd "C-c e d") 'bw/dired-init)
(global-set-key (kbd "C-c t w") 'bw/toggle-word-wrap)
(global-set-key (kbd "C-c c c") 'bw/khal-calendar)
(global-set-key (kbd "C-c c a") 'bw/khal-agenda)
(global-set-key (kbd "C-c c s") 'bw/calendar-sync)

;; which-key labels
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c i" "📍 imenu"
    "C-c l" "🔍 line search"
    "C-c r" "🔎 ripgrep"
    "C-c f" "📂 find file"
    "C-c g" "🌿 magit"
    "C-c d" "📖 devdocs"
    "C-c w" "🌐 eww"
    "C-c W" "🏠 docs home"
    "C-c R" "🔄 reload init"
    "C-c Q" "🔁 restart"
    "C-c h" "📋 cheatsheet"
    "C-c p" "🎨 palette"
    "C-c y" "📑 duplicate"
    "C-c a" "📅 agenda"
    "C-c n" "✏️ capture"
    "C-c e" "⚙️ emacs"
    "C-c t" "🔀 toggles"
    "C-c c" "📆 calendar"))
