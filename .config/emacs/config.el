;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; Hide UI bars for a cleaner, minimalist look
(menu-bar-mode -1)   ; Hide menu bar (File, Edit, etc.)
(tool-bar-mode -1)   ; Hide tool bar (icons)
(scroll-bar-mode -1) ; Hide scroll bar

;; Remove Windows title bar (frameless window like WezTerm)
;; This removes the title bar, minimize/maximize buttons, and external borders
(add-to-list 'default-frame-alist '(undecorated . t))
;; Allow resizing by dragging internal borders
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
;; Allow moving the frame by dragging the mode line
(add-to-list 'default-frame-alist '(drag-with-mode-line . t))

;; Save and restore frame geometry
;; Use desktop-save-mode to automatically save/restore frame size and position
(desktop-save-mode 1)
(setq desktop-restore-frames t)
(setq desktop-restore-forces-onscreen nil)  ; Allow frames to be positioned off-screen initially

;; Function to manually save current frame geometry
(defun my/save-frame-geometry ()
  "Save current frame geometry to default-frame-alist and desktop file."
  (interactive)
  (let ((frame (selected-frame)))
    (let ((width (frame-width frame))
          (height (frame-height frame))
          (top (frame-parameter frame 'top))
          (left (frame-parameter frame 'left)))
      ;; Remove existing geometry entries first
      (setq default-frame-alist
            (assq-delete-all 'width
              (assq-delete-all 'height
                (assq-delete-all 'top
                  (assq-delete-all 'left default-frame-alist)))))
      ;; Add new geometry
      (add-to-list 'default-frame-alist `(width . ,width))
      (add-to-list 'default-frame-alist `(height . ,height))
      (add-to-list 'default-frame-alist `(top . ,top))
      (add-to-list 'default-frame-alist `(left . ,left))
      (add-to-list 'default-frame-alist '(user-position . t))  ; Tell WM to respect position
      ;; Save to desktop file
      (desktop-save-in-desktop-dir)
      (message "Frame geometry saved: %dx%d at (%d, %d). Restart Emacs to apply." width height left top))))

;; Alternative: Set geometry manually by uncommenting and adjusting these values:
;; (add-to-list 'default-frame-alist '(width . 120))   ; columns
;; (add-to-list 'default-frame-alist '(height . 40))   ; rows
;; (add-to-list 'default-frame-alist '(top . 50))      ; pixels from top
;; (add-to-list 'default-frame-alist '(left . 50))     ; pixels from left
;; (add-to-list 'default-frame-alist '(user-position . t))

;; Set font to JetBrains Mono Nerd Font
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12))

;; Set Fira Code as fallback font for ligatures and additional Unicode support
;; The unicode module will use this for Hebrew characters and other Unicode blocks
;; Ligatures module will use it for better ligature rendering
(setq doom-symbol-font (font-spec :family "Fira Code" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; Default theme - will be synced with Omarchy via theme-set hook
(setq doom-theme 'doom-tokyo-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")




;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Send files to trash instead of fully deleting
(setq delete-by-moving-to-trash t)
;; Save automatically
(setq auto-save-default t)
;; Remove "Really exit Emacs?" confirmation dialog
(setq confirm-kill-emacs nil)

;; Performance optimizations
;; Note: Doom already sets some of these, but these values are safe overrides
(setq gc-cons-threshold (* 256 1024 1024))  ; 256MB - reduces GC frequency
(setq read-process-output-max (* 4 1024 1024))  ; 4MB - improves async process performance
(setq comp-deferred-compilation t)  ; Enable deferred native compilation
(setq comp-async-jobs-number 8)  ; Parallel native compilation jobs

;; Garbage collector optimization (gcmh is already enabled by Doom)
(setq gcmh-idle-delay 5)  ; Run GC after 5 seconds of idle time
(setq gcmh-high-cons-threshold (* 1024 1024 1024))  ; 1GB - only GC when memory usage is high

;; Version control optimization (limit to Git only for faster operations)
(setq vc-handled-backends '(Git))

;; Speed of which-key popup
(setq which-key-idle-delay 0.2)




;; Completion mechanisms
(setq completing-read-function #'completing-read-default)
(setq read-file-name-function #'read-file-name-default)
;; Makes path completion more like find-file everywhere
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
;; Use the familiar C-x C-f interface for directory completion
(map! :map minibuffer-mode-map
      :when (featurep! :completion vertico)
      "C-x C-f" #'find-file)

;; Save minibuffer history - enables command history in M-x
(use-package! savehist
  :config
  (setq savehist-file (concat doom-cache-dir "savehist")
        savehist-save-minibuffer-history t
        history-length 1000
        history-delete-duplicates t
        savehist-additional-variables '(search-ring
                                        regexp-search-ring
                                        extended-command-history))
  (savehist-mode 1))

(after! vertico
  ;; Add file preview
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  ;; Make vertico use a more minimal display
  (setq vertico-count 17
        vertico-cycle t
        vertico-resize t)
  ;; Enable alternative filter methods
  (setq vertico-sort-function #'vertico-sort-history-alpha)
  ;; Quick actions keybindings
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (define-key vertico-map (kbd "M-RET") #'vertico-exit-input)

  ;; History navigation
  (define-key vertico-map (kbd "M-p") #'vertico-previous-history)
  (define-key vertico-map (kbd "M-n") #'vertico-next-history)
  (define-key vertico-map (kbd "C-r") #'consult-history)

  ;; Configure orderless for better filtering
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion orderless))))

  ;; Customize orderless behavior
  (setq orderless-component-separator #'orderless-escapable-split-on-space
        orderless-matching-styles '(orderless-literal
                                    orderless-prefixes
                                    orderless-initialism
                                    orderless-flex
                                    orderless-regexp)))

;; Quick command repetition
(use-package! vertico-repeat
  :after vertico
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (map! :leader
        (:prefix "r"
         :desc "Repeat completion" "v" #'vertico-repeat)))

;; TODO Not currently working
;; Enhanced sorting and filtering with prescient
;; (use-package! vertico-prescient
;;   :after vertico
;;   :config
;;   (vertico-prescient-mode 1)
;;   (prescient-persist-mode 1)
;;   (setq prescient-sort-length-enable nil
;;         prescient-filter-method '(literal regexp initialism fuzzy)))

;; Enhanced marginalia annotations
(after! marginalia
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  ;; Show more details in marginalia
  (setq marginalia-max-relative-age 0
        marginalia-align 'right))

;; Corrected Embark configuration
(map! :leader
      (:prefix ("k" . "embark")  ;; Using 'k' prefix instead of 'e' which conflicts with elfeed
       :desc "Embark act" "a" #'embark-act
       :desc "Embark dwim" "d" #'embark-dwim
       :desc "Embark collect" "c" #'embark-collect))

;; Configure consult for better previews
(after! consult
  (setq consult-preview-key "M-."
        consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip"
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  ;; More useful previews for different commands
  (consult-customize
   consult-theme consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any)))

;; Enhanced directory navigation
(use-package! consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map vertico-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

;; Add additional useful shortcuts
(map! :leader
      (:prefix "s"
       :desc "Command history" "h" #'consult-history
       :desc "Recent directories" "d" #'consult-dir))

;; Saving with C-s
(map! "C-s" #'save-buffer)

;; Open workflow guide
(map! :leader
      (:prefix "f"
       :desc "Open workflow guide" "w" (lambda () (interactive)
                                         (find-file (expand-file-name "WORKFLOW-GUIDE.md" doom-user-dir)))))

;; Zoom in/out
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Line wrapping
(global-visual-line-mode t)

;; ============================================================================
;; Org Mode Configuration
;; ============================================================================

;; Enable org-habit module for habit tracking
(use-package org
  :ensure nil
  :custom (org-modules '(org-habit)))

(after! org
  ;; Keybindings for org mode
  (map! :map org-mode-map
        :n "<M-left>" #'org-do-promote
        :n "<M-right>" #'org-do-demote)
  
  ;; Auto-clock in when state changes to STRT (starting)
  (defun my/org-clock-in-if-starting ()
    "Clock in when the task state changes to STRT"
    (when (and (string= org-state "STRT")
               (not (org-clock-is-active)))
      (org-clock-in)))
  
  ;; Auto-clock out when leaving STRT state
  (defun my/org-clock-out-if-not-starting ()
    "Clock out when leaving STRT state"
    (when (and (org-clock-is-active)
               (not (string= org-state "STRT")))
      (org-clock-out)))
  
  ;; Add hooks for auto clocking
  (add-hook 'org-after-todo-state-change-hook 'my/org-clock-in-if-starting)
  (add-hook 'org-after-todo-state-change-hook 'my/org-clock-out-if-not-starting)
  
  ;; Show habits in agenda
  (setq org-habit-show-all-today t)
  (setq org-habit-graph-column 1)
  
  ;; Agenda view improvements
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (visual-line-mode -1)
              (setq truncate-lines 1)))
  
  ;; Prevent clock from stopping when marking subtasks as done
  (setq org-clock-out-when-done nil)
  
  ;; Additional keybindings
  (define-key org-mode-map (kbd "C-c e") #'org-set-effort)
  (define-key org-mode-map (kbd "C-c i") #'org-clock-in)
  (define-key org-mode-map (kbd "C-c o") #'org-clock-out))

;; Org Agenda - Custom dashboard view
(setq org-agenda-remove-tags t)
(setq org-agenda-block-separator 32)  ; Space character
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "\n HIGHEST PRIORITY")
                 (org-agenda-prefix-format "   %i %?-2 t%s")))
          (agenda ""
                  ((org-agenda-start-day "+0d")
                   (org-agenda-span 1)
                   (org-agenda-time)
                   (org-agenda-remove-tags t)
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-scheduled-leaders '("" ""))
                   (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈┈┈ NOW")
                   (org-agenda-overriding-header "\n TODAY'S SCHEDULE")
                   (org-agenda-prefix-format "   %i %?-2 t%s")))
          (tags-todo "-STYLE=\"habit\""
                     ((org-agenda-overriding-header "\n ALL TODO")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "   %i %?-2 t%s")))))))

;; Remove scheduled tag from agenda display
(setq org-agenda-scheduled-leaders '("" ""))
;; Remove holidays from agenda
(setq org-agenda-include-diary nil)

;; Org Capture Templates - Quick capture for different types of items
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/org/inbox.org" "Inbox")
         "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        
        ("e" "Event" entry
         (file+headline "~/org/calendar.org" "Events")
         "* %^{Event}\n%^{SCHEDULED}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        
        ("d" "Deadline" entry
         (file+headline "~/org/calendar.org" "Deadlines")
         "* TODO %^{Task}\nDEADLINE: %^{Deadline}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        
        ("p" "Project" entry
         (file+headline "~/org/projects.org" "Projects")
         "* PROJ %^{Project name}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n** TODO %?")
        
        ("i" "Idea" entry
         (file+headline "~/org/ideas.org" "Ideas")
         "** IDEA %^{Idea}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        
        ("b" "Bookmark" entry
         (file+headline "~/org/bookmarks.org" "Inbox")
         "** [[%^{URL}][%^{Title}]]\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
         :empty-lines 0)
        
        ("n" "Note" entry
         (file+headline "~/org/notes.org" "Inbox")
         "* [%<%Y-%m-%d %a>] %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?"
         :prepend t)))

;; Custom function for inserting images into org files
(defun my/org-insert-image ()
  "Select and insert an image into org file."
  (interactive)
  (let ((selected-file (read-file-name "Select image: " "~/Pictures/" nil t)))
    (when selected-file
      (insert (format "[[file:%s]]\n" selected-file))
      (org-display-inline-images))))

;; Bind image insertion to C-c C-i in org mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-i") #'my/org-insert-image))

;; Org Babel - Execute code blocks in org files
(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))  ; Enable shell execution (add more languages as needed)
  
  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window))

;; ============================================================================
;; Window Navigation - Use C-hjkl instead of arrow keys
;; ============================================================================

;; Navigate between window splits using C-hjkl (vim-style)
(map! :map general-override-mode-map
      "C-h" #'evil-window-left
      "C-j" #'evil-window-down
      "C-k" #'evil-window-up
      "C-l" #'evil-window-right
      ;; Window resizing with Shift
      "S-<right>" (lambda () (interactive)
                    (if (window-in-direction 'left)
                        (evil-window-decrease-width 5)
                      (evil-window-increase-width 5)))
      "S-<left>"  (lambda () (interactive)
                    (if (window-in-direction 'right)
                        (evil-window-decrease-width 5)
                      (evil-window-increase-width 5)))
      "S-<up>"    (lambda () (interactive)
                    (if (window-in-direction 'below)
                        (evil-window-decrease-height 2)
                      (evil-window-increase-height 2)))
      "S-<down>"  (lambda () (interactive)
                    (if (window-in-direction 'above)
                        (evil-window-decrease-height 2)
                      (evil-window-increase-height 2))))

;; ============================================================================
;; Harper Grammar Checker Setup
;; ============================================================================

;; Harper requires harper-ls to be installed and in PATH
;; Download from: https://writewithharper.com/docs/integrations/emacs
;; Then install harper-ls binary to your PATH

(after! eglot
  ;; Add harper-ls for text-based modes (org, markdown, text)
  (add-to-list 'eglot-server-programs
               '(text-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(org-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("harper-ls" "--stdio"))))

;; Enable eglot (and thus Harper) in text-based modes
(add-hook 'text-mode-hook 'eglot-ensure)
(add-hook 'org-mode-hook 'eglot-ensure)
(add-hook 'markdown-mode-hook 'eglot-ensure)

;; Optional: Configure Harper settings
;; (setq-default eglot-workspace-configuration
;;               '(:harper-ls (:dialect "American"
;;                              :diagnosticSeverity "hint"
;;                              :linters (:SpellCheck t
;;                                        :AnA t
;;                                        :SentenceCapitalization t
;;                                        :LongSentences t
;;                                        :RepeatedWords t))))

;; ============================================================================
;; Dictionary, Thesaurus, and Etymology Lookup
;; ============================================================================

;; Doom's lookup module already provides dictionary/thesaurus support
;; Enable +dictionary flag in init.el: (lookup +dictionary)
;; Then you can use:
;; - SPC s d t - Dictionary lookup
;; - SPC s d T - Thesaurus lookup

;; Additional packages for enhanced lookup (if needed)
;; Power Thesaurus is already included with +dictionary flag
;; For etymology, you can add define-word package

(use-package! define-word
  :defer t
  :config
  ;; Use online dictionary for definitions (includes etymology info)
  (setq define-word-displayfn-alist
        (cl-loop for (service . _) in define-word-services
                 collect (cons service #'+eval-display-results-in-popup))))

;; Custom keybindings for lookup (if not already bound)
(map! :leader
      (:prefix "s"
       :desc "Dictionary lookup" "d" #'+lookup/dictionary-definition
       :desc "Thesaurus lookup" "D" #'+lookup/synonyms))

;; ============================================================================
;; EWW Browser Configuration
;; ============================================================================

;; Make EWW open in full buffer (sane default for reading)
(after! eww
  (set-popup-rule! "^\\*eww\\*" :ignore t))

;; ============================================================================
;; Company vs Corfu
;; ============================================================================

;; Company is an older completion backend (you have corfu enabled instead)
;; Company provides popup-based completion, while Corfu provides inline completion
;; You're using Corfu which is more modern and integrated with Vertico
;; No need to change - Corfu is better for your setup