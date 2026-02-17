;;; ibuffer.el --- ibuffer config -*- lexical-binding: t; -*-
(require 'ibuffer)
(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))

;; =========================================================================
;; SPC local leader for ibuffer-mode
;; =========================================================================

;; Bind SPC in ibuffer's evil normal state to the ibuffer leader map
(evil-define-key 'normal ibuffer-mode-map (kbd "SPC") 'bw/ibuffer-leader-map)

;; --- Sub-prefix keymaps ---
(define-key bw/ibuffer-leader-map (kbd "*") 'bw/ibuffer-mark-map)
(define-key bw/ibuffer-leader-map (kbd "s") 'bw/ibuffer-sort-map)
(define-key bw/ibuffer-leader-map (kbd "f") 'bw/ibuffer-filter-map)

;; --- Marking ---
(define-key bw/ibuffer-leader-map (kbd "m") 'ibuffer-mark-forward)
(define-key bw/ibuffer-leader-map (kbd "u") 'ibuffer-unmark-forward)
(define-key bw/ibuffer-leader-map (kbd "U") 'ibuffer-unmark-all-marks)
(define-key bw/ibuffer-leader-map (kbd "t") 'ibuffer-toggle-marks)

;; --- Mark by (* prefix) ---
(define-key bw/ibuffer-mark-map (kbd "m") 'ibuffer-mark-by-mode)
(define-key bw/ibuffer-mark-map (kbd "n") 'ibuffer-mark-by-name-regexp)
(define-key bw/ibuffer-mark-map (kbd "f") 'ibuffer-mark-by-file-name-regexp)
(define-key bw/ibuffer-mark-map (kbd "s") 'ibuffer-mark-special-buffers)
(define-key bw/ibuffer-mark-map (kbd "u") 'ibuffer-mark-unsaved-buffers)
(define-key bw/ibuffer-mark-map (kbd "r") 'ibuffer-mark-read-only-buffers)

;; --- Actions on marked ---
(define-key bw/ibuffer-leader-map (kbd "D") 'ibuffer-do-delete)
(define-key bw/ibuffer-leader-map (kbd "S") 'ibuffer-do-save)
(define-key bw/ibuffer-leader-map (kbd "A") 'ibuffer-do-view)
(define-key bw/ibuffer-leader-map (kbd "R") 'ibuffer-do-rename-uniquely)
(define-key bw/ibuffer-leader-map (kbd "Q") 'ibuffer-do-query-replace)
(define-key bw/ibuffer-leader-map (kbd "E") 'ibuffer-do-eval)

;; --- Sorting (s prefix) ---
(define-key bw/ibuffer-sort-map (kbd "a") 'ibuffer-do-sort-by-alphabetic)
(define-key bw/ibuffer-sort-map (kbd "s") 'ibuffer-do-sort-by-size)
(define-key bw/ibuffer-sort-map (kbd "m") 'ibuffer-do-sort-by-major-mode)
(define-key bw/ibuffer-sort-map (kbd "v") 'ibuffer-do-sort-by-recency)
(define-key bw/ibuffer-sort-map (kbd "f") 'ibuffer-do-sort-by-filename/process)

;; --- Filtering (f prefix) ---
(define-key bw/ibuffer-filter-map (kbd "m") 'ibuffer-filter-by-used-mode)
(define-key bw/ibuffer-filter-map (kbd "n") 'ibuffer-filter-by-name)
(define-key bw/ibuffer-filter-map (kbd "f") 'ibuffer-filter-by-filename)
(define-key bw/ibuffer-filter-map (kbd "/") 'ibuffer-filter-disable)

;; --- View/Navigation ---
(define-key bw/ibuffer-leader-map (kbd "RET") 'ibuffer-visit-buffer)
(define-key bw/ibuffer-leader-map (kbd "o") 'ibuffer-visit-buffer-other-window)
(define-key bw/ibuffer-leader-map (kbd "g") 'ibuffer-update)

;; =========================================================================
;; which-key labels for ibuffer leader
;; =========================================================================
(with-eval-after-load 'which-key
  ;; --- Main ibuffer leader ---
  (which-key-add-keymap-based-replacements bw/ibuffer-leader-map
    "m" "📍 mark"
    "u" "📍 unmark"
    "U" "📍 unmark all"
    "t" "📍 toggle"
    "*" "📍+mark by..."
    "D" "❌ delete marked"
    "S" "💾 save marked"
    "A" "👁️ view marked"
    "R" "✏️ rename marked"
    "Q" "🔄 query-replace"
    "E" "⚡ eval in marked"
    "s" "🔃+sort"
    "f" "🔍+filter"
    "RET" "👁️ open"
    "o" "👁️ open other"
    "g" "🔄 refresh")

  ;; --- Mark by (* prefix) ---
  (which-key-add-keymap-based-replacements bw/ibuffer-mark-map
    "m" "📍 by mode"
    "n" "📍 by name"
    "f" "📍 by file"
    "s" "📍 special"
    "u" "📍 unsaved"
    "r" "📍 read-only")

  ;; --- Sort (s prefix) ---
  (which-key-add-keymap-based-replacements bw/ibuffer-sort-map
    "a" "🔃 alphabetic"
    "s" "🔃 size"
    "m" "🔃 major-mode"
    "v" "🔃 recency"
    "f" "🔃 filename")

  ;; --- Filter (f prefix) ---
  (which-key-add-keymap-based-replacements bw/ibuffer-filter-map
    "m" "🔍 by mode"
    "n" "🔍 by name"
    "f" "🔍 by file"
    "/" "🔍 clear filters"))
