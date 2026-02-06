;;; which-key-posframe.el — sectioned which-key with posframe -*- lexical-binding: t; -*-

;; ═══════════════════════════════════════════════════════════════════
;; Configuration
;; ═══════════════════════════════════════════════════════════════════

(use-package which-key
  :demand t
  :config
  (unless which-key-mode (which-key-mode 1))
  (setq which-key-idle-delay 0.03
        which-key-idle-secondary-delay 0.03
        which-key-separator ":"
        which-key-prefix-prefix ""
        which-key-add-column-padding 2
        which-key-show-early-on-C-h t
        echo-keystrokes 0.01)

  ;; Semantic sort: group by category emoji instead of alphabetical
  (defvar bw/which-key-category-priorities
    '(("🚀" . 5)  ("📊" . 10) ("🔍" . 15) ("📄" . 18) ("📁" . 19)
      ("🧭" . 20) ("📂" . 25) ("📅" . 28) ("✏" . 30)  ("✨" . 32)
      ("📐" . 35) ("✂" . 40)  ("🔀" . 45) ("💾" . 50) ("📦" . 55)
      ("⏳" . 60) ("🤖" . 63) ("🐚" . 65) ("🌐" . 67) ("🔧" . 70)
      ("🎨" . 72) ("📖" . 75) ("🔖" . 77) ("📋" . 78) ("📧" . 80)
      ("🔭" . 99))
    "Priority ordering for category emojis in which-key descriptions.")

  (defun bw/which-key--description-sort-priority (description)
    "Extract sort priority from a which-key DESCRIPTION string."
    (cond
     ((null description) 50)
     ((string-match-p "\\+" description) 0)
     (t (catch 'found
          (dolist (category-entry bw/which-key-category-priorities)
            (when (string-prefix-p (car category-entry) description)
              (throw 'found (cdr category-entry))))
          50))))

  (defun bw/which-key-sort-by-category-then-alpha (entry-a entry-b)
    "Sort which-key entries by category emoji priority, then alphabetically."
    (let ((priority-a (bw/which-key--description-sort-priority (cdr entry-a)))
          (priority-b (bw/which-key--description-sort-priority (cdr entry-b))))
      (if (= priority-a priority-b)
          (which-key-key-order-alpha entry-a entry-b)
        (< priority-a priority-b))))

  (setq which-key-sort-order #'bw/which-key-sort-by-category-then-alpha))

(use-package posframe
  :demand t)

(use-package which-key-posframe
  :demand t
  :after (which-key posframe)
  :config
  (setq which-key-max-display-columns 5
        which-key-max-description-length 35)

  (set-face-attribute 'which-key-posframe nil :background "#21242b")
  (set-face-attribute 'which-key-posframe-border nil :background "#51afef")

  (defvar bw/which-key-vertical-separator
    (propertize " │ " 'face '(:foreground "#3f444a"))
    "Vertical separator between side-by-side sections.")

  ;; ═══════════════════════════════════════════════════════════════════
  ;; Data — section definitions, priority tables, prefix mapping
  ;; ═══════════════════════════════════════════════════════════════════

  ;; Section definitions: keymap symbol → list of (HEADER . (KEY ...))
  ;; Use :break to force a new row of sections.
  (defvar bw/which-key-section-definitions (make-hash-table :test 'eq)
    "Section groupings per keymap for sectioned which-key rendering.")

  (puthash 'bw/leader-w-map
    '(("📐 Resize"   . ("+" "-" "<" ">" "="))
      ("✂️ Split"     . ("2" "3" "s" "v"))
      ("🧭 Navigate"  . ("h" "j" "k" "l" "o" "w"))
      ("🔀 Move"      . ("H" "J" "K" "L" "x"))
      :break
      ("📦 Manage"    . ("0" "1" "d" "m" "q"))
      ("⏳ History"   . ("u" "r"))
      ("🔧 Special"   . ("V" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-b-map
    '(("🔄 Switch"  . ("b" "l" "n" "p"))
      ("✨ Create"  . ("N" "x" "e"))
      ("💾 Save"    . ("s"))
      ("📦 Manage"  . ("d" "k" "r" "R" "z"))
      ("📋 Info"    . ("i" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-g-map
    '(("📊 Status"  . ("g" "d" "l" "b"))
      ("✏️ Stage"   . ("s" "S"))
      ("💾 Commit"  . ("c" "t"))
      ("🔀 Remote"  . ("f" "F" "p" "r"))
      ("📦 Setup"   . ("C" "i" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-c-map
    '(("🔍 Navigate" . ("d" "D" "h" "H" "o"))
      ("✏️ Edit"     . ("a" "f" "r" "w" "y"))
      ("⚠️ Diag"     . ("x"))
      ("📖 Docs"     . ("p" "P" "k" "m" "M" "W"))
      ("🔧 Tools"    . ("t" "T" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-f-map
    '(("📄 Open"    . ("f" "r" "p" "e" "E"))
      ("💾 Save"    . ("s" "S"))
      ("📦 Manage"  . ("c" "D" "R" "y" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-s-map
    '(("📄 Buffer"   . ("l" "s" "S" "o" "i"))
      ("📁 Project"  . ("I" "b" "d" "f" "g" "p"))
      ("🔖 Index"    . ("m" "r" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-o-map
    '(("📁 Files"    . ("-" "d" "F" "T" "." "D"))
      ("📅 Plan"     . ("a" "A" "c" "C" "n"))
      ("🐚 Shell"    . ("e" "s" "t"))
      :break
      ("🔍 Find"     . ("b" "B" "f" "i" "r" "P"))
      ("🤖 Apps"     . ("g" "o" "m" "w" "S"))
      ("📖 Docs"     . ("M" "h" "p" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-t-map
    '(("🎨 Display"  . ("b" "c" "F" "h" "l" "v" "w" "t" "z"))
      ("🔧 Tools"    . ("f" "i" "I" "r" "s" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-h-map
    '(("🔍 Describe" . ("f" "v" "k" "m" "x" "o" "c" "F" "a" "w"))
      ("📖 Browse"   . ("b" "i" "d" "l" "M" "p" "P" "e" "t"))
      ("🔧 Reload"   . ("r" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-p-map
    '(("📁 Files"    . ("f" "F" "d" "b"))
      ("🔍 Search"   . ("s" "g" "r"))
      ("🚀 Run"      . ("!" "&" "e" "c" "x"))
      ("📦 Manage"   . ("k" "p" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-q-map
    '(("💾 Save"     . ("q" "K"))
      ("⚡ Quick"    . ("Q" "f"))
      ("🔄 Restart"  . ("r" "R" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-map
    '(("🚀 Launch"    . ("SPC" "." "," "`" "x"))
      ("⚡ Execute"   . ("!" "&" ":" ";"))
      ("🔍 Search"    . ("/" "*"))
      ("🔧 Utility"   . ("'" "u" "~"))
      :break
      ("📂 Files"     . ("f" "p" "g"))
      ("💻 Code"      . ("c" "s" "i"))
      ("🪟 Interface" . ("w" "b" "t"))
      ("📖 Open"      . ("o" "a" "n" "h" "q" "v")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-a-map
    '(("📰 Apps" . ("r" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-i-map
    '(("✏️ Markup"  . ("t" "S" "d"))
      ("📋 Yank"    . ("r" "y" "s"))
      ("🔤 Special" . ("e" "u" "f" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-n-map
    '(("📝 Notes" . ("a" "c" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-hr-map
    '(("🔄 Reload" . ("R" "t" "r")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-oh-map
    '(("📖 Docs" . ("c" "d" "h" "H")))
    bw/which-key-section-definitions)

  ;; Map prefix key descriptions to keymap symbols (setq for reload safety)
  (setq bw/which-key-prefix-to-keymap-alist
    '(("SPC" . bw/leader-map)
      ("SPC w" . bw/leader-w-map) ("SPC b" . bw/leader-b-map)
      ("SPC g" . bw/leader-g-map) ("SPC c" . bw/leader-c-map)
      ("SPC f" . bw/leader-f-map) ("SPC s" . bw/leader-s-map)
      ("SPC o" . bw/leader-o-map) ("SPC t" . bw/leader-t-map)
      ("SPC h" . bw/leader-h-map) ("SPC p" . bw/leader-p-map)
      ("SPC q" . bw/leader-q-map) ("SPC a" . bw/leader-a-map)
      ("SPC i" . bw/leader-i-map) ("SPC n" . bw/leader-n-map)
      ("SPC h r" . bw/leader-hr-map) ("SPC o h" . bw/leader-oh-map)))

  ;; ═══════════════════════════════════════════════════════════════════
  ;; Rendering Helpers — pure functions, no side effects
  ;; ═══════════════════════════════════════════════════════════════════

  (defun bw/which-key--find-keymap-symbol (keymap)
    "Find the symbol name for KEYMAP by checking known leader maps."
    (catch 'found
      (dolist (keymap-symbol '(bw/leader-map bw/leader-w-map bw/leader-b-map
                               bw/leader-c-map bw/leader-f-map bw/leader-g-map
                               bw/leader-h-map bw/leader-hr-map bw/leader-i-map
                               bw/leader-n-map bw/leader-o-map bw/leader-oh-map
                               bw/leader-p-map bw/leader-q-map bw/leader-s-map
                               bw/leader-t-map bw/leader-v-map))
        (when (and (boundp keymap-symbol) (eq (symbol-value keymap-symbol) keymap))
          (throw 'found keymap-symbol)))
      nil))

  (defun bw/which-key--clean-description (description)
    "Strip \"group:\" prefix from which-key DESCRIPTION if present."
    (if (and description (string-prefix-p "group:" description))
        (substring description 6)
      (or description "")))

  (defun bw/which-key--entry-visible-width (key description)
    "Calculate visible width of a formatted entry with KEY and DESCRIPTION."
    (+ 1 (string-width key) 1 (string-width (bw/which-key--clean-description description)) 1))

  (defun bw/which-key--format-entry-clipped (key description pixel-budget)
    "Format KEY:DESCRIPTION entry, clipping description to fit PIXEL-BUDGET.
Uses `string-pixel-width' to measure actual rendered width, so emoji
characters that overflow their `string-width' are handled correctly.
Strips \"group:\" prefix from sub-keymap descriptions."
    (let* ((description (bw/which-key--clean-description description))
           (key-propertized (propertize key 'face '(:foreground "#51afef")))
           (full (concat " " key-propertized ":" description)))
      (if (<= (string-pixel-width full) pixel-budget)
          full
        ;; Clip description until it fits
        (let ((desc description))
          (while (and (> (length desc) 0)
                      (> (string-pixel-width
                          (concat " " key-propertized ":" desc))
                         pixel-budget))
            (setq desc (substring desc 0 -1)))
          (concat " " key-propertized ":" desc)))))

  (defun bw/which-key--centered-header (section-name total-width)
    "Create a centered header string for SECTION-NAME within TOTAL-WIDTH chars."
    (let* ((header-text (concat " " section-name " "))
           (header-text-width (string-width header-text))
           (fill-total-width (max 0 (- total-width header-text-width)))
           (left-fill-width (/ fill-total-width 2))
           (right-fill-width (- fill-total-width left-fill-width)))
      (propertize (concat (make-string left-fill-width ?─)
                          header-text
                          (make-string right-fill-width ?─))
                  'face '(:foreground "#5B6268"))))

  (defun bw/which-key--optimal-column-count (entries available-width)
    "Determine optimal number of columns for ENTRIES within AVAILABLE-WIDTH."
    (let ((widest-entry-width 0))
      (dolist (entry entries)
        (setq widest-entry-width
              (max widest-entry-width
                   (bw/which-key--entry-visible-width (car entry) (cdr entry)))))
      (max 1 (min (length entries) 5 (/ available-width (+ widest-entry-width 2))))))

  (defun bw/which-key--split-sections-into-bands (sections)
    "Split SECTIONS list into bands, breaking on :break markers."
    (let ((accumulated-bands '())
          (current-band '()))
      (dolist (section-or-break sections)
        (if (eq section-or-break :break)
            (when current-band
              (push (nreverse current-band) accumulated-bands)
              (setq current-band nil))
          (push section-or-break current-band)))
      (when current-band
        (push (nreverse current-band) accumulated-bands))
      (nreverse accumulated-bands)))

  ;; ═══════════════════════════════════════════════════════════════════
  ;; Rendering Engine — functions that build buffer content
  ;; ═══════════════════════════════════════════════════════════════════

  (defun bw/which-key--insert-entries-block (entries column-width column-count &optional left-offset)
    "Insert ENTRIES in COLUMN-COUNT columns, each COLUMN-WIDTH wide.
Uses :align-to display properties to pin each column start and
pixel-clips entries to prevent overflow.  LEFT-OFFSET shifts all
columns right by that many character columns."
    (let* ((row-count (ceiling (length entries) column-count))
           (offset (or left-offset 0))
           (col-pixel-budget (* column-width (default-font-width))))
      (dotimes (row row-count)
        (dotimes (column column-count)
          (let ((entry-index (+ (* column row-count) row))
                (abs-col (+ offset (* column column-width))))
            ;; Pin cursor to absolute column position
            (when (or (> column 0) (> offset 0))
              (insert (propertize " " 'display `(space :align-to ,abs-col))))
            (when (< entry-index (length entries))
              (let ((entry (nth entry-index entries)))
                (insert (bw/which-key--format-entry-clipped
                         (car entry) (cdr entry) col-pixel-budget))))))
        (insert "\n"))))

  (defun bw/which-key--render-single-section-band (section popup-width margin-chars)
    "Render a single-section band with centered header and tight columns.
MARGIN-CHARS shifts content right to center within the posframe."
    (let* ((section-name (car section))
           (entries (cdr section))
           (column-count (bw/which-key--optimal-column-count entries popup-width))
           (column-width (/ popup-width column-count)))
      (when (> margin-chars 0)
        (insert (propertize " " 'display `(space :align-to ,margin-chars))))
      (insert (bw/which-key--centered-header section-name popup-width) "\n")
      (bw/which-key--insert-entries-block entries column-width column-count margin-chars)))

  (defun bw/which-key--render-multi-section-band (sections popup-width margin-chars skip-last-row-separators)
    "Render sections side by side with pixel-perfect centered entry groups.
Each section's entries form a tight column cluster centered under its header.
All positions use pixel-based :align-to — (space :align-to (PX)) — so emoji
width discrepancies cannot accumulate.  MARGIN-CHARS shifts all content right
to center within the posframe.  When SKIP-LAST-ROW-SEPARATORS is non-nil,
omit vertical separators on the final entry row to prevent the 'impaling'
effect on headers of subsequent bands."
    (let* ((char-px (default-font-width))
           (section-count (length sections))
           (separator-width (string-width bw/which-key-vertical-separator))
           (total-separator-width (* separator-width (1- section-count)))
           ;; Section widths in chars (with remainder distribution)
           (available-chars (- popup-width total-separator-width))
           (base-sw (/ available-chars section-count))
           (sw-remainder (% available-chars section-count))
           (section-widths
            (cl-loop for i from 0 below section-count
                     collect (if (< i sw-remainder) (1+ base-sw) base-sw)))
           ;; Section starts in chars — offset by margin
           (section-starts
            (let ((pos margin-chars))
              (cl-loop for i from 0 below section-count
                       collect (prog1 pos
                                 (setq pos (+ pos (nth i section-widths)
                                               separator-width))))))
           (section-starts-px (mapcar (lambda (c) (* c char-px)) section-starts))
           (section-widths-px (mapcar (lambda (c) (* c char-px)) section-widths))
           ;; Separator positions in pixels
           (separator-positions-px
            (cl-loop for i from 0 below (1- section-count)
                     collect (+ (nth i section-starts-px) (nth i section-widths-px))))
           (max-row-count 0)
           (gap-px char-px)
           ;; Per-section layout: always try 2 columns, clip if natural widths overflow
           (section-layouts
            (cl-loop
             for section in sections
             for sw-px in section-widths-px
             collect
             (let* ((entries (cdr section))
                    (n (length entries))
                    (try-cols (min 2 n))
                    (try-rows (ceiling n try-cols))
                    ;; Measure natural pixel width per physical column
                    (col-pxs-2
                     (when (>= try-cols 2)
                       (cl-loop for c from 0 below try-cols
                                collect
                                (cl-loop for r from 0 below try-rows
                                         maximize
                                         (let ((idx (+ (* c try-rows) r)))
                                           (if (< idx n)
                                               (let ((e (nth idx entries)))
                                                 (string-pixel-width
                                                  (concat " " (car e) ":" (bw/which-key--clean-description (cdr e)))))
                                             0))))))
                    (group-px-2
                     (when col-pxs-2
                       (+ (apply #'+ col-pxs-2) gap-px)))
                    ;; Always use 2 columns when possible
                    (use-2 (>= try-cols 2))
                    (col-count (if use-2 try-cols 1))
                    (row-count (ceiling n col-count))
                    ;; Natural widths if they fit; equal budgets with clipping otherwise
                    (natural-fits (and group-px-2 (<= group-px-2 sw-px)))
                    (col-pxs
                     (cond
                      ((and use-2 natural-fits) col-pxs-2)
                      (use-2
                       (let ((col-budget (/ (- sw-px gap-px) col-count)))
                         (make-list col-count col-budget)))
                      (t (list (cl-loop for e in entries
                                        maximize (string-pixel-width
                                                  (concat " " (car e) ":" (bw/which-key--clean-description (cdr e)))))))))
                    (group-px (+ (apply #'+ col-pxs)
                                 (* gap-px (max 0 (1- col-count))))))
               (list :entries entries :col-count col-count
                     :row-count row-count :col-pxs col-pxs
                     :group-px group-px)))))
      ;; Max rows across all sections
      (dolist (layout section-layouts)
        (setq max-row-count (max max-row-count (plist-get layout :row-count))))
      ;; Header row — no separators between headers, just positioned with gaps
      (cl-loop for section in sections
               for si from 0
               for sw in section-widths
               do (progn
                    (insert (propertize " " 'display
                                        `(space :align-to (,(nth si section-starts-px)))))
                    (insert (bw/which-key--centered-header (car section) sw))))
      (insert "\n")
      ;; Entry rows — tight centered groups with pixel-based :align-to
      (dotimes (row max-row-count)
        (let ((is-last-row (= row (1- max-row-count))))
          (cl-loop for section in sections
                   for si from 0
                   for sw-px in section-widths-px
                   for layout in section-layouts
                   do (let* ((entries (plist-get layout :entries))
                             (col-count (plist-get layout :col-count))
                             (row-count (plist-get layout :row-count))
                             (col-pxs (plist-get layout :col-pxs))
                             (group-px (plist-get layout :group-px))
                             (section-start-px (nth si section-starts-px))
                             ;; Center the tight group within the section
                             (center-offset-px (max 0 (/ (- sw-px group-px) 2)))
                             (group-start-px (+ section-start-px center-offset-px)))
                        ;; Render sub-columns at pixel-precise positions
                        (dotimes (col col-count)
                          (let* ((entry-index (+ (* col row-count) row))
                                 (col-start-px
                                  (+ group-start-px
                                     (cl-loop for c from 0 below col
                                              sum (+ (nth c col-pxs) gap-px)))))
                            ;; Pin to absolute pixel position
                            (insert (propertize " " 'display
                                                `(space :align-to (,col-start-px))))
                            ;; Insert pixel-clipped entry
                            (when (and (< row row-count)
                                       (< entry-index (length entries)))
                              (let ((entry (nth entry-index entries)))
                                (insert (bw/which-key--format-entry-clipped
                                         (car entry) (cdr entry)
                                         (nth col col-pxs)))))))
                        ;; Separator — skip on last row if next band follows
                        (when (and (< si (1- section-count))
                                   (not (and is-last-row skip-last-row-separators)))
                          (insert (propertize " " 'display
                                              `(space :align-to (,(nth si separator-positions-px)))))
                          (insert bw/which-key-vertical-separator)))))
        (insert "\n"))))

  (defun bw/which-key--render-sectioned-layout (buffer keymap-symbol)
    "Replace BUFFER content with sectioned rendering for KEYMAP-SYMBOL.
Content is centered within the posframe with even margins on all sides."
    (let* ((section-definitions (gethash keymap-symbol bw/which-key-section-definitions))
           (keymap (symbol-value keymap-symbol))
           (all-bindings (which-key--get-keymap-bindings keymap))
           (popup-width (- (frame-width (or (frame-parent (selected-frame))
                                              (selected-frame)))
                          16))
           (margin-chars 4)
           ;; Split on :break markers first, then resolve entries per band
           (raw-bands (bw/which-key--split-sections-into-bands section-definitions))
           (resolved-bands
            (delq nil
                  (cl-loop for band in raw-bands
                           collect
                           (let ((resolved
                                  (cl-loop for section in band
                                           for section-name = (car section)
                                           for section-keys = (cdr section)
                                           for resolved-entries =
                                             (cl-loop for key in section-keys
                                                      for binding = (assoc key all-bindings)
                                                      when binding collect binding)
                                           when resolved-entries
                                           collect (cons section-name resolved-entries))))
                             (when resolved resolved)))))
           (band-count (length resolved-bands)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "\n")  ;; Top padding
          (cl-loop for band in resolved-bands
                   for band-index from 0
                   do (when band
                        (let ((skip-last-sep (< band-index (1- band-count))))
                          (if (= (length band) 1)
                              (bw/which-key--render-single-section-band
                               (car band) popup-width margin-chars)
                            (bw/which-key--render-multi-section-band
                             band popup-width margin-chars skip-last-sep)))))
          (insert "\n")  ;; Bottom padding
          ))))

  ;; ═══════════════════════════════════════════════════════════════════
  ;; Activation — override posframe show function
  ;; ═══════════════════════════════════════════════════════════════════

  ;; In Emacs 30, which-key--update is a C subr whose idle timer calls it
  ;; directly at the C level, bypassing any fset wrapper. The only reliable
  ;; Lisp-level hook is which-key-posframe--show-buffer, called via
  ;; which-key-custom-show-popup-function after the C subr populates the buffer.

  (defun bw/which-key--resolve-current-keymap-symbol ()
    "Determine the keymap symbol for the current which-key prefix, if any."
    (let* ((prefix-keys (ignore-errors
                          (funcall which-key-this-command-keys-function)))
           (prefix-description (and prefix-keys (> (length prefix-keys) 0)
                                    (key-description prefix-keys))))
      (and prefix-description
           (cdr (assoc prefix-description bw/which-key-prefix-to-keymap-alist)))))

  (defun bw/which-key-posframe--show-buffer (act-popup-dim)
    "Show which-key in a posframe, rendering sectioned layout when defined.
ACT-POPUP-DIM is the (height . width) from which-key's default rendering."
    (when (posframe-workable-p)
      (let* ((buffer (get-buffer " *which-key*"))
             (keymap-symbol (bw/which-key--resolve-current-keymap-symbol))
             (has-sections (and keymap-symbol
                                (gethash keymap-symbol
                                         bw/which-key-section-definitions))))
        (when buffer
          ;; Re-render with sectioned layout if this prefix has sections
          (when has-sections
            (bw/which-key--render-sectioned-layout buffer keymap-symbol))
          ;; Show the posframe, sizing to actual buffer content for sectioned,
          ;; or using which-key's computed dimensions otherwise
          (let* ((height (if has-sections
                             (with-current-buffer buffer
                               (count-lines (point-min) (point-max)))
                           (car act-popup-dim)))
                 (max-height (min height (/ (frame-height) 3))))
            (posframe-show buffer
                           :height max-height
                           :width (- (frame-width) 8)
                           :min-width (- (frame-width) 8)
                           :poshandler
                           (lambda (info)
                             (let* ((parent-width (plist-get info :parent-frame-width))
                                    (parent-height (plist-get info :parent-frame-height))
                                    (child-width (plist-get info :posframe-width))
                                    (child-height (plist-get info :posframe-height))
                                    (bottom-margin 130))
                               (cons (/ (- parent-width child-width) 2)
                                     (- parent-height child-height bottom-margin))))
                           :background-color "#21242b"
                           :foreground-color "#bbc2cf"
                           :border-width 1
                           :border-color "#51afef"))))))

  ;; Clean up stale functions from previous code versions
  (dolist (sym '(bw/wk--resolve-keymap-sym bw/wk-sections bw/wk--render-sectioned
                 bw/wk--render-band bw/wk--render-multi-band bw/wk--split-bands
                 bw/wk--centered-header bw/wk--format-entry
                 bw/which-key--post-process-if-sectioned))
    (when (fboundp sym) (fmakunbound sym)))

  ;; Undo any stale fset wrapper on which-key--update from previous versions.
  ;; Chase closures to find the original C subr and restore it.
  (let ((fn (symbol-function 'which-key--update)))
    (when (and fn (not (subrp fn)))
      (let ((original fn))
        (while (and original (not (subrp original)))
          (cond
           ((advice--p original)
            (setq original (advice--cd*r original)))
           ((and (closurep original) (ignore-errors (aref original 2)))
            (let ((env (aref original 2)))
              (if (and (consp env) (consp (car env)))
                  (setq original (cdar env))
                (setq original nil))))
           (t (setq original nil))))
        (when (subrp original)
          (fset 'which-key--update original)))))

  ;; Remove any stale override from previous versions, then install ours
  (advice-remove 'which-key-posframe--show-buffer 'bw/which-key-posframe--show-buffer)
  (advice-add 'which-key-posframe--show-buffer :override
              #'bw/which-key-posframe--show-buffer)

  (unless which-key-posframe-mode (which-key-posframe-mode 1)))
