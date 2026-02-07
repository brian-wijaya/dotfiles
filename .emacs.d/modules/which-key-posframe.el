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
  ;; Use :break to force a new band (row of sections).
  ;; Use :gap within a key list to insert a blank line between sub-groups.
  (defvar bw/which-key-section-definitions (make-hash-table :test 'eq)
    "Section groupings per keymap for sectioned which-key rendering.")

  (puthash 'bw/leader-w-map
    '(("📐 Resize"   . ("+" "-" :gap "<" ">" :gap "="))
      ("✂️ Split"     . ("s" "2" :gap "v" "3"))
      ("🧭 Navigate"  . ("h" "j" "k" "l" :gap "o" "w"))
      ("🔀 Move"      . ("H" "J" "K" "L" :gap "x"))
      :break
      ("📦 Manage"    . ("d" "q" :gap "0" "1" "m"))
      ("⏳ History"   . ("u" "r"))
      ("🔧 Special"   . ("V" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-b-map
    '(("🔄 Switch"  . ("b" "l" :gap "n" "p"))
      ("✨ Create"  . ("N" "x" "e"))
      ("💾 Save"    . ("s"))
      ("📦 Manage"  . ("d" "k" :gap "r" "R" :gap "z"))
      ("📋 Info"    . ("i" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-g-map
    '(("📊 Status"  . ("g" "d" :gap "l" "b"))
      ("✏️ Stage"   . ("s" "S"))
      ("💾 Commit"  . ("c" "t"))
      ("🔀 Remote"  . ("f" "F" :gap "p" "r"))
      ("📦 Setup"   . ("C" "i" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-c-map
    '(("🔍 Navigate" . ("d" "D" :gap "h" "H" "o"))
      ("✏️ Edit"     . ("a" "f" "r" "w" "y"))
      ("⚠️ Diag"     . ("x"))
      ("📖 Docs"     . ("p" "P" :gap "k" "m" "M" "W"))
      ("🔧 Tools"    . ("t" "T" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-f-map
    '(("📄 Open"    . ("f" "r" :gap "p" "e" "E"))
      ("💾 Save"    . ("s" "S"))
      ("📦 Manage"  . ("c" "D" "R" "y" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-s-map
    '(("📄 Buffer"   . ("s" "S" :gap "l" "o" "i"))
      ("📁 Project"  . ("f" "g" :gap "I" "b" "d" "p"))
      ("🔖 Index"    . ("m" "r" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-o-map
    '(("📁 Files"    . ("E" "-" "d" :gap "F" "T" "." "D"))
      ("📅 Plan"     . ("a" "A" :gap "c" "C" "n"))
      ("📝 Open"     . ("e" "s" "t"))
      :break
      ("🔍 Find"     . ("b" "B" :gap "f" "i" "r" "P"))
      ("🤖 Apps"     . ("g" "o" "m" "w" "S"))
      ("📖 Docs"     . ("M" "h" "p" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-t-map
    '(("🎨 Display"  . ("b" "c" "F" :gap "h" "l" "v" "w" :gap "t" "z"))
      ("🔧 Tools"    . ("f" "i" "I" "r" "s" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-h-map
    '(("🔍 Describe" . ("f" "v" "k" :gap "m" "x" "o" :gap "c" "F" "a" "w"))
      ("📖 Browse"   . ("b" "i" :gap "d" "l" "M" :gap "p" "P" "e" "t"))
      ("🔧 Reload"   . ("r" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-p-map
    '(("📁 Files"    . ("f" "F" :gap "d" "b"))
      ("🔍 Search"   . ("s" "g" "r"))
      ("🚀 Run"      . ("!" "&" :gap "e" "c" "x"))
      ("📦 Manage"   . ("k" "p" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-q-map
    '(("💾 Save"     . ("q" "K"))
      ("⚡ Quick"    . ("Q" "f"))
      ("🔄 Restart"  . ("r" "R" "~")))
    bw/which-key-section-definitions)

  (puthash 'bw/leader-map
    '(("🚀 Launch"    . ("SPC" "." "," :gap "`" "x"))
      ("⚡ Execute"   . ("!" "&" :gap ":" ";"))
      ("🔍 Search"    . ("/" "*"))
      ("🔧 Utility"   . ("'" "u" "~"))
      :break
      ("📂 Files"     . ("f" "p" "g"))
      ("💻 Code"      . ("c" "s" "i"))
      ("🪟 Interface" . ("w" "b" "t"))
      ("📖 Open"      . ("o" "a" :gap "n" "h" :gap "q" "v")))
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

  (puthash 'bw/leader-v-map
    '(("📦 Vault" . ("i" "a" "s")))
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
      ("SPC v" . bw/leader-v-map)
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
                               bw/leader-t-map bw/leader-v-map
                               bw/ce-copy-map bw/ce-sort-map))
        (when (and (boundp keymap-symbol) (eq (symbol-value keymap-symbol) keymap))
          (throw 'found keymap-symbol)))
      nil))

  (defun bw/which-key--clean-description (description)
    "Strip \"group:\" prefix from which-key DESCRIPTION if present."
    (if (and description (string-prefix-p "group:" description))
        (substring description 6)
      (or description "")))

  (defun bw/which-key--extract-emoji (description)
    "Extract leading emoji from DESCRIPTION string.
Returns the emoji as a string, or nil if none found."
    (let ((desc (bw/which-key--clean-description description)))
      (when (and desc (> (length desc) 0))
        (let ((first-char (aref desc 0)))
          (when (or (>= first-char #x1F300)
                    (and (>= first-char #x2300) (<= first-char #x23FF))
                    (and (>= first-char #x2600) (<= first-char #x27BF)))
            (string first-char))))))

  (defun bw/which-key--resolve-entry (key keymap all-bindings)
    "Resolve KEY in KEYMAP to (KEY . DISPLAY-STRING).
DISPLAY-STRING is emoji+actual-command-name.  Emoji is extracted from
the which-key label in ALL-BINDINGS; command name comes from lookup-key."
    (let* ((wk-binding (assoc key all-bindings))
           (wk-desc (and wk-binding (cdr wk-binding)))
           (emoji (and wk-desc (bw/which-key--extract-emoji wk-desc)))
           (raw-binding (ignore-errors (lookup-key keymap (kbd key))))
           (cmd-name (cond
                      ((and (symbolp raw-binding)
                            (boundp raw-binding)
                            (keymapp (symbol-value raw-binding)))
                       (bw/which-key--clean-description wk-desc))
                      ((symbolp raw-binding)
                       (concat (or emoji "") (symbol-name raw-binding)))
                      ((keymapp raw-binding)
                       (bw/which-key--clean-description wk-desc))
                      (t (or (bw/which-key--clean-description wk-desc) "?")))))
      (when wk-binding
        (cons key cmd-name))))

  (defun bw/which-key--format-entry (key description)
    "Format KEY:DESCRIPTION entry with colored key."
    (let ((key-propertized (propertize key 'face '(:foreground "#51afef"))))
      (concat " " key-propertized ":" description)))

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

  ;; ═══════════════════════════════════════════════════════════════════
  ;; Rendering Engine — flexible-width sections with auto-packing
  ;; ═══════════════════════════════════════════════════════════════════

  (defun bw/which-key--measure-section-px (section)
    "Return pixel width needed for SECTION (NAME . resolved-entries).
Measures the widest formatted entry and the header, returns the larger
plus 2-char padding."
    (let* ((char-px (default-font-width))
           (section-name (car section))
           (entries (cdr section))
           (header-text (concat "── " section-name " ──"))
           (header-px (string-pixel-width header-text))
           (max-entry-px 0))
      (dolist (entry entries)
        (unless (eq (car entry) :gap)
          (let ((px (string-pixel-width
                     (bw/which-key--format-entry (car entry) (cdr entry)))))
            (when (> px max-entry-px)
              (setq max-entry-px px)))))
      (+ (max header-px max-entry-px) (* 2 char-px))))

  (defun bw/which-key--render-band (sections section-widths-px margin-px separator-px char-px)
    "Render SECTIONS side by side with flexible per-section widths.
SECTION-WIDTHS-PX is a list of pixel widths, one per section.
Uses pixel-based :align-to for precise positioning."
    (let* ((section-count (length sections))
           (section-starts-px
            (let ((pos margin-px))
              (cl-loop for i from 0 below section-count
                       collect (prog1 pos
                                 (setq pos (+ pos (nth i section-widths-px)
                                               separator-px))))))
           (separator-positions-px
            (cl-loop for i from 0 below (1- section-count)
                     collect (+ (nth i section-starts-px)
                                (nth i section-widths-px))))
           (section-entry-lists (mapcar #'cdr sections))
           (max-row-count (apply #'max
                                 (or (mapcar #'length section-entry-lists) '(0)))))
      ;; Header row
      (cl-loop for section in sections
               for si from 0
               for width-px in section-widths-px
               do (let ((header-chars (max 1 (/ width-px char-px))))
                    (insert (propertize " " 'display
                                        `(space :align-to (,(nth si section-starts-px)))))
                    (insert (bw/which-key--centered-header (car section) header-chars))))
      (insert "\n")
      ;; Entry rows
      (dotimes (row max-row-count)
        (cl-loop for si from 0
                 for entries in section-entry-lists
                 do (let ((entry (and (< row (length entries)) (nth row entries))))
                      (insert (propertize " " 'display
                                          `(space :align-to (,(nth si section-starts-px)))))
                      (when (and entry (not (eq (car entry) :gap)))
                        (let ((estart (point)))
                          (insert (bw/which-key--format-entry (car entry) (cdr entry)))
                          (when bw/which-key--entry-collector
                            (push (list estart (point) (car entry))
                                  bw/which-key--entry-collector))))
                      (when (< si (1- section-count))
                        (insert (propertize " " 'display
                                            `(space :align-to (,(nth si separator-positions-px)))))
                        (insert bw/which-key-vertical-separator))))
        (insert "\n"))))

  (defun bw/which-key--render-sectioned-layout (buffer keymap-symbol)
    "Replace BUFFER content with sectioned rendering for KEYMAP-SYMBOL.
Resolves entries dynamically via lookup-key, measures section widths,
auto-packs sections into bands that fit the frame width.
Skips re-render if same keymap is already displayed (preserves nav state)."
    ;; Guard: skip re-render if same keymap (which-key timer fires repeatedly)
    (unless (eq (buffer-local-value 'bw/which-key--rendered-keymap buffer)
                keymap-symbol)
    (let* ((section-defs (gethash keymap-symbol bw/which-key-section-definitions))
           (keymap (symbol-value keymap-symbol))
           (all-bindings (which-key--get-keymap-bindings keymap))
           (char-px (default-font-width))
           (frame (or (frame-parent (selected-frame)) (selected-frame)))
           (popup-width-chars (- (frame-width frame) 16))
           (margin-px (* 4 char-px))
           (separator-px (string-pixel-width bw/which-key-vertical-separator))
           ;; 1. Resolve all sections flat, tracking :break positions
           (resolved-flat '())
           (break-indices '())
           (section-keys-seen '())
           (sidx 0))
      ;; Walk definitions: resolve entries, note forced breaks
      (dolist (item section-defs)
        (if (eq item :break)
            (when resolved-flat
              (push sidx break-indices))
          (let* ((section-name (car item))
                 (section-keys (cdr item))
                 (entries
                  (let (result)
                    (dolist (key section-keys (nreverse result))
                      (if (eq key :gap)
                          (push '(:gap . nil) result)
                        (push key section-keys-seen)
                        (let ((entry (bw/which-key--resolve-entry
                                      key keymap all-bindings)))
                          (when entry (push entry result))))))))
            (when entries
              (push (cons section-name entries) resolved-flat)
              (cl-incf sidx)))))
      (setq resolved-flat (nreverse resolved-flat))
      (setq break-indices (nreverse break-indices))
      ;; Stage 4: Exhaustive inclusion — find uncategorized bindings
      (let* ((categorized (cl-remove-if (lambda (k) (string= k "~"))
                                        section-keys-seen))
             (uncategorized
              (cl-remove-if
               (lambda (b)
                 (let ((key (car b)))
                   (or (member key categorized)
                       (string-prefix-p "<" key)       ; mouse/fn keys
                       (string-prefix-p "ESC" key)     ; ESC duplicates
                       (string= key "~")               ; tilde catchall
                       (string-match-p "which-key" (or (cdr b) ""))))) ; wk internals
               all-bindings)))
        (when uncategorized
          (let ((other-entries
                 (let (result)
                   (dolist (b uncategorized (nreverse result))
                     (let ((entry (bw/which-key--resolve-entry
                                   (car b) keymap all-bindings)))
                       (when entry (push entry result)))))))
            (when other-entries
              (push (cons "🔧 Other" other-entries) resolved-flat)
              (cl-incf sidx)))))
      ;; 2. Measure each section's pixel width
      (let ((widths-px (mapcar #'bw/which-key--measure-section-px resolved-flat)))
        ;; 3. Auto-pack into bands (greedy bin-packing)
        (let* ((available-px (- (* popup-width-chars char-px) (* 2 margin-px)))
               (bands '())
               (cur-sections '())
               (cur-widths '())
               (cur-width 0))
          (cl-loop for section in resolved-flat
                   for width in widths-px
                   for i from 0
                   do (let ((needed (if cur-sections (+ width separator-px) width)))
                        ;; Forced break at marked positions
                        (when (and cur-sections (member i break-indices))
                          (push (cons (nreverse cur-sections) (nreverse cur-widths)) bands)
                          (setq cur-sections nil cur-widths nil cur-width 0
                                needed width))
                        ;; Auto break when band overflows
                        (when (and cur-sections (> (+ cur-width needed) available-px))
                          (push (cons (nreverse cur-sections) (nreverse cur-widths)) bands)
                          (setq cur-sections nil cur-widths nil cur-width 0
                                needed width))
                        (push section cur-sections)
                        (push width cur-widths)
                        (setq cur-width (+ cur-width needed))))
          (when cur-sections
            (push (cons (nreverse cur-sections) (nreverse cur-widths)) bands))
          (setq bands (nreverse bands))
          ;; 4. Render all bands with entry collection
          (with-current-buffer buffer
            (let ((inhibit-read-only t)
                  (bw/which-key--entry-collector (list :entries)))
              (erase-buffer)
              (insert "\n")
              (cl-loop for (band-sections . band-widths) in bands
                       for band-index from 0
                       do (progn
                            (when (> band-index 0)
                              (insert "\n"))
                            (bw/which-key--render-band
                             band-sections band-widths
                             margin-px separator-px char-px)))
              (insert "\n")
              ;; Store nav state
              (setq bw/which-key--rendered-keymap keymap-symbol
                    bw/which-key--nav-entries
                    (vconcat (cdr (nreverse bw/which-key--entry-collector)))
                    bw/which-key--nav-index nil
                    bw/which-key--scroll-line 0)
              (when bw/which-key--nav-overlay
                (delete-overlay bw/which-key--nav-overlay)
                (setq bw/which-key--nav-overlay nil)))))))))

  ;; ═══════════════════════════════════════════════════════════════════
  ;; Navigation & Pagination
  ;; ═══════════════════════════════════════════════════════════════════

  ;; Temporary collector used during rendering
  (defvar bw/which-key--entry-collector nil
    "Accumulates (START END KEY) entries during rendering.")

  ;; Buffer-local navigation state (set in " *which-key*" buffer)
  (defvar-local bw/which-key--rendered-keymap nil
    "Keymap symbol currently rendered, used to skip redundant re-renders.")
  (defvar-local bw/which-key--nav-entries nil
    "Vector of (START END KEY) for navigable entries.")
  (defvar-local bw/which-key--nav-index nil
    "Currently highlighted entry index, or nil.")
  (defvar-local bw/which-key--nav-overlay nil
    "Overlay used for highlight.")
  (defvar-local bw/which-key--scroll-line 0
    "Current scroll line offset for pagination.")

  (defun bw/which-key--posframe-window ()
    "Get the window displaying the which-key posframe, or nil."
    (let* ((buffer (get-buffer " *which-key*"))
           (frame (and buffer
                       (buffer-local-value 'posframe--frame buffer))))
      (and frame (frame-live-p frame) (frame-first-window frame))))

  ;; Flag to suppress which-key re-rendering during nav mode
  (defvar bw/which-key--in-nav-loop nil
    "Non-nil while the navigation event loop is active.")

  (defun bw/which-key--page-scroll (direction)
    "Scroll the which-key posframe by one page in DIRECTION (+1 or -1)."
    (let* ((win (bw/which-key--posframe-window))
           (buffer (get-buffer " *which-key*")))
      (when (and win buffer)
        (with-current-buffer buffer
          (let* ((visible (window-body-height win))
                 (total (count-lines (point-min) (point-max)))
                 (delta (* direction (- visible 2)))
                 (new-scroll (max 0 (min (+ bw/which-key--scroll-line delta)
                                         (max 0 (- total visible))))))
            (setq bw/which-key--scroll-line new-scroll)
            (set-window-start win
              (save-excursion
                (goto-char (point-min))
                (forward-line new-scroll)
                (point))))))))

  (defun bw/which-key--ensure-overlay ()
    "Ensure the navigation overlay exists in the which-key buffer."
    (let ((buffer (get-buffer " *which-key*")))
      (when buffer
        (with-current-buffer buffer
          (unless (and bw/which-key--nav-overlay
                       (overlay-buffer bw/which-key--nav-overlay))
            (setq bw/which-key--nav-overlay (make-overlay 1 1 buffer))
            (overlay-put bw/which-key--nav-overlay
                         'face '(:background "#3d4451"))
            (overlay-put bw/which-key--nav-overlay 'priority 100))))))

  (defun bw/which-key--move-highlight (index)
    "Move the highlight overlay to entry at INDEX."
    (let ((buffer (get-buffer " *which-key*")))
      (when buffer
        (with-current-buffer buffer
          (when bw/which-key--nav-entries
            (let* ((count (length bw/which-key--nav-entries))
                   (idx (mod index count))
                   (entry (aref bw/which-key--nav-entries idx))
                   (start (nth 0 entry))
                   (end (nth 1 entry)))
              (setq bw/which-key--nav-index idx)
              (bw/which-key--ensure-overlay)
              (move-overlay bw/which-key--nav-overlay start end)
              ;; Auto-scroll if highlight is off-screen
              (let ((win (bw/which-key--posframe-window)))
                (when win
                  (let ((win-start (window-start win))
                        (win-end (window-end win t)))
                    (when (or (< start win-start) (>= start win-end))
                      (set-window-start win
                        (save-excursion
                          (goto-char start)
                          (forward-line -2)
                          (point)))))))))))))

  (defun bw/which-key--show-posframe (buffer)
    "Show the which-key posframe for BUFFER with standard appearance.
Used by both the main show-buffer and nav-mode to (re-)display the popup."
    (let* ((total-height (with-current-buffer buffer
                           (count-lines (point-min) (point-max))))
           (max-height (min total-height (/ (* (frame-height) 2) 3)))
           (text-edges (window-body-pixel-edges))
           (gutter-px (car text-edges)))
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
                              (bottom-margin 130)
                              (text-width (- parent-width gutter-px))
                              (x-pos (+ gutter-px
                                        (/ (- text-width child-width) 2))))
                         (cons (max 0 x-pos)
                               (- parent-height child-height bottom-margin))))
                     :background-color "#21242b"
                     :foreground-color "#bbc2cf"
                     :border-width 1
                     :border-color "#51afef")))

  (defun bw/which-key--nav-execute-entry (buf)
    "Execute the currently highlighted entry and clean up nav state."
    (with-current-buffer buf
      (when (and bw/which-key--nav-index bw/which-key--nav-entries
                 bw/which-key--rendered-keymap)
        (let* ((entry (aref bw/which-key--nav-entries bw/which-key--nav-index))
               (key (nth 2 entry))
               (keymap (symbol-value bw/which-key--rendered-keymap))
               (cmd (ignore-errors (lookup-key keymap (kbd key)))))
          (bw/which-key--reset-nav-state)
          (posframe-hide " *which-key*")
          (when (and cmd (commandp cmd))
            (command-execute cmd))))))

  (defun bw/which-key--enter-nav-mode ()
    "Enter interactive navigation mode for the which-key popup.
Captures all subsequent input: arrows move highlight, RET executes,
PgUp/PgDn scroll, any other key exits and dispatches to the prefix keymap."
    (interactive)
    (let ((buf (get-buffer " *which-key*"))
          (initial-event last-input-event))
      (when (and buf (buffer-local-value 'bw/which-key--nav-entries buf))
        ;; Set flag immediately — which-key may have already hidden the posframe
        ;; when the prefix sequence completed, before this command runs.
        (setq bw/which-key--in-nav-loop t)
        (unwind-protect
            (progn
              ;; Highlight initial entry
              (with-current-buffer buf
                (bw/which-key--move-highlight
                 (if (eq initial-event 'up)
                     (1- (length bw/which-key--nav-entries))
                   0)))
              ;; Re-show posframe (was hidden when prefix completed)
              (bw/which-key--show-posframe buf)
              ;; Event loop
              (let ((continue t))
                (while continue
                  (let ((event (read-event)))
                    (pcase event
                      ('down
                       (with-current-buffer buf
                         (bw/which-key--move-highlight
                          (1+ (or bw/which-key--nav-index 0)))))
                      ('up
                       (with-current-buffer buf
                         (bw/which-key--move-highlight
                          (1- (or bw/which-key--nav-index 0)))))
                      ('next  (bw/which-key--page-scroll 1))
                      ('prior (bw/which-key--page-scroll -1))
                      ((or 13 'return)
                       (setq continue nil)
                       (bw/which-key--nav-execute-entry buf))
                      (27
                       (setq continue nil)
                       (with-current-buffer buf
                         (bw/which-key--reset-nav-state))
                       (posframe-hide " *which-key*"))
                      (_
                       (setq continue nil)
                       (with-current-buffer buf
                         (let ((km (and bw/which-key--rendered-keymap
                                        (symbol-value bw/which-key--rendered-keymap))))
                           (let ((cmd (and km (lookup-key km (vector event)))))
                             (bw/which-key--reset-nav-state)
                             (posframe-hide " *which-key*")
                             (if (and cmd (commandp cmd))
                                 (command-execute cmd)
                               (push event unread-command-events)))))))))))
          ;; Cleanup: always clear the flag
          (setq bw/which-key--in-nav-loop nil)))))

  (defun bw/which-key--reset-nav-state ()
    "Reset all navigation state in the which-key buffer."
    (let ((buffer (get-buffer " *which-key*")))
      (when buffer
        (with-current-buffer buffer
          (setq bw/which-key--rendered-keymap nil
                bw/which-key--nav-index nil
                bw/which-key--scroll-line 0
                bw/which-key--nav-entries nil)
          (when bw/which-key--nav-overlay
            (delete-overlay bw/which-key--nav-overlay)
            (setq bw/which-key--nav-overlay nil))))))

  ;; Navigation keymap — set as parent of all prefix keymaps.
  ;; Only Down and Up enter nav mode; other nav keys are handled in the loop.
  ;; Use setq to ensure reload updates the bindings.
  (defvar bw/which-key--nav-map nil
    "Navigation keymap installed as parent of all leader prefix keymaps.")
  (setq bw/which-key--nav-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<down>")  #'bw/which-key--enter-nav-mode)
      (define-key map (kbd "<up>")    #'bw/which-key--enter-nav-mode)
      map))

  ;; Install nav bindings into the ACTUAL keymap objects used by Evil.
  ;; The symbol values (bw/leader-w-map etc.) may differ from the real
  ;; keymaps in the leader chain, so we traverse the chain directly.
  (defun bw/which-key--install-nav-parents ()
    "Install nav keymap as parent of all actual prefix keymaps.
On reload, strips any old nav-map parent before installing the new one."
    (dolist (entry bw/which-key-prefix-to-keymap-alist)
      (let* ((prefix-str (car entry))
             (actual-km
              (if (string= prefix-str "SPC")
                  bw/leader-map
                (let* ((parts (cdr (split-string prefix-str " ")))
                       (km bw/leader-map))
                  (dolist (part parts km)
                    (let ((next (lookup-key km (kbd part))))
                      (setq km (if (keymapp next) next km))))))))
        (when (keymapp actual-km)
          ;; Strip any existing nav-map parent (handles reload with new map object)
          (let ((existing (keymap-parent actual-km)))
            (when existing
              ;; If parent is a composed keymap, filter out old nav maps
              (when (and (consp existing) (eq (car existing) 'keymap)
                         (consp (cadr existing)) (eq (caadr existing) 'keymap))
                ;; Composed keymap: find the non-nav parent
                (let ((non-nav nil))
                  (dolist (sub (cdr existing))
                    (when (and (keymapp sub)
                               (not (lookup-key sub (kbd "<down>"))))
                      (setq non-nav sub)))
                  (set-keymap-parent actual-km non-nav)
                  (setq existing non-nav)))
              ;; If parent IS the old nav map, remove it
              (when (and existing (lookup-key existing (kbd "<down>")))
                (set-keymap-parent actual-km nil)
                (setq existing nil))))
          ;; Install fresh nav-map as parent
          (let ((existing (keymap-parent actual-km)))
            (if existing
                (set-keymap-parent actual-km
                  (make-composed-keymap
                   (list bw/which-key--nav-map existing)))
              (set-keymap-parent actual-km bw/which-key--nav-map)))))))
  (bw/which-key--install-nav-parents)

  ;; ═══════════════════════════════════════════════════════════════════
  ;; Activation — override posframe show function
  ;; ═══════════════════════════════════════════════════════════════════

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
    (when (and (posframe-workable-p)
               (not bw/which-key--in-nav-loop))
      (let* ((buffer (get-buffer " *which-key*"))
             (keymap-symbol (bw/which-key--resolve-current-keymap-symbol))
             (has-sections (and keymap-symbol
                                (gethash keymap-symbol
                                         bw/which-key-section-definitions))))
        (when buffer
          ;; Reset rendered-keymap when switching to a different keymap
          (when (and has-sections
                     (not (eq (buffer-local-value
                               'bw/which-key--rendered-keymap buffer)
                              keymap-symbol)))
            (with-current-buffer buffer
              (setq bw/which-key--rendered-keymap nil)))
          (when has-sections
            (bw/which-key--render-sectioned-layout buffer keymap-symbol))
          (if has-sections
              (progn
                (bw/which-key--show-posframe buffer)
                ;; Restore scroll position
                (let ((win (bw/which-key--posframe-window))
                      (scroll (buffer-local-value 'bw/which-key--scroll-line buffer)))
                  (when (and win (> scroll 0))
                    (set-window-start win
                      (with-current-buffer buffer
                        (save-excursion
                          (goto-char (point-min))
                          (forward-line scroll)
                          (point)))))))
            ;; Non-sectioned: use act-popup-dim height, same posframe appearance
            (let* ((total-height (car act-popup-dim))
                   (max-height (min total-height (/ (* (frame-height) 2) 3)))
                   (text-edges (window-body-pixel-edges))
                   (gutter-px (car text-edges)))
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
                                      (bottom-margin 130)
                                      (text-width (- parent-width gutter-px))
                                      (x-pos (+ gutter-px
                                                (/ (- text-width child-width) 2))))
                                 (cons (max 0 x-pos)
                                       (- parent-height child-height bottom-margin))))
                             :background-color "#21242b"
                             :foreground-color "#bbc2cf"
                             :border-width 1
                             :border-color "#51afef")))))))

  ;; Clean up stale functions from previous code versions
  (dolist (sym '(bw/wk--resolve-keymap-sym bw/wk-sections bw/wk--render-sectioned
                 bw/wk--render-band bw/wk--render-multi-band bw/wk--split-bands
                 bw/wk--centered-header bw/wk--format-entry
                 bw/which-key--post-process-if-sectioned
                 bw/which-key--format-entry-clipped
                 bw/which-key--insert-entries-block
                 bw/which-key--optimal-column-count
                 bw/which-key--entry-visible-width
                 bw/which-key--split-sections-into-bands
                 bw/which-key--render-single-section-band
                 bw/which-key--render-multi-section-band))
    (when (fboundp sym) (fmakunbound sym)))

  ;; Undo any stale fset wrapper on which-key--update from previous versions.
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

  ;; Wrap the hide function to suppress hiding during nav mode.
  ;; which-key-posframe--hide is a C subr (Emacs 30) so we can't advise it;
  ;; instead, replace the variable with a lambda that guards the call.
  (setq which-key-custom-hide-popup-function
        (lambda ()
          (unless bw/which-key--in-nav-loop
            (which-key-posframe--hide))))

  (unless which-key-posframe-mode (which-key-posframe-mode 1)))
