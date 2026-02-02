;;; vsnake.el --- True newspaper column view -*- lexical-binding: t; -*-

;;; Commentary:
;; Newspaper-style sequential column viewing mode.
;; Content flows left-to-right across columns: col1 shows lines 1-H,
;; col2 shows lines H+1-2H, col3 shows lines 2H+1-3H, etc.
;; All columns have uniform width for consistent visual parsing.
;;
;; Entry point: `bw/vsnake-toggle' (bound to SPC w V in bindings-doom-full.el).

;;; Code:

(require 'cl-lib)

;; --- Configuration ---

(defcustom bw/vsnake-auto-recalc 'off
  "When to automatically recalculate column layout on frame/font resize.
Possible values:
  \\='off        - Never auto-recalculate (manual only via \\='r) [DEFAULT]
  \\='always     - Recalculate on significant resize (>10% width change)
  \\='first-time - Recalculate only on first activation per buffer"
  :type '(choice (const :tag "Never" off)
                 (const :tag "Always" always)
                 (const :tag "First time only" first-time))
  :group 'vsnake)

(defcustom bw/vsnake-recalc-debounce-ms 2000
  "Milliseconds to wait after resize stops before recalculating.
Prevents thrashing during continuous font size adjustments."
  :type 'integer
  :group 'vsnake)

(defvar bw/vsnake-min-widths
  '((vterm-mode . 50)      ; logs need space for timestamps
    (eshell-mode . 50)
    (term-mode . 50)
    (prog-mode . 35)       ; code can compress more
    (text-mode . 40)       ; prose needs more for words
    (org-mode . 40)
    (markdown-mode . 40)
    (help-mode . 60)       ; documentation: fixed comfortable width
    (Info-mode . 60)
    (Man-mode . 60)
    (default . 35))
  "Mode-specific minimum column widths in characters.")

;; --- Buffer-Local State ---

(defvar-local bw/vsnake--saved-layout nil
  "Saved (num-cols col-width) for this buffer.
Set when user manually adjusts layout or exits vsnake.
Nil means calculate from scratch.")

(defvar-local bw/vsnake--layout-source nil
  "Symbol indicating how current layout was determined.
Values: \\='calculated, \\='restored, \\='manual.")

(defvar-local bw/vsnake--saved-config nil
  "Window configuration saved before entering vsnake mode.")

(defvar-local bw/vsnake--num-cols nil
  "Current number of columns in vsnake mode.")

(defvar-local bw/vsnake--col-width nil
  "Current column width in characters.")

(defvar-local bw/vsnake--col-height nil
  "Current column height in lines.")

(defvar-local bw/vsnake--recalc-timer nil
  "Timer for debounced auto-recalculation.")

(defvar-local bw/vsnake--windows nil
  "List of windows used for columns (left to right).")

(defvar-local bw/vsnake--source-buffer nil
  "The buffer being viewed in vsnake mode.")

(defvar-local bw/vsnake--header-window nil
  "Dedicated header window spanning frame width.")

(defvar-local bw/vsnake--modeline-window nil
  "Dedicated mode-line window spanning frame width.")

;; Forward declaration for minor mode variable (defined by define-minor-mode later)
(defvar bw/vsnake-reading-mode)

;; --- Content Analysis ---

(defun bw/vsnake--sample-line-lengths (&optional max-lines)
  "Return list of line lengths from buffer (up to MAX-LINES).
Samples from beginning of buffer. MAX-LINES defaults to 10000.
Filters out very short lines (< 10 chars) as they're likely empty or headers."
  (let ((max-lines (or max-lines 10000))
        (lengths '()))
    (save-excursion
      (goto-char (point-min))
      (dotimes (_ max-lines)
        (unless (eobp)
          (let ((len (- (line-end-position) (line-beginning-position))))
            ;; Only include lines with meaningful content (≥10 chars)
            (when (>= len 10)
              (push len lengths)))
          (forward-line 1))))
    (nreverse lengths)))

(defun bw/vsnake--percentile (data p)
  "Return the Pth percentile of DATA (0.0 to 1.0).
DATA must be a non-empty list of numbers."
  (when (null data)
    (error "vsnake--percentile: empty data list"))
  (let* ((sorted (sort (copy-sequence data) #'<))
         (len (length sorted))
         (index (if (= len 1)
                    0
                  (floor (* p (1- len))))))
    (nth index sorted)))

(defun bw/vsnake--mode-bin (data bin-width)
  "Return center of most common bin in DATA with BIN-WIDTH.
Returns the midpoint of the most frequently occurring range."
  (let ((bins (make-hash-table :test 'equal)))
    ;; Count occurrences in bins
    (dolist (val data)
      (let ((bin (/ val bin-width)))
        (puthash bin (1+ (gethash bin bins 0)) bins)))
    ;; Find most common bin
    (let ((max-count 0)
          (max-bin 0))
      (maphash (lambda (bin count)
                 (when (> count max-count)
                   (setq max-count count)
                   (setq max-bin bin)))
               bins)
      ;; Return center of bin
      (+ (* max-bin bin-width) (/ bin-width 2)))))

(defun bw/vsnake--mode-min-width ()
  "Return minimum column width for current major mode."
  (let ((mode-entry (or (cl-find-if (lambda (entry)
                                      (derived-mode-p (car entry)))
                                    bw/vsnake-min-widths)
                        (assq 'default bw/vsnake-min-widths))))
    (cdr mode-entry)))

(defun bw/vsnake--analyze-content ()
  "Analyze buffer content and return metrics plist.
Returns: (:p75 N :p50 N :mode-bin N :effective-width N :min-width N)"
  (let* ((lengths (bw/vsnake--sample-line-lengths 10000))
         (min-width (bw/vsnake--mode-min-width))
         ;; Handle edge case: empty buffer or all-empty lines
         (p75 (if (and lengths (> (length lengths) 0))
                  (max min-width (bw/vsnake--percentile lengths 0.75))
                60))
         (p50 (if (and lengths (> (length lengths) 0))
                  (max min-width (bw/vsnake--percentile lengths 0.50))
                50))
         (mode-bin (if (and lengths (> (length lengths) 0))
                       (bw/vsnake--mode-bin lengths 10)  ; Larger bin for more stability
                     55))
         ;; Effective width: prioritize P75, only constrain by mode if P75 is excessive
         ;; Add 10% breathing room for readability
         (target-width (floor (* p75 1.1)))
         (effective-width (if (and (> mode-bin 20) (> target-width (+ mode-bin 30)))
                              ;; P75 is much wider than typical content, constrain it
                              (+ mode-bin 20)
                            ;; P75 is reasonable, use it
                            target-width)))
    ;; Clamp effective width to reasonable range [min-width, 80]
    (setq effective-width (max min-width (min effective-width 80)))
    (list :p75 p75
          :p50 p50
          :mode-bin mode-bin
          :effective-width effective-width
          :min-width min-width)))

;; --- Layout Calculation ---

(defun bw/vsnake-calculate-uniform-layout (frame-width content-metrics)
  "Return (num-cols col-width) with all columns equal width.
Maximizes visible content area (num-cols × col-width).
FRAME-WIDTH is total frame width in characters.
CONTENT-METRICS is plist from `bw/vsnake--analyze-content'."
  (let* ((available (- frame-width 6))  ; reserve 6 chars for left gutter
         (target (plist-get content-metrics :effective-width))
         (min-width (plist-get content-metrics :min-width))

         ;; Start by trying to fit columns at target width
         (num-cols (max 1 (floor (/ available (+ target 1)))))

         ;; Distribute available space uniformly across columns
         ;; Formula: (available - separators) / num-cols
         ;;   where separators = num-cols - 1 (one between each column)
         (col-width (if (> num-cols 1)
                        (floor (/ (- available (1- num-cols)) num-cols))
                      available)))

    ;; If col-width fell below minimum, reduce column count and redistribute
    (while (and (> num-cols 1) (< col-width min-width))
      (setq num-cols (1- num-cols))
      (setq col-width (if (> num-cols 1)
                          (floor (/ (- available (1- num-cols)) num-cols))
                        available)))

    ;; Final bounds check
    (when (< col-width min-width)
      (if (>= available min-width)
          (setq col-width min-width)  ; Accept narrow frame with warning
        (error "vsnake: frame-width %d too narrow (need ≥%d)"
               frame-width (+ min-width 7))))

    ;; Report utilization
    (let ((content-area (* num-cols col-width))
          (utilization (* 100.0 (/ (float (* num-cols col-width)) available))))
      (message "vsnake: %d cols × %d chars = %d content area (%.1f%% utilization)"
               num-cols col-width content-area utilization))

    (list num-cols col-width)))

(defun bw/vsnake-get-layout ()
  "Return (num-cols col-width) for current buffer.
Uses saved layout if available, otherwise calculates from content."
  (if bw/vsnake--saved-layout
      (progn
        (setq bw/vsnake--layout-source 'restored)
        (message "vsnake: restored layout %S" bw/vsnake--saved-layout)
        bw/vsnake--saved-layout)
    (let ((layout (bw/vsnake-calculate-uniform-layout
                   (window-total-width)
                   (bw/vsnake--analyze-content))))
      (setq bw/vsnake--layout-source 'calculated)
      layout)))

(defun bw/vsnake-save-layout (num-cols col-width)
  "Save layout configuration for this buffer."
  (setq bw/vsnake--saved-layout (list num-cols col-width)))

;; --- Frame Chrome (Header/Mode-line) ---

(defun bw/vsnake--create-frame-chrome ()
  "Create dedicated header and mode-line windows spanning frame width.
Returns (header-window middle-window . modeline-window).
Leaves the middle window selected for column creation."
  ;; Temporarily allow small windows for chrome creation
  (let ((window-min-height 1)
        (window-safe-min-height 1))
    ;; Create header at top (1 line)
    (let* ((header-win (split-window nil 1 'above))
           (header-buf (get-buffer-create " *vsnake-header*"))
           (middle-win (selected-window)))  ; Remember middle window
      (with-current-buffer header-buf
        (erase-buffer)
        (setq mode-line-format nil
              header-line-format nil
              cursor-type nil
              buffer-read-only t))
      (set-window-buffer header-win header-buf)
      (set-window-dedicated-p header-win t)

      ;; Create mode-line at bottom (1 line) - split from middle window
      (select-window middle-win)
      (let* ((modeline-win (split-window nil 1 'below))
             (modeline-buf (get-buffer-create " *vsnake-modeline*")))
        (with-current-buffer modeline-buf
          (erase-buffer)
          (setq mode-line-format nil
                header-line-format nil
                cursor-type nil
                buffer-read-only t))
        (set-window-buffer modeline-win modeline-buf)
        (set-window-dedicated-p modeline-win t)

        ;; Ensure we're back in middle window
        (select-window middle-win)

        (list header-win middle-win modeline-win)))))

(defun bw/vsnake--update-frame-chrome ()
  "Update header and mode-line content to reflect current buffer state."
  (when (and bw/vsnake--header-window bw/vsnake--modeline-window)
    ;; Update header
    (with-current-buffer (window-buffer bw/vsnake--header-window)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize
                 (format " %s  " (buffer-name bw/vsnake--source-buffer))
                 'face '(:background "#3c3836" :foreground "#ebdbb2" :weight bold)))))

    ;; Update mode-line
    (with-current-buffer (window-buffer bw/vsnake--modeline-window)
      (let ((inhibit-read-only t)
            (line-num (with-current-buffer bw/vsnake--source-buffer
                        (line-number-at-pos (point))))
            (col-num (with-current-buffer bw/vsnake--source-buffer
                       (current-column)))
            (mode-name (with-current-buffer bw/vsnake--source-buffer
                         mode-name)))
        (erase-buffer)
        (insert (propertize
                 (format " %s  Line %d, Col %d  [vsnake: %d cols × %d chars]  "
                         mode-name line-num col-num
                         bw/vsnake--num-cols bw/vsnake--col-width)
                 'face '(:background "#3c3836" :foreground "#ebdbb2")))))))

;; --- Column Display ---

(defun bw/vsnake--create-columns (num-cols col-width)
  "Create NUM-COLS side-by-side windows of COL-WIDTH.
Returns list of windows (left to right).
Assumes current window is maximized (delete-other-windows already called)."
  (let ((windows (list (selected-window))))
    ;; Split from right to left - each new window becomes the split point
    ;; This avoids the "too small" error by always splitting the full remaining width
    (dotimes (_ (1- num-cols))
      (let ((new-win (split-window-right col-width)))
        (push new-win windows)
        (select-window new-win)))  ; Select the new window for next split

    ;; Balance widths to ensure uniformity
    (balance-windows)

    ;; Return windows in left-to-right order
    (nreverse windows)))

(defun bw/vsnake--position-columns ()
  "Position each column window to show its section of buffer.
Columns show truly sequential content - each starts where previous ended."
  (let* ((num-cols (length bw/vsnake--windows))
         (total-lines (count-lines (point-min) (point-max)))
         (current-line (line-number-at-pos (point)))
         ;; Start from top of buffer by default
         (first-start-pos (point-min)))

    ;; Position each column sequentially using buffer positions
    ;; Use two-pass approach: set initial positions, then refine
    (let ((next-start-pos first-start-pos))
      ;; Pass 1: Set initial window positions
      (dotimes (i num-cols)
        (let ((win (nth i bw/vsnake--windows)))
          (with-selected-window win
            (set-window-buffer win bw/vsnake--source-buffer)
            (set-window-start win next-start-pos)
            (display-line-numbers-mode 1)

            ;; Prevent automatic scrolling - windows are static viewports
            (setq-local scroll-conservatively 101)  ; Never recenter
            (setq-local scroll-margin 0)            ; No margin-triggered scrolling
            (setq-local auto-hscroll-mode nil)      ; No horizontal auto-scroll

            (if (zerop i)
                (setq mode-line-format
                      '(:eval (format " %s  Line %d  [vsnake: %d cols × %d chars]  "
                                      mode-name
                                      (line-number-at-pos (point))
                                      bw/vsnake--num-cols
                                      bw/vsnake--col-width)))
              (setq mode-line-format nil))
            (setq header-line-format nil)
            (set-window-fringes win 0 0)
            (set-window-margins win 0 0)
            (setq cursor-type (if (zerop i) t nil))
            (setq truncate-lines nil
                  word-wrap t))
          ;; Rough estimate for next column
          (setq next-start-pos
                (save-excursion
                  (goto-char next-start-pos)
                  (forward-line bw/vsnake--col-height)
                  (point)))))

      ;; Pass 2: Refine positions using actual window-end
      (redisplay t)
      (dotimes (i (1- num-cols))
        (let* ((win (nth i bw/vsnake--windows))
               (next-win (nth (1+ i) bw/vsnake--windows))
               (actual-end (window-end win t)))
          (set-window-start next-win actual-end))))))

(defun bw/vsnake--rebuild-columns (num-cols col-width)
  "Rebuild column layout with new dimensions.
Tears down existing windows and recreates with new NUM-COLS and COL-WIDTH."
  (let ((buf (current-buffer))
        (pt (point)))
    ;; Clear dedicated flags
    (dolist (win (window-list))
      (set-window-dedicated-p win nil)
      (set-window-parameter win 'no-delete-other-windows nil))

    ;; Delete all other windows
    (delete-other-windows)

    ;; Create new column layout
    (setq bw/vsnake--windows (bw/vsnake--create-columns num-cols col-width))
    (setq bw/vsnake--num-cols num-cols)
    (setq bw/vsnake--col-width col-width)
    (setq bw/vsnake--col-height (window-body-height (car bw/vsnake--windows)))
    (setq bw/vsnake--source-buffer buf)

    ;; Position columns
    (goto-char pt)
    (bw/vsnake--position-columns)

    ;; Select leftmost column
    (select-window (car bw/vsnake--windows))))

;; --- Scrolling ---

(defun bw/vsnake--scroll-all-columns (delta)
  "Scroll all columns by DELTA lines.
Maintains sequential column positioning accounting for word-wrapping."
  (let* ((num-cols (length bw/vsnake--windows))
         (total-lines (count-lines (point-min) (point-max)))
         ;; Get current top position of leftmost column
         (current-top-pos (window-start (car bw/vsnake--windows)))
         ;; Calculate new top position by moving DELTA lines
         (new-top-pos (save-excursion
                        (goto-char current-top-pos)
                        (forward-line delta)
                        (point))))

    ;; Reposition all columns sequentially starting from new-top-pos
    (let ((next-start-pos (max (point-min) new-top-pos)))
      ;; Pass 1: Set initial positions
      (dotimes (i num-cols)
        (let ((win (nth i bw/vsnake--windows)))
          (set-window-start win next-start-pos)
          (setq next-start-pos
                (save-excursion
                  (goto-char next-start-pos)
                  (forward-line bw/vsnake--col-height)
                  (point)))))

      ;; Pass 2: Refine using actual window-end
      (redisplay t)
      (dotimes (i (1- num-cols))
        (let* ((win (nth i bw/vsnake--windows))
               (next-win (nth (1+ i) bw/vsnake--windows))
               (actual-end (window-end win t)))
          (set-window-start next-win actual-end))))))

;; --- Cursor Movement (Tapestry Model) ---

(defun bw/vsnake--current-column-index ()
  "Return index of currently selected column window (0-based)."
  (cl-position (selected-window) bw/vsnake--windows :test #'eq))

(defun bw/vsnake--find-column-containing-pos (pos)
  "Return index of column window that contains buffer position POS, or nil if none."
  (let ((idx 0)
        (found nil))
    (while (and (< idx (length bw/vsnake--windows))
                (not found))
      (let* ((win (nth idx bw/vsnake--windows))
             (win-start (window-start win))
             (win-end (window-end win t)))
        (when (and (>= pos win-start) (<= pos win-end))
          (setq found idx)))
      (setq idx (1+ idx)))
    found))

(defun bw/vsnake-move-down-line ()
  "Move cursor down one line, flowing seamlessly across columns.
The columns act as a unified tapestry - cursor flows from bottom of one column
to top of the next without scrolling. Only scrolls when reaching bottom of rightmost column."
  (interactive)
  (let* ((current-col-idx (bw/vsnake--current-column-index))
         (next-pos (save-excursion (forward-line 1) (point)))
         (target-col-idx (bw/vsnake--find-column-containing-pos next-pos)))

    (cond
     ;; Target position visible in same column - move without triggering auto-scroll
     ((and target-col-idx (= target-col-idx current-col-idx))
      (goto-char next-pos))  ; Use goto-char instead of forward-line to prevent auto-scroll

     ;; Target position visible in another column - flow to that column
     (target-col-idx
      (select-window (nth target-col-idx bw/vsnake--windows))
      (goto-char next-pos))

     ;; Target position not visible anywhere - scroll entire view down
     (t
      (bw/vsnake--scroll-all-columns 1)
      (goto-char next-pos)  ; Use goto-char instead of forward-line
      ;; Ensure cursor stays visible in correct column
      (let ((new-col-idx (bw/vsnake--find-column-containing-pos (point))))
        (when new-col-idx
          (select-window (nth new-col-idx bw/vsnake--windows))))))

    (force-mode-line-update)))

(defun bw/vsnake-move-up-line ()
  "Move cursor up one line, flowing seamlessly across columns.
Flows from top of one column to bottom of previous column without scrolling.
Only scrolls when reaching top of leftmost column."
  (interactive)
  (let* ((current-col-idx (bw/vsnake--current-column-index))
         (prev-pos (save-excursion (forward-line -1) (point)))
         (target-col-idx (bw/vsnake--find-column-containing-pos prev-pos)))

    (cond
     ;; Target position visible in same column - move without triggering auto-scroll
     ((and target-col-idx (= target-col-idx current-col-idx))
      (goto-char prev-pos))  ; Use goto-char instead of forward-line to prevent auto-scroll

     ;; Target position visible in another column - flow to that column
     (target-col-idx
      (select-window (nth target-col-idx bw/vsnake--windows))
      (goto-char prev-pos))

     ;; Target position not visible anywhere - scroll entire view up
     (t
      (bw/vsnake--scroll-all-columns -1)
      (goto-char prev-pos)  ; Use goto-char instead of forward-line
      ;; Ensure cursor stays visible in correct column
      (let ((new-col-idx (bw/vsnake--find-column-containing-pos (point))))
        (when new-col-idx
          (select-window (nth new-col-idx bw/vsnake--windows))))))

    (force-mode-line-update)))

(defun bw/vsnake-next-column ()
  "Move cursor to next column (rightward), staying at same line when possible."
  (interactive)
  (let* ((current-idx (bw/vsnake--current-column-index))
         (next-idx (and current-idx (1+ current-idx))))
    (if (and next-idx (< next-idx (length bw/vsnake--windows)))
        (let ((current-line (line-number-at-pos (point)))
              (next-win (nth next-idx bw/vsnake--windows)))
          (select-window next-win)
          ;; Try to go to same line in next column
          (goto-char (window-start next-win))
          (forward-line (- current-line (line-number-at-pos (window-start next-win)))))
      (message "vsnake: already at rightmost column"))))

(defun bw/vsnake-prev-column ()
  "Move cursor to previous column (leftward), staying at same line when possible."
  (interactive)
  (let* ((current-idx (bw/vsnake--current-column-index))
         (prev-idx (and current-idx (1- current-idx))))
    (if (and prev-idx (>= prev-idx 0))
        (let ((current-line (line-number-at-pos (point)))
              (prev-win (nth prev-idx bw/vsnake--windows)))
          (select-window prev-win)
          ;; Try to go to same line in prev column
          (goto-char (window-start prev-win))
          (forward-line (- current-line (line-number-at-pos (window-start prev-win)))))
      (message "vsnake: already at leftmost column"))))

(defun bw/vsnake-move-down-page ()
  "Move cursor down by one page (column height), flowing across columns."
  (interactive)
  (dotimes (_ bw/vsnake--col-height)
    (bw/vsnake-move-down-line)))

(defun bw/vsnake-move-up-page ()
  "Move cursor up by one page (column height), flowing across columns."
  (interactive)
  (dotimes (_ bw/vsnake--col-height)
    (bw/vsnake-move-up-line)))

(defun bw/vsnake-move-down-half-page ()
  "Move cursor down by half page, flowing across columns."
  (interactive)
  (dotimes (_ (/ bw/vsnake--col-height 2))
    (bw/vsnake-move-down-line)))

(defun bw/vsnake-move-up-half-page ()
  "Move cursor up by half page, flowing across columns."
  (interactive)
  (dotimes (_ (/ bw/vsnake--col-height 2))
    (bw/vsnake-move-up-line)))

(defun bw/vsnake-scroll-view-down ()
  "Scroll view down one line without moving cursor (like Vim's C-e)."
  (interactive)
  (bw/vsnake--scroll-all-columns 1)
  ;; Ensure cursor stays visible after scroll
  (let ((col-idx (bw/vsnake--find-column-containing-pos (point))))
    (when col-idx
      (select-window (nth col-idx bw/vsnake--windows))))
  (force-mode-line-update))

(defun bw/vsnake-scroll-view-up ()
  "Scroll view up one line without moving cursor (like Vim's C-y)."
  (interactive)
  (bw/vsnake--scroll-all-columns -1)
  ;; Ensure cursor stays visible after scroll
  (let ((col-idx (bw/vsnake--find-column-containing-pos (point))))
    (when col-idx
      (select-window (nth col-idx bw/vsnake--windows))))
  (force-mode-line-update))

;; --- Manual Resize Commands ---

(defun bw/vsnake-widen-columns (delta)
  "Increase all column widths by DELTA chars.
Reduces column count if necessary to fit frame."
  (interactive "p")
  (let* ((new-col-width (+ bw/vsnake--col-width delta))
         (available (- (window-total-width) 6))
         (max-cols (max 1 (floor (/ (- available (1- bw/vsnake--num-cols))
                                     new-col-width))))
         (new-num-cols (min bw/vsnake--num-cols max-cols)))

    (bw/vsnake-save-layout new-num-cols new-col-width)
    (setq bw/vsnake--layout-source 'manual)
    (bw/vsnake--rebuild-columns new-num-cols new-col-width)

    (message "vsnake: columns %d→%d, width %d→%d (manual)"
             bw/vsnake--num-cols new-num-cols
             bw/vsnake--col-width new-col-width)))

(defun bw/vsnake-narrow-columns (delta)
  "Decrease all column widths by DELTA chars.
Increases column count if more columns fit."
  (interactive "p")
  (let* ((min-width (bw/vsnake--mode-min-width))
         (new-col-width (max min-width (- bw/vsnake--col-width delta)))
         (available (- (window-total-width) 6))
         (new-num-cols (max 1 (floor (/ available (+ new-col-width 1))))))

    (bw/vsnake-save-layout new-num-cols new-col-width)
    (setq bw/vsnake--layout-source 'manual)
    (bw/vsnake--rebuild-columns new-num-cols new-col-width)

    (message "vsnake: columns %d→%d, width %d→%d (manual)"
             bw/vsnake--num-cols new-num-cols
             bw/vsnake--col-width new-col-width)))

(defun bw/vsnake-add-column ()
  "Add one more column if frame width permits."
  (interactive)
  (let* ((new-num-cols (1+ bw/vsnake--num-cols))
         (available (- (window-total-width) 6))
         (new-col-width (floor (/ (- available (1- new-num-cols)) new-num-cols)))
         (min-width (bw/vsnake--mode-min-width)))

    (if (< new-col-width min-width)
        (message "vsnake: cannot add column (would shrink below min-width %d)" min-width)
      (bw/vsnake-save-layout new-num-cols new-col-width)
      (setq bw/vsnake--layout-source 'manual)
      (bw/vsnake--rebuild-columns new-num-cols new-col-width)
      (message "vsnake: added column (%d total, width %d)" new-num-cols new-col-width))))

(defun bw/vsnake-remove-column ()
  "Remove one column (minimum 1)."
  (interactive)
  (when (<= bw/vsnake--num-cols 1)
    (user-error "vsnake: cannot remove last column"))

  (let* ((new-num-cols (1- bw/vsnake--num-cols))
         (available (- (window-total-width) 6))
         (new-col-width (floor (/ (- available (1- new-num-cols)) new-num-cols))))

    (bw/vsnake-save-layout new-num-cols new-col-width)
    (setq bw/vsnake--layout-source 'manual)
    (bw/vsnake--rebuild-columns new-num-cols new-col-width)
    (message "vsnake: removed column (%d remaining, width %d)" new-num-cols new-col-width)))

(defun bw/vsnake-recalculate ()
  "Recalculate column layout from scratch, ignoring saved layout."
  (interactive)
  (unless bw/vsnake-reading-mode
    (user-error "Not in vsnake mode"))

  ;; Force fresh calculation
  (let* ((bw/vsnake--saved-layout nil)  ; temporarily override
         (layout (bw/vsnake-calculate-uniform-layout
                  (window-total-width)
                  (bw/vsnake--analyze-content)))
         (num-cols (car layout))
         (col-width (cadr layout)))

    ;; Save new calculated layout
    (bw/vsnake-save-layout num-cols col-width)
    (setq bw/vsnake--layout-source 'calculated)

    ;; Rebuild column display
    (bw/vsnake--rebuild-columns num-cols col-width)

    (message "vsnake: recalculated to %d columns × %d chars" num-cols col-width)))

;; --- Auto-Recalc ---

(defvar bw/vsnake--last-frame-width nil
  "Last known frame width to detect significant changes.")

(defun bw/vsnake--maybe-auto-recalc ()
  "Trigger debounced auto-recalculation if enabled.
Only triggers on significant width changes (>10%) to avoid spurious events."
  (when (and bw/vsnake-reading-mode
             (not (eq bw/vsnake-auto-recalc 'off)))
    (let* ((current-width (window-total-width))
           (last-width (or bw/vsnake--last-frame-width current-width))
           (width-delta (abs (- current-width last-width)))
           (significant-change (> width-delta (* 0.1 last-width))))  ; >10% change

      (when significant-change
        (setq bw/vsnake--last-frame-width current-width)

        (pcase bw/vsnake-auto-recalc
          ('always
           ;; Cancel pending timer, start new one
           (when bw/vsnake--recalc-timer
             (cancel-timer bw/vsnake--recalc-timer))
           (setq bw/vsnake--recalc-timer
                 (run-with-timer (/ bw/vsnake-recalc-debounce-ms 1000.0) nil
                                 #'bw/vsnake--do-auto-recalc)))

          ('first-time
           ;; Only recalc if layout was calculated (not restored/manual)
           (when (eq bw/vsnake--layout-source 'calculated)
             (when bw/vsnake--recalc-timer
               (cancel-timer bw/vsnake--recalc-timer))
             (setq bw/vsnake--recalc-timer
                   (run-with-timer (/ bw/vsnake-recalc-debounce-ms 1000.0) nil
                                   #'bw/vsnake--do-auto-recalc)))))))))

(defun bw/vsnake--do-auto-recalc ()
  "Execute the deferred recalculation."
  (setq bw/vsnake--recalc-timer nil)
  (when bw/vsnake-reading-mode
    (bw/vsnake-recalculate)
    (message "vsnake: auto-recalculated (debounced after %.1fs)"
             (/ bw/vsnake-recalc-debounce-ms 1000.0))))

;; --- Mode Definition ---

(define-minor-mode bw/vsnake-reading-mode
  "Read-only navigation mode for vsnake column view."
  :lighter " vsnake"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Exit
            (define-key map (kbd "q") #'bw/vsnake-exit)
            (define-key map (kbd "Q") #'bw/vsnake-exit)

            ;; Cursor movement (tapestry model - flows across columns)
            (define-key map (kbd "j") #'bw/vsnake-move-down-line)
            (define-key map (kbd "k") #'bw/vsnake-move-up-line)
            (define-key map (kbd "n") #'bw/vsnake-move-down-line)
            (define-key map (kbd "p") #'bw/vsnake-move-up-line)
            (define-key map (kbd "<down>") #'bw/vsnake-move-down-line)
            (define-key map (kbd "<up>") #'bw/vsnake-move-up-line)

            ;; Page movement (moves cursor by page, flowing across columns)
            (define-key map (kbd "<next>") #'bw/vsnake-move-down-page)
            (define-key map (kbd "<prior>") #'bw/vsnake-move-up-page)
            (define-key map (kbd "SPC") #'bw/vsnake-move-down-page)
            (define-key map (kbd "C-d") #'bw/vsnake-move-down-half-page)
            (define-key map (kbd "C-u") #'bw/vsnake-move-up-half-page)

            ;; View scrolling (scrolls view without moving cursor - Vim's C-e/C-y)
            (define-key map (kbd "C-e") #'bw/vsnake-scroll-view-down)
            (define-key map (kbd "C-y") #'bw/vsnake-scroll-view-up)

            ;; Column navigation
            (define-key map (kbd "L") #'bw/vsnake-next-column)
            (define-key map (kbd "H") #'bw/vsnake-prev-column)
            (define-key map (kbd "C-<right>") #'bw/vsnake-next-column)
            (define-key map (kbd "C-<left>") #'bw/vsnake-prev-column)

            ;; Manual resize
            (define-key map (kbd "=") #'bw/vsnake-widen-columns)
            (define-key map (kbd "-") #'bw/vsnake-narrow-columns)
            (define-key map (kbd "+") #'bw/vsnake-add-column)
            (define-key map (kbd "_") #'bw/vsnake-remove-column)
            (define-key map (kbd "r") #'bw/vsnake-recalculate)

            ;; Top/bottom
            (define-key map (kbd "g") nil)
            (define-key map (kbd "g g") #'beginning-of-buffer)
            (define-key map (kbd "G") #'end-of-buffer)

            ;; Search
            (define-key map (kbd "/") #'isearch-forward)
            (define-key map (kbd "?") #'isearch-backward)
            (define-key map (kbd "C-s") #'isearch-forward)
            (define-key map (kbd "C-r") #'isearch-backward)

            ;; Mouse wheel scrolling (scrolls view, synchronized across all columns)
            (define-key map (kbd "<mouse-4>") #'bw/vsnake-scroll-view-up)
            (define-key map (kbd "<mouse-5>") #'bw/vsnake-scroll-view-down)
            (define-key map (kbd "<wheel-up>") #'bw/vsnake-scroll-view-up)
            (define-key map (kbd "<wheel-down>") #'bw/vsnake-scroll-view-down)

            map))

(defun bw/vsnake-toggle ()
  "Toggle newspaper-style column reading mode for current buffer.
Splits the frame into side-by-side columns showing sequential content.
First column shows lines 1-H, second shows H+1-2H, etc."
  (interactive)
  ;; If already active, exit
  (if (bound-and-true-p bw/vsnake-reading-mode)
      (bw/vsnake-exit)
    ;; Sanity check: buffer must have content
    (when (= (point-min) (point-max))
      (user-error "vsnake: cannot activate on empty buffer"))

    (let ((buf (current-buffer))
          (config (current-window-configuration))
          (pt (point)))
      ;; Save state
      (setq bw/vsnake--saved-config config)
      (setq bw/vsnake--source-buffer buf)

      ;; Get or calculate layout
      (let* ((layout (bw/vsnake-get-layout))
             (num-cols (car layout))
             (col-width (cadr layout)))

        ;; Setup: clear all windows
        (dolist (win (window-list))
          (set-window-dedicated-p win nil)
          (set-window-parameter win 'no-delete-other-windows nil))
        (delete-other-windows)

        ;; Create column layout in middle section
        (setq bw/vsnake--windows (bw/vsnake--create-columns num-cols col-width))
        (setq bw/vsnake--num-cols num-cols)
        (setq bw/vsnake--col-width col-width)
        (setq bw/vsnake--col-height (window-body-height (car bw/vsnake--windows)))

        ;; Hide vertical borders by making them match background
        (set-face-attribute 'vertical-border nil
                            :foreground (face-background 'default))

        ;; Set frame title to show buffer name
        (setq frame-title-format (format "%s [vsnake]" (buffer-name buf)))

        ;; Position columns to show sequential content
        (goto-char pt)
        (bw/vsnake--position-columns)

        ;; Initialize frame width tracking for auto-recalc
        (setq bw/vsnake--last-frame-width (window-total-width))

        ;; Select leftmost column
        (select-window (car bw/vsnake--windows))

        ;; Activate minor mode
        (bw/vsnake-reading-mode 1)

        (message "vsnake: %d columns × %d chars (%s) — q to exit, r to recalc"
                 num-cols col-width bw/vsnake--layout-source)))))

(defun bw/vsnake-exit ()
  "Exit vsnake mode and restore previous window configuration."
  (interactive)
  (when bw/vsnake-reading-mode
    ;; Save current layout before tearing down
    (when (and bw/vsnake--num-cols bw/vsnake--col-width)
      (bw/vsnake-save-layout bw/vsnake--num-cols bw/vsnake--col-width))

    ;; Cancel any pending recalc timer
    (when bw/vsnake--recalc-timer
      (cancel-timer bw/vsnake--recalc-timer)
      (setq bw/vsnake--recalc-timer nil))

    ;; Restore vertical-border face and frame title to defaults
    (set-face-attribute 'vertical-border nil :foreground 'unspecified)
    (setq frame-title-format '("%b"))

    ;; Cleanup
    (bw/vsnake-reading-mode -1)
    (when bw/vsnake--saved-config
      (set-window-configuration bw/vsnake--saved-config)
      (setq bw/vsnake--saved-config nil))

    (message "vsnake: exited (layout saved)")))

;; Alias for the binding in bindings-doom-full.el
(defalias 'bw/vsnake-mode #'bw/vsnake-toggle)

;; --- Hooks ---

(add-hook 'window-size-change-functions
          (lambda (_frame) (bw/vsnake--maybe-auto-recalc)))

(with-eval-after-load 'face-remap
  (add-hook 'text-scale-mode-hook #'bw/vsnake--maybe-auto-recalc))

(provide 'vsnake)
;;; vsnake.el ends here
