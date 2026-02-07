;;; vsnake.el --- Interactive newspaper column layout engine -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive newspaper-column layout engine for Emacs.
;; Content flows left-to-right across columns: col1 shows lines 1-H,
;; col2 shows lines H+1-2H, col3 shows lines 2H+1-3H, etc.
;; All columns have uniform width for consistent visual parsing.
;;
;; The buffer's native keymap handles ALL input — vsnake never shadows
;; buffer-native bindings. Meta-controls live behind a prefix (SPC w V).
;;
;; Implements: vsnake-v1.yaml constraint surface.

;;; Code:

(require 'cl-lib)

;; --- Configuration ---

(defvar bw/vsnake-min-widths
  '((vterm-mode . 50)
    (eshell-mode . 50)
    (term-mode . 50)
    (prog-mode . 35)
    (text-mode . 40)
    (org-mode . 40)
    (markdown-mode . 40)
    (help-mode . 60)
    (Info-mode . 60)
    (Man-mode . 60)
    (default . 35))
  "Mode-specific minimum column widths in characters.")

(defvar bw/vsnake-target-widths
  '((vterm-mode . 80)
    (eshell-mode . 80)
    (term-mode . 80)
    (default . nil))
  "Mode-specific target column widths for process-backed buffers.
nil means use content analysis. Non-nil overrides :effective-width
in layout calculation, producing fewer, wider columns.")

;; --- Buffer-Local State ---

(defvar-local bw/vsnake--saved-layout nil
  "Saved (num-cols col-width) for this buffer.")

(defvar-local bw/vsnake--layout-source nil
  "Symbol: calculated, restored, or manual.")

(defvar-local bw/vsnake--saved-config nil
  "Window configuration saved before entering vsnake.")

(defvar-local bw/vsnake--num-cols nil
  "Current number of columns.")

(defvar-local bw/vsnake--col-width nil
  "Current column width in characters.")

(defvar-local bw/vsnake--col-height nil
  "Current column height in lines.")

(defvar-local bw/vsnake--windows nil
  "List of column windows (left to right).")

(defvar-local bw/vsnake--source-buffer nil
  "The buffer being viewed in vsnake mode.")

(defvar-local bw/vsnake--needs-reposition nil
  "Non-nil when columns need repositioning. [EC-5]")

(defvar-local bw/vsnake--inhibit-watcher nil
  "Non-nil suppresses window-configuration-change detection. [EC-11]")

(defvar-local bw/vsnake--saved-vertical-border nil
  "Saved vertical-border foreground for restoration.")

(defvar-local bw/vsnake--saved-frame-title nil
  "Saved frame-title-format for restoration.")

;; Forward declaration
(defvar bw/vsnake-mode)

;; --- Content Analysis ---

(defun bw/vsnake--sample-line-lengths (&optional max-lines)
  "Return list of line lengths from buffer (up to MAX-LINES)."
  (let ((max-lines (or max-lines 10000))
        (lengths '()))
    (save-excursion
      (goto-char (point-min))
      (dotimes (_ max-lines)
        (unless (eobp)
          (let ((len (- (line-end-position) (line-beginning-position))))
            (when (>= len 10)
              (push len lengths)))
          (forward-line 1))))
    (nreverse lengths)))

(defun bw/vsnake--percentile (data p)
  "Return the Pth percentile of DATA (0.0 to 1.0)."
  (when (null data)
    (error "vsnake--percentile: empty data list"))
  (let* ((sorted (sort (copy-sequence data) #'<))
         (len (length sorted))
         (index (if (= len 1) 0 (floor (* p (1- len))))))
    (nth index sorted)))

(defun bw/vsnake--mode-bin (data bin-width)
  "Return center of most common bin in DATA with BIN-WIDTH."
  (let ((bins (make-hash-table :test 'equal)))
    (dolist (val data)
      (let ((bin (/ val bin-width)))
        (puthash bin (1+ (gethash bin bins 0)) bins)))
    (let ((max-count 0)
          (max-bin 0))
      (maphash (lambda (bin count)
                 (when (> count max-count)
                   (setq max-count count
                         max-bin bin)))
               bins)
      (+ (* max-bin bin-width) (/ bin-width 2)))))

(defun bw/vsnake--mode-min-width ()
  "Return minimum column width for current major mode."
  (let ((mode-entry (or (cl-find-if (lambda (entry)
                                      (derived-mode-p (car entry)))
                                    bw/vsnake-min-widths)
                        (assq 'default bw/vsnake-min-widths))))
    (cdr mode-entry)))

(defun bw/vsnake--mode-target-width ()
  "Return target column width for current major mode, or nil."
  (let ((mode-entry (or (cl-find-if (lambda (entry)
                                      (derived-mode-p (car entry)))
                                    bw/vsnake-target-widths)
                        (assq 'default bw/vsnake-target-widths))))
    (when mode-entry (cdr mode-entry))))

(defun bw/vsnake--analyze-content ()
  "Analyze buffer content and return metrics plist.
Skipped for process-backed buffers per EC-7."
  (let* ((min-width (bw/vsnake--mode-min-width))
         (lengths (bw/vsnake--sample-line-lengths 10000))
         (p75 (if (and lengths (> (length lengths) 0))
                  (max min-width (bw/vsnake--percentile lengths 0.75))
                60))
         (mode-bin (if (and lengths (> (length lengths) 0))
                       (bw/vsnake--mode-bin lengths 10)
                     55))
         (target-width (floor (* p75 1.1)))
         (effective-width (if (and (> mode-bin 20) (> target-width (+ mode-bin 30)))
                              (+ mode-bin 20)
                            target-width)))
    (setq effective-width (max min-width (min effective-width 80)))
    (list :effective-width effective-width :min-width min-width)))

;; --- Layout Calculation ---

(defun bw/vsnake-calculate-uniform-layout (frame-width content-metrics)
  "Return (num-cols col-width) with all columns equal width."
  (let* ((available (- frame-width 6))
         (target (plist-get content-metrics :effective-width))
         (min-width (plist-get content-metrics :min-width))
         (num-cols (max 1 (floor (/ available (+ target 1)))))
         (col-width (if (> num-cols 1)
                        (floor (/ (- available (1- num-cols)) num-cols))
                      available)))
    (while (and (> num-cols 1) (< col-width min-width))
      (setq num-cols (1- num-cols))
      (setq col-width (if (> num-cols 1)
                          (floor (/ (- available (1- num-cols)) num-cols))
                        available)))
    (when (< col-width min-width)
      (if (>= available min-width)
          (setq col-width min-width)
        (error "vsnake: frame-width %d too narrow (need ≥%d)"
               frame-width (+ min-width 7))))
    (list num-cols col-width)))

(defun bw/vsnake-get-layout ()
  "Return (num-cols col-width) for current buffer.
For process-backed buffers, uses min-width directly per EC-7."
  (if bw/vsnake--saved-layout
      (progn
        (setq bw/vsnake--layout-source 'restored)
        bw/vsnake--saved-layout)
    (let* ((process-backed (get-buffer-process (current-buffer)))
           (metrics (if process-backed
                        (let* ((mw (bw/vsnake--mode-min-width))
                               (tw (or (bw/vsnake--mode-target-width) mw)))
                          (list :effective-width tw :min-width mw))
                      (bw/vsnake--analyze-content)))
           (layout (bw/vsnake-calculate-uniform-layout
                    (frame-width) metrics)))
      (setq bw/vsnake--layout-source 'calculated)
      layout)))

(defun bw/vsnake-save-layout (num-cols col-width)
  "Save layout configuration for this buffer."
  (setq bw/vsnake--saved-layout (list num-cols col-width)))

;; --- Column Display ---

(defun bw/vsnake--create-columns (num-cols col-width)
  "Create NUM-COLS side-by-side windows of COL-WIDTH.
Returns list of windows (left to right)."
  (let ((windows (list (selected-window))))
    (dotimes (_ (1- num-cols))
      (let ((new-win (split-window-right col-width)))
        (push new-win windows)
        (select-window new-win)))
    (balance-windows)
    (nreverse windows)))

(defun bw/vsnake--position-columns-from (start-pos)
  "Position all columns sequentially starting from START-POS.
Uses two-pass: rough estimate then refine with window-end.
Initial activation only — uses (redisplay t) for accurate window-end."
  (let ((next-start-pos start-pos))
    ;; Pass 1: rough positions
    (dotimes (i (length bw/vsnake--windows))
      (let ((win (nth i bw/vsnake--windows)))
        (set-window-buffer win bw/vsnake--source-buffer)
        (set-window-start win next-start-pos)
        (with-selected-window win
          (set-window-fringes win 0 0)
          (set-window-margins win 0 0)
          (setq truncate-lines nil
                word-wrap t)
          (setq mode-line-format
                (when (zerop i)
                  '(:eval (format " %s  L%d  [vsnake: %d×%d]  "
                                  mode-name
                                  (line-number-at-pos (point))
                                  bw/vsnake--num-cols
                                  bw/vsnake--col-width))))
          (setq header-line-format nil))
        (setq next-start-pos
              (with-selected-window win
                (save-excursion
                  (goto-char next-start-pos)
                  (vertical-motion (window-body-height win))
                  (point))))))
    ;; Pass 2: refine positions after rendering
    (redisplay t)
    (if (get-buffer-process bw/vsnake--source-buffer)
        ;; Tail-pin: reverse-chain from last column so point-max is at bottom
        (let ((rev-wins (reverse bw/vsnake--windows)))
          (with-selected-window (car rev-wins)
            (save-excursion
              (goto-char (point-max))
              (vertical-motion (- (window-body-height (car rev-wins))))
              (set-window-start (car rev-wins) (point))))
          (dotimes (i (1- (length rev-wins)))
            (let* ((this-win (nth i rev-wins))
                   (prev-win (nth (1+ i) rev-wins)))
              (with-selected-window prev-win
                (save-excursion
                  (goto-char (window-start this-win))
                  (vertical-motion (- (window-body-height prev-win)))
                  (let ((computed (point)))
                    ;; If not enough content to differentiate, show empty
                    (if (>= computed (window-start this-win))
                        (set-window-start prev-win (point-max))
                      (set-window-start prev-win computed))))))))
      ;; Normal: forward-chain with accurate window-end
      (dotimes (i (1- (length bw/vsnake--windows)))
        (let* ((win (nth i bw/vsnake--windows))
               (next-win (nth (1+ i) bw/vsnake--windows))
               (actual-end (window-end win t)))
          (set-window-start next-win actual-end))))))

(defun bw/vsnake--compute-start-for-point (pt &optional tail-pin)
  "Compute the column-1 start position so that PT is visible.
When TAIL-PIN is non-nil, positions so PT appears in the last column
\(for process buffers). Otherwise centers PT. [OC-5]
Uses vertical-motion to count screen lines (respects word-wrap)."
  (let* ((win (car bw/vsnake--windows))
         (col-height (or bw/vsnake--col-height
                         (window-body-height win)))
         (num-cols (length bw/vsnake--windows))
         (lines-before (if tail-pin
                            (* col-height num-cols)
                          (* col-height (/ num-cols 2)))))
    (with-selected-window win
      (save-excursion
        (goto-char pt)
        (vertical-motion (- lines-before))
        (point)))))

(defun bw/vsnake--reposition-columns ()
  "Chain-update all column window-starts using vertical-motion.
For process buffers, reverse-chains from last column (tail-pinned to
point-max). For file buffers, forward-chains from column 1.
Never calls (redisplay t). [EC-5, EC-6]"
  (when (and bw/vsnake--windows
             (window-live-p (car bw/vsnake--windows)))
    (if (get-buffer-process (current-buffer))
        ;; Process buffers: reverse-chain from last column
        ;; so point-max stays at bottom of rightmost column
        (let ((rev-wins (reverse bw/vsnake--windows)))
          ;; Anchor last column: point-max at bottom
          (let ((last-win (car rev-wins)))
            (when (window-live-p last-win)
              (with-selected-window last-win
                (save-excursion
                  (goto-char (point-max))
                  (vertical-motion (- (window-body-height last-win)))
                  (set-window-start last-win (point))))))
          ;; Chain backwards — if not enough content, left cols show empty
          (dotimes (i (1- (length rev-wins)))
            (let* ((this-win (nth i rev-wins))
                   (prev-win (nth (1+ i) rev-wins)))
              (when (and (window-live-p this-win) (window-live-p prev-win))
                (with-selected-window prev-win
                  (save-excursion
                    (goto-char (window-start this-win))
                    (vertical-motion (- (window-body-height prev-win)))
                    (let ((computed (point)))
                      (if (>= computed (window-start this-win))
                          (set-window-start prev-win (point-max))
                        (set-window-start prev-win computed)))))))))
      ;; File buffers: forward-chain from column 1
      (dotimes (i (1- (length bw/vsnake--windows)))
        (let* ((win (nth i bw/vsnake--windows))
               (next-win (nth (1+ i) bw/vsnake--windows)))
          (when (and (window-live-p win) (window-live-p next-win))
            (let ((next-start
                   (with-selected-window win
                     (save-excursion
                       (goto-char (window-start win))
                       (vertical-motion (window-body-height win))
                       (point)))))
              (set-window-start next-win next-start))))))))

(defun bw/vsnake--reposition-columns-around (pt)
  "Reposition all columns so that buffer position PT is visible.
Used for large jumps where PT is outside all columns. [EC-10 case c]"
  (when bw/vsnake--windows
    (let ((start-pos (bw/vsnake--compute-start-for-point pt)))
      (set-window-start (car bw/vsnake--windows) start-pos)
      ;; Chain remaining columns using vertical-motion
      (let ((next-pos start-pos))
        (dotimes (i (length bw/vsnake--windows))
          (let ((win (nth i bw/vsnake--windows)))
            (when (window-live-p win)
              (set-window-start win next-pos)
              (setq next-pos
                    (with-selected-window win
                      (save-excursion
                        (goto-char next-pos)
                        (vertical-motion (window-body-height win))
                        (point)))))))))))

(defun bw/vsnake--find-column-containing-pos (pos)
  "Return index of column containing POS, or nil."
  (let ((idx 0) (found nil))
    (while (and (< idx (length bw/vsnake--windows)) (not found))
      (let* ((win (nth idx bw/vsnake--windows))
             (ws (window-start win))
             (we (window-end win)))
        (when (and (>= pos ws) (<= pos we))
          (setq found idx)))
      (setq idx (1+ idx)))
    found))

(defun bw/vsnake--cursor-column ()
  "Return the rightmost (terminal-cursor) column window."
  (car (last bw/vsnake--windows)))

;; --- Rebuild ---

(defun bw/vsnake--rebuild-columns (num-cols col-width)
  "Rebuild column layout with new dimensions."
  (let ((bw/vsnake--inhibit-watcher t)
        (buf (current-buffer))
        (pt (point)))
    (dolist (win (window-list))
      (unless (window-parameter win 'window-side)
        (set-window-dedicated-p win nil)
        (set-window-parameter win 'no-delete-other-windows nil)))
    (delete-other-windows)
    (setq bw/vsnake--windows (bw/vsnake--create-columns num-cols col-width))
    (setq bw/vsnake--num-cols num-cols
          bw/vsnake--col-width col-width
          bw/vsnake--col-height (window-body-height (car bw/vsnake--windows))
          bw/vsnake--source-buffer buf)
    (let ((start-pos (bw/vsnake--compute-start-for-point pt)))
      (bw/vsnake--position-columns-from start-pos))
    ;; Select column containing point
    (let ((col-idx (bw/vsnake--find-column-containing-pos pt)))
      (when col-idx
        (select-window (nth col-idx bw/vsnake--windows))))
    (goto-char pt)))

;; --- Hooks: Synchronous Content Flow [EC-2, EC-5] ---

(defun bw/vsnake--after-change (_beg _end _len)
  "Set dirty flag on buffer modification. [EC-5a]"
  (when (bound-and-true-p bw/vsnake-mode)
    (setq bw/vsnake--needs-reposition t)))

(defun bw/vsnake--pre-redisplay (_wins)
  "Reposition columns and pin cursor column for process buffers.
Runs on every pre-redisplay for process-backed buffers (vterm modifies
buffer via C, bypassing after-change-functions). [EC-5b, EC-9]"
  (when (and (bound-and-true-p bw/vsnake-mode)
             bw/vsnake--windows
             (window-live-p (car bw/vsnake--windows)))
    (let ((proc (get-buffer-process (current-buffer))))
      ;; For process-backed buffers, always reposition (vterm bypasses
      ;; after-change-functions). For others, only when dirty flag is set.
      (when (or proc bw/vsnake--needs-reposition)
        (setq bw/vsnake--needs-reposition nil)
        (bw/vsnake--reposition-columns)
        ;; EC-9: keep cursor column's window-point at process mark
        (when proc
          (let ((cursor-win (bw/vsnake--cursor-column))
                (pmark (process-mark proc)))
            (when (and (window-live-p cursor-win)
                       (not (eq cursor-win (selected-window))))
              (set-window-point cursor-win pmark))))))))

;; --- Hook: Point-Leaves-Viewport [EC-10] ---

(defun bw/vsnake--post-command ()
  "After each command, ensure selected window contains point. [EC-10]"
  (when (and (bound-and-true-p bw/vsnake-mode)
             bw/vsnake--windows)
    (let* ((pt (point))
           (sel (selected-window))
           (ws (window-start sel))
           (we (window-end sel)))
      ;; Case (a): point inside selected window — no action
      (unless (and (>= pt ws) (<= pt we))
        ;; Case (b): point inside a different column
        (let ((col-idx (bw/vsnake--find-column-containing-pos pt)))
          (if col-idx
              (select-window (nth col-idx bw/vsnake--windows))
            ;; Case (c): point outside all columns — reposition around point
            (bw/vsnake--reposition-columns-around pt)
            (let ((new-idx (bw/vsnake--find-column-containing-pos pt)))
              (when new-idx
                (select-window (nth new-idx bw/vsnake--windows))))))))))

;; --- Hook: Window Config Watcher [EC-11] ---

(defun bw/vsnake--window-config-changed ()
  "Exit vsnake if window configuration changed unexpectedly. [EC-11]"
  (when (and (bound-and-true-p bw/vsnake-mode)
             (not bw/vsnake--inhibit-watcher)
             bw/vsnake--windows)
    (let ((expected bw/vsnake--windows)
          (actual (window-list nil 'no-minibuffer)))
      ;; Check: are all expected windows still present?
      (unless (cl-every (lambda (w) (memq w actual)) expected)
        (bw/vsnake-exit)))))

;; --- vterm Point Protection [EC-8] ---

(defun bw/vsnake--protect-vterm-redraw (orig-fn &rest args)
  "Prevent vterm--redraw from moving point in non-cursor columns. [EC-8]"
  (if (and (bound-and-true-p bw/vsnake-mode)
           bw/vsnake--windows
           (not (eq (selected-window) (bw/vsnake--cursor-column))))
      (let ((saved-pt (window-point (selected-window))))
        (apply orig-fn args)
        (set-window-point (selected-window) saved-pt))
    (apply orig-fn args)))

(with-eval-after-load 'vterm
  (advice-add 'vterm--redraw :around #'bw/vsnake--protect-vterm-redraw))

;; --- Manual Resize Commands ---

(defun bw/vsnake-widen-columns (delta)
  "Increase all column widths by DELTA chars."
  (interactive "p")
  (let* ((new-col-width (+ bw/vsnake--col-width delta))
         (available (- (frame-width) 6))
         (max-cols (max 1 (floor (/ (- available (1- bw/vsnake--num-cols))
                                     new-col-width))))
         (new-num-cols (min bw/vsnake--num-cols max-cols)))
    (bw/vsnake-save-layout new-num-cols new-col-width)
    (setq bw/vsnake--layout-source 'manual)
    (bw/vsnake--rebuild-columns new-num-cols new-col-width)
    (message "vsnake: %d cols × %d chars (manual)" new-num-cols new-col-width)))

(defun bw/vsnake-narrow-columns (delta)
  "Decrease all column widths by DELTA chars."
  (interactive "p")
  (let* ((min-width (bw/vsnake--mode-min-width))
         (new-col-width (max min-width (- bw/vsnake--col-width delta)))
         (available (- (frame-width) 6))
         (new-num-cols (max 1 (floor (/ available (+ new-col-width 1))))))
    (bw/vsnake-save-layout new-num-cols new-col-width)
    (setq bw/vsnake--layout-source 'manual)
    (bw/vsnake--rebuild-columns new-num-cols new-col-width)
    (message "vsnake: %d cols × %d chars (manual)" new-num-cols new-col-width)))

(defun bw/vsnake-add-column ()
  "Add one more column if frame width permits."
  (interactive)
  (let* ((new-num-cols (1+ bw/vsnake--num-cols))
         (available (- (frame-width) 6))
         (new-col-width (floor (/ (- available (1- new-num-cols)) new-num-cols)))
         (min-width (bw/vsnake--mode-min-width)))
    (if (< new-col-width min-width)
        (message "vsnake: cannot add column (min-width %d)" min-width)
      (bw/vsnake-save-layout new-num-cols new-col-width)
      (setq bw/vsnake--layout-source 'manual)
      (bw/vsnake--rebuild-columns new-num-cols new-col-width)
      (message "vsnake: %d cols × %d chars" new-num-cols new-col-width))))

(defun bw/vsnake-remove-column ()
  "Remove one column (minimum 1)."
  (interactive)
  (when (<= bw/vsnake--num-cols 1)
    (user-error "vsnake: cannot remove last column"))
  (let* ((new-num-cols (1- bw/vsnake--num-cols))
         (available (- (frame-width) 6))
         (new-col-width (floor (/ (- available (1- new-num-cols)) new-num-cols))))
    (bw/vsnake-save-layout new-num-cols new-col-width)
    (setq bw/vsnake--layout-source 'manual)
    (bw/vsnake--rebuild-columns new-num-cols new-col-width)
    (message "vsnake: %d cols × %d chars" new-num-cols new-col-width)))

(defun bw/vsnake-recalculate ()
  "Recalculate column layout from scratch."
  (interactive)
  (unless (bound-and-true-p bw/vsnake-mode)
    (user-error "Not in vsnake mode"))
  (let* ((bw/vsnake--saved-layout nil)
         (process-backed (get-buffer-process (current-buffer)))
         (metrics (if process-backed
                      (let ((mw (bw/vsnake--mode-min-width)))
                        (list :effective-width mw :min-width mw))
                    (bw/vsnake--analyze-content)))
         (layout (bw/vsnake-calculate-uniform-layout
                  (frame-width) metrics))
         (num-cols (car layout))
         (col-width (cadr layout)))
    (bw/vsnake-save-layout num-cols col-width)
    (setq bw/vsnake--layout-source 'calculated)
    (bw/vsnake--rebuild-columns num-cols col-width)
    (message "vsnake: recalculated %d cols × %d chars" num-cols col-width)))

;; --- Prefix Keymap [OC-2] ---

(defvar bw/vsnake-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "V") #'bw/vsnake-toggle)
    (define-key map (kbd "q") #'bw/vsnake-exit)
    (define-key map (kbd "+") #'bw/vsnake-add-column)
    (define-key map (kbd "-") #'bw/vsnake-remove-column)
    (define-key map (kbd "=") #'bw/vsnake-widen-columns)
    (define-key map (kbd "_") #'bw/vsnake-narrow-columns)
    (define-key map (kbd "r") #'bw/vsnake-recalculate)
    map)
  "Prefix keymap for vsnake meta-controls.
Bound to SPC w V in bindings-doom-full.el.")

;; --- Mode Definition [OC-4] ---

(define-minor-mode bw/vsnake-mode
  "Interactive newspaper-column layout mode.
Purely geometric — buffer's native keymap handles all input."
  :lighter " vsnake"
  :keymap (make-sparse-keymap)  ; empty keymap per OC-4
  (if bw/vsnake-mode
      (progn
        (add-hook 'after-change-functions #'bw/vsnake--after-change nil t)
        (add-hook 'pre-redisplay-functions #'bw/vsnake--pre-redisplay nil t)
        (add-hook 'post-command-hook #'bw/vsnake--post-command nil t)
        (add-hook 'window-configuration-change-hook
                  #'bw/vsnake--window-config-changed nil t))
    (remove-hook 'after-change-functions #'bw/vsnake--after-change t)
    (remove-hook 'pre-redisplay-functions #'bw/vsnake--pre-redisplay t)
    (remove-hook 'post-command-hook #'bw/vsnake--post-command t)
    (remove-hook 'window-configuration-change-hook
                 #'bw/vsnake--window-config-changed t)))

;; --- Toggle / Exit ---

(defun bw/vsnake-toggle ()
  "Toggle interactive newspaper-column layout for current buffer."
  (interactive)
  (if (bound-and-true-p bw/vsnake-mode)
      (bw/vsnake-exit)
    (when (= (point-min) (point-max))
      (user-error "vsnake: cannot activate on empty buffer"))
    ;; CL-4: verify vterm advice is installed
    (when (and (derived-mode-p 'vterm-mode)
               (fboundp 'vterm--redraw)
               (not (advice-member-p #'bw/vsnake--protect-vterm-redraw
                                     'vterm--redraw)))
      (message "vsnake: WARNING — vterm--redraw advice not installed, scrollback browsing may glitch"))
    (let ((bw/vsnake--inhibit-watcher t)
          (buf (current-buffer))
          (config (current-window-configuration))
          (pt (point)))
      (setq bw/vsnake--saved-config config
            bw/vsnake--source-buffer buf)
      ;; Save frame state for clean restoration [EC-3]
      (setq bw/vsnake--saved-vertical-border
            (face-attribute 'vertical-border :foreground nil t))
      (setq bw/vsnake--saved-frame-title frame-title-format)
      (let* ((layout (bw/vsnake-get-layout))
             (num-cols (car layout))
             (col-width (cadr layout)))
        ;; Clear and rebuild — preserve side windows (keyhints etc.)
        (dolist (win (window-list))
          (unless (window-parameter win 'window-side)
            (set-window-dedicated-p win nil)
            (set-window-parameter win 'no-delete-other-windows nil)))
        (delete-other-windows)
        (setq bw/vsnake--windows (bw/vsnake--create-columns num-cols col-width)
              bw/vsnake--num-cols num-cols
              bw/vsnake--col-width col-width
              bw/vsnake--col-height (window-body-height (car bw/vsnake--windows)))
        ;; Hide vertical borders
        (set-face-attribute 'vertical-border nil
                            :foreground (face-background 'default))
        (setq frame-title-format (format "%s [vsnake]" (buffer-name buf)))
        ;; Position columns: process buffers pin to tail, others anchor on point [OC-5]
        (let* ((proc (get-buffer-process buf))
               (anchor (if proc (point-max) pt))
               (start-pos (bw/vsnake--compute-start-for-point anchor proc)))
          (bw/vsnake--position-columns-from start-pos))
        ;; Select appropriate column: process buffers always use cursor column
        (let ((proc (get-buffer-process buf)))
          (if proc
              (progn
                (select-window (bw/vsnake--cursor-column))
                (goto-char (point-max)))
            (let ((col-idx (bw/vsnake--find-column-containing-pos pt)))
              (when col-idx
                (select-window (nth col-idx bw/vsnake--windows))))
            (goto-char pt)))
        ;; Activate minor mode (installs hooks)
        (bw/vsnake-mode 1)
        (message "vsnake: %d cols × %d chars (%s) — SPC w V for controls"
                 num-cols col-width bw/vsnake--layout-source)))))

(defun bw/vsnake-exit ()
  "Exit vsnake and restore previous window configuration."
  (interactive)
  (when (bound-and-true-p bw/vsnake-mode)
    ;; Save layout for next activation
    (when (and bw/vsnake--num-cols bw/vsnake--col-width)
      (bw/vsnake-save-layout bw/vsnake--num-cols bw/vsnake--col-width))
    ;; Restore frame state [EC-3]
    (when bw/vsnake--saved-vertical-border
      (set-face-attribute 'vertical-border nil
                          :foreground bw/vsnake--saved-vertical-border))
    (when bw/vsnake--saved-frame-title
      (setq frame-title-format bw/vsnake--saved-frame-title))
    ;; Deactivate minor mode (removes hooks)
    (bw/vsnake-mode -1)
    ;; Restore window config
    (when bw/vsnake--saved-config
      (set-window-configuration bw/vsnake--saved-config)
      (setq bw/vsnake--saved-config nil))
    (setq bw/vsnake--windows nil)
    (message "vsnake: exited (layout saved)")))

;; Keep backward compat alias
(defalias 'bw/vsnake-reading-mode #'bw/vsnake-mode)

(provide 'vsnake)
;;; vsnake.el ends here
