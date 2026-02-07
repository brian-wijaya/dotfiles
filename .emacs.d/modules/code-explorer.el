;;; code-explorer.el --- Sibling-continuous depth-zoom file browser -*- lexical-binding: t; -*-

;;; Commentary:
;; Two-pane file browser: left navigator (bands of directory groups) +
;; right display pane (real editor buffer with vsnake adaptive width).
;; l/h change depth, j/k move through entries crossing group boundaries.
;; Spec: ~/.claude/e2e-stories/code-explorer-v0.yaml

;;; Code:

(require 'cl-lib)
(require 'vsnake)

;; ═══════════════════════════════════════════════════════════════════
;; Data Structures
;; ═══════════════════════════════════════════════════════════════════

(cl-defstruct bw/ce-group
  dir           ; absolute path of the directory
  entries       ; list of (name full-path is-dir) tuples
  start-line    ; line number where group begins in navigator
  end-line)     ; line number where group ends

;; ═══════════════════════════════════════════════════════════════════
;; Configuration
;; ═══════════════════════════════════════════════════════════════════

(defvar bw/ce-binary-exts
  '("png" "jpg" "jpeg" "gif" "bmp" "ico" "svg" "webp"
    "mp3" "mp4" "mkv" "avi" "mov" "flac" "wav" "ogg"
    "zip" "tar" "gz" "bz2" "xz" "7z" "rar"
    "pdf" "doc" "docx" "xls" "xlsx" "ppt" "pptx"
    "so" "o" "a" "dylib" "exe" "dll" "class" "pyc" "elc")
  "File extensions treated as binary (no preview).")

(defvar bw/ce-max-file-size (* 1024 1024)
  "Maximum file size in bytes for auto-preview (1MB).")

(defvar bw/ce-temp-buffer-limit 8
  "Maximum number of temp preview buffers before killing oldest.")

(defvar bw/ce-sort-cache-file
  (expand-file-name "code-explorer-sort.el" "~/.cache/")
  "File for persisting sort overrides across sessions.")

;; ═══════════════════════════════════════════════════════════════════
;; Buffer-Local State
;; ═══════════════════════════════════════════════════════════════════

(defvar-local bw/ce--display-window nil "Right-side display window.")
(defvar-local bw/ce--display-buffer nil "Current display buffer.")
(defvar-local bw/ce--pinned-file nil "Path of pinned file, or nil.")
(defvar-local bw/ce--depth 0 "Current depth (0 = initial).")
(defvar-local bw/ce--band nil "List of bw/ce-group structs for current band.")
(defvar-local bw/ce--band-parents nil "List of parent dirs that generated current band.")
(defvar-local bw/ce--start-dir nil "Directory where explorer was invoked.")
(defvar-local bw/ce--history nil "List of history snapshots for back/forward.")
(defvar-local bw/ce--history-index -1 "Position in history (-1 = head).")
(defvar-local bw/ce--marks (make-hash-table :test 'equal) "Global marks: path → t.")
(defvar-local bw/ce--show-hidden t "Whether to show dotfiles.")
(defvar-local bw/ce--temp-buffers nil "List of temp preview buffers.")
(defvar-local bw/ce--sort-overrides (make-hash-table :test 'equal) "Dir → sort-method.")
(defvar-local bw/ce--global-sort 'alpha-dirs-first "Default sort method.")
(defvar-local bw/ce--last-previewed nil "Path of last previewed file.")
(defvar-local bw/ce--wall-overlay nil "Temporary wall-hit overlay.")
(defvar-local bw/ce--wall-timer nil "Timer for wall-hit flash removal.")
(defvar-local bw/ce--prev-buffer nil "Buffer that was active before explorer.")

;; ═══════════════════════════════════════════════════════════════════
;; Sort System
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--sort-method-for-dir (dir)
  "Get sort method for DIR: check overrides, then global default."
  (or (gethash dir bw/ce--sort-overrides)
      bw/ce--global-sort))

(defun bw/ce--compare-alpha-dirs-first (a b)
  "Compare entries A B: directories first, then case-insensitive alpha."
  (let ((a-dir (nth 2 a)) (b-dir (nth 2 b))
        (a-name (downcase (nth 0 a))) (b-name (downcase (nth 0 b))))
    (cond
     ((and a-dir (not b-dir)) t)
     ((and (not a-dir) b-dir) nil)
     (t (string< a-name b-name)))))

(defun bw/ce--compare-date (a b)
  "Compare entries A B by modification time, dirs first."
  (let ((a-dir (nth 2 a)) (b-dir (nth 2 b)))
    (cond
     ((and a-dir (not b-dir)) t)
     ((and (not a-dir) b-dir) nil)
     (t (let ((a-time (nth 5 (file-attributes (nth 1 a))))
              (b-time (nth 5 (file-attributes (nth 1 b)))))
          (time-less-p b-time a-time))))))

(defun bw/ce--compare-size (a b)
  "Compare entries A B by size (largest first), dirs first."
  (let ((a-dir (nth 2 a)) (b-dir (nth 2 b)))
    (cond
     ((and a-dir (not b-dir)) t)
     ((and (not a-dir) b-dir) nil)
     (t (let ((a-size (or (nth 7 (file-attributes (nth 1 a))) 0))
              (b-size (or (nth 7 (file-attributes (nth 1 b))) 0)))
          (> a-size b-size))))))

(defun bw/ce--compare-type (a b)
  "Compare entries A B by extension, dirs first."
  (let ((a-dir (nth 2 a)) (b-dir (nth 2 b)))
    (cond
     ((and a-dir (not b-dir)) t)
     ((and (not a-dir) b-dir) nil)
     (t (let ((a-ext (or (file-name-extension (nth 0 a)) ""))
              (b-ext (or (file-name-extension (nth 0 b)) "")))
          (if (string= a-ext b-ext)
              (string< (downcase (nth 0 a)) (downcase (nth 0 b)))
            (string< (downcase a-ext) (downcase b-ext))))))))

(defun bw/ce--sort-entries (entries method)
  "Sort ENTRIES by METHOD. Each entry is (name full-path is-dir)."
  (let ((sorted (copy-sequence entries)))
    (sort sorted
          (pcase method
            ('alpha-dirs-first #'bw/ce--compare-alpha-dirs-first)
            ('date-modified #'bw/ce--compare-date)
            ('size #'bw/ce--compare-size)
            ('type-ext #'bw/ce--compare-type)
            (_ #'bw/ce--compare-alpha-dirs-first)))))

(defun bw/ce--load-sort-cache ()
  "Load sort overrides from cache file."
  (when (file-exists-p bw/ce-sort-cache-file)
    (condition-case nil
        (let ((data (with-temp-buffer
                      (insert-file-contents bw/ce-sort-cache-file)
                      (read (current-buffer)))))
          (when (hash-table-p data)
            (setq bw/ce--sort-overrides data)))
      (error nil))))

(defun bw/ce--save-sort-cache ()
  "Save sort overrides to cache file."
  (when (> (hash-table-count bw/ce--sort-overrides) 0)
    (with-temp-file bw/ce-sort-cache-file
      (prin1 bw/ce--sort-overrides (current-buffer)))))

;; ═══════════════════════════════════════════════════════════════════
;; Directory Scanning
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--scan-directory (dir)
  "Scan DIR and return list of (name full-path is-dir) entries.
Respects `bw/ce--show-hidden`. Filters . and .."
  (condition-case nil
      (let ((files (directory-files dir t nil t))
            (result nil))
        (dolist (f files)
          (let ((name (file-name-nondirectory f)))
            (unless (or (string= name ".") (string= name "..")
                        (and (not bw/ce--show-hidden)
                             (string-prefix-p "." name)))
              (push (list name f (file-directory-p f)) result))))
        (nreverse result))
    (file-error
     (list (list "[Permission denied]" dir nil)))))

(defun bw/ce--compute-initial-band (start-dir)
  "Compute initial band: children of START-DIR's parent as groups."
  (let* ((parent (file-name-directory (directory-file-name start-dir)))
         (siblings (bw/ce--scan-directory parent))
         (sort-method (bw/ce--sort-method-for-dir parent))
         (dirs-only (cl-remove-if-not (lambda (e) (nth 2 e)) siblings))
         (sorted-dirs (bw/ce--sort-entries dirs-only sort-method))
         (groups nil))
    (dolist (dir-entry sorted-dirs)
      (let* ((dir-path (nth 1 dir-entry))
             (entries (bw/ce--scan-directory dir-path))
             (group-sort (bw/ce--sort-method-for-dir dir-path))
             (sorted (bw/ce--sort-entries entries group-sort)))
        (push (make-bw/ce-group :dir dir-path :entries sorted
                                :start-line 0 :end-line 0)
              groups)))
    (setq bw/ce--band-parents (list parent))
    (nreverse groups)))

(defun bw/ce--compute-deeper-band (current-band)
  "Compute band at depth+1: expand all directories in CURRENT-BAND."
  (catch 'empty
  (let ((all-dirs nil)
        (parent-dirs nil))
    ;; Collect all directory entries across all groups
    (dolist (group current-band)
      (let ((group-dir (bw/ce-group-dir group)))
        (dolist (entry (bw/ce-group-entries group))
          (when (nth 2 entry)
            (push (cons (nth 1 entry) group-dir) all-dirs)))))
    (unless all-dirs (throw 'empty nil))
    ;; Build new groups, preserving parent ordering
    (let ((groups nil))
      (dolist (dir-pair (nreverse all-dirs))
        (let* ((dir-path (car dir-pair))
               (parent-dir (cdr dir-pair))
               (entries (bw/ce--scan-directory dir-path))
               (group-sort (bw/ce--sort-method-for-dir dir-path))
               (sorted (bw/ce--sort-entries entries group-sort)))
          (push parent-dir parent-dirs)
          (push (make-bw/ce-group :dir dir-path :entries sorted
                                  :start-line 0 :end-line 0)
                groups)))
      (setq bw/ce--band-parents (delete-dups (nreverse parent-dirs)))
      (nreverse groups)))))

(defun bw/ce--compute-shallower-band (current-band)
  "Compute band at depth-1: go up to parent directories."
  (let* ((current-dirs (mapcar #'bw/ce-group-dir current-band))
         (parents (delete-dups
                   (mapcar (lambda (d)
                             (file-name-directory (directory-file-name d)))
                           current-dirs)))
         (grandparent (when (= (length parents) 1)
                        (file-name-directory
                         (directory-file-name (car parents)))))
         ;; If all groups share one parent, go up one more level to show siblings
         (target-parent (or grandparent
                            (file-name-directory
                             (directory-file-name (car parents)))))
         (siblings (bw/ce--scan-directory target-parent))
         (sort-method (bw/ce--sort-method-for-dir target-parent))
         (dirs-only (cl-remove-if-not (lambda (e) (nth 2 e)) siblings))
         (sorted-dirs (bw/ce--sort-entries dirs-only sort-method))
         (groups nil))
    (dolist (dir-entry sorted-dirs)
      (let* ((dir-path (nth 1 dir-entry))
             (entries (bw/ce--scan-directory dir-path))
             (group-sort (bw/ce--sort-method-for-dir dir-path))
             (sorted (bw/ce--sort-entries entries group-sort)))
        (push (make-bw/ce-group :dir dir-path :entries sorted
                                :start-line 0 :end-line 0)
              groups)))
    (setq bw/ce--band-parents (list target-parent))
    (nreverse groups)))

;; ═══════════════════════════════════════════════════════════════════
;; Path Truncation
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--truncate-path (path max-width)
  "Truncate PATH to fit within MAX-WIDTH columns.
Preserve ~/prefix and tail. Cut at 1/3 point with ... separator."
  (let ((display (abbreviate-file-name path)))
    (if (<= (string-width display) max-width)
        display
      (let* ((cut-point (max 3 (/ max-width 3)))
             (tail-len (max 1 (- max-width cut-point 3))) ; 3 for "..."
             (head (truncate-string-to-width display cut-point))
             (tail (substring display (max 0 (- (length display) tail-len)))))
        (concat head "..." tail)))))

;; ═══════════════════════════════════════════════════════════════════
;; Navigator Rendering
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--render-band (band)
  "Render BAND (list of groups) into current navigator buffer."
  (let ((inhibit-read-only t)
        (nav-width (window-body-width))
        (line 1)
        (group-idx 0))
    (erase-buffer)
    (dolist (group band)
      ;; Divider between groups
      (when (> group-idx 0)
        (let ((divider (propertize (make-string (max 1 (- nav-width 2)) ?─)
                                   'face '(:foreground "#3f444a")
                                   'bw/ce-divider t)))
          (insert " " divider "\n")
          (cl-incf line)))
      (setf (bw/ce-group-start-line group) line)
      ;; Entries
      (dolist (entry (bw/ce-group-entries group))
        (let* ((name (nth 0 entry))
               (full-path (nth 1 entry))
               (is-dir (nth 2 entry))
               (marked (gethash full-path bw/ce--marks))
               (icon (condition-case nil
                         (if is-dir
                             (nerd-icons-icon-for-dir full-path)
                           (nerd-icons-icon-for-file name))
                       (error "")))
               (suffix (cond
                        (is-dir "/")
                        ((and (not is-dir)
                              (file-symlink-p full-path))
                         (concat " -> " (file-symlink-p full-path)))
                        ((and (not is-dir)
                              (file-executable-p full-path))
                         "*")
                        (t "")))
               (icon-width (string-width icon))
               (display-path (bw/ce--truncate-path full-path
                              (- nav-width 4 icon-width (length suffix))))
               (mark-indicator (if marked "* " "  "))
               (line-text (concat mark-indicator icon " " display-path suffix))
               (face (cond
                      (marked '(:foreground "#ECBE7B"))
                      (is-dir '(:foreground "#51afef"))
                      (t nil))))
          (insert (propertize line-text
                              'bw/ce-path full-path
                              'bw/ce-is-dir is-dir
                              'bw/ce-group-idx group-idx
                              'face face)
                  "\n")
          (cl-incf line)))
      (setf (bw/ce-group-end-line group) (1- line))
      (cl-incf group-idx))
    ;; Remove trailing newline
    (when (> (point-max) (point-min))
      (goto-char (point-max))
      (when (= (char-before) ?\n)
        (delete-char -1)))))

;; ═══════════════════════════════════════════════════════════════════
;; Zone Mapping
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--band-total-entries (band)
  "Count total entries across all groups in BAND."
  (cl-reduce #'+ band :key (lambda (g) (length (bw/ce-group-entries g)))))

(defun bw/ce--cursor-entry-index ()
  "Get the 0-based entry index at point in the current band."
  (let ((line (line-number-at-pos))
        (idx 0))
    (catch 'found
      (dolist (group bw/ce--band)
        (let ((start (bw/ce-group-start-line group))
              (end (bw/ce-group-end-line group)))
          (when (and (>= line start) (<= line end))
            (throw 'found (+ idx (- line start))))
          (setq idx (+ idx (length (bw/ce-group-entries group))))))
      idx)))

(defun bw/ce--goto-entry-index (idx)
  "Move cursor to entry at 0-based IDX across the band."
  (let ((remaining idx))
    (catch 'found
      (dolist (group bw/ce--band)
        (let ((count (length (bw/ce-group-entries group))))
          (if (< remaining count)
              (progn
                (goto-char (point-min))
                (forward-line (1- (+ (bw/ce-group-start-line group) remaining)))
                (throw 'found t))
            (setq remaining (- remaining count)))))
      ;; Fallback: go to last entry
      (goto-char (point-max))
      (beginning-of-line))))

(defun bw/ce--zone-map (old-band new-band)
  "Map cursor proportionally from OLD-BAND to NEW-BAND."
  (let* ((old-total (bw/ce--band-total-entries old-band))
         (old-idx (bw/ce--cursor-entry-index))
         (pct (if (> old-total 0) (/ (float old-idx) old-total) 0.0))
         (new-total (bw/ce--band-total-entries new-band))
         (new-idx (min (max 0 (1- new-total))
                       (round (* pct new-total)))))
    new-idx))

;; ═══════════════════════════════════════════════════════════════════
;; Wall-Hit Flash
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--wall-flash (direction)
  "Flash DIRECTION edge of navigator. DIRECTION is `left' or `right'."
  ;; Clean up any existing flash
  (when bw/ce--wall-timer (cancel-timer bw/ce--wall-timer))
  (when (and bw/ce--wall-overlay (overlay-buffer bw/ce--wall-overlay))
    (delete-overlay bw/ce--wall-overlay))
  (let* ((win (selected-window))
         (buf (current-buffer))
         (ov (make-overlay (point-min) (point-max) buf)))
    (overlay-put ov 'face
                 (if (eq direction 'right)
                     '(:background "#da8548" :extend t)
                   '(:background "#da8548" :extend t)))
    ;; Use a thin line at the edge via after-string/before-string
    (overlay-put ov 'priority 200)
    ;; Flash the fringe area by briefly coloring the overlay
    (if (eq direction 'right)
        (overlay-put ov 'after-string
                     (propertize "  " 'face '(:background "#da8548")))
      (overlay-put ov 'before-string
                   (propertize "  " 'face '(:background "#da8548"))))
    (setq bw/ce--wall-overlay ov)
    (setq bw/ce--wall-timer
          (run-at-time 0.15 nil
                       (lambda ()
                         (when (and ov (overlay-buffer ov))
                           (delete-overlay ov)))))))

;; ═══════════════════════════════════════════════════════════════════
;; Display Pane
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--binary-p (path)
  "Return non-nil if PATH is a binary file."
  (let ((ext (file-name-extension path)))
    (and ext (member (downcase ext) bw/ce-binary-exts))))

(defun bw/ce--show-placeholder (path)
  "Show a placeholder in display pane for binary/large file at PATH."
  (let ((buf (get-buffer-create "*ce-placeholder*"))
        (size (nth 7 (file-attributes path)))
        (name (file-name-nondirectory path)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n\n")
        (insert (propertize (concat "  " (condition-case nil
                                             (nerd-icons-icon-for-file name)
                                           (error ""))
                                    "  " name)
                            'face '(:foreground "#bbc2cf" :height 1.3)))
        (insert "\n\n")
        (insert (propertize (format "  %s"
                                    (if size
                                        (cond
                                         ((> size (* 1024 1024))
                                          (format "%.1f MB" (/ size 1048576.0)))
                                         ((> size 1024)
                                          (format "%.1f KB" (/ size 1024.0)))
                                         (t (format "%d bytes" size)))
                                      "unknown size"))
                            'face '(:foreground "#5B6268")))
        (setq buffer-read-only t)))
    (when (window-live-p bw/ce--display-window)
      (set-window-buffer bw/ce--display-window buf))))

(defun bw/ce--track-temp-buffer (buf)
  "Track BUF in temp buffer ring. Kill oldest if over limit."
  (unless (member buf bw/ce--temp-buffers)
    (push buf bw/ce--temp-buffers)
    (when (> (length bw/ce--temp-buffers) bw/ce-temp-buffer-limit)
      (let ((oldest (car (last bw/ce--temp-buffers))))
        (when (and (buffer-live-p oldest)
                   (not (buffer-modified-p oldest))
                   (not (string= (buffer-file-name oldest) bw/ce--pinned-file)))
          (kill-buffer oldest))
        (setq bw/ce--temp-buffers
              (cl-remove oldest bw/ce--temp-buffers))))))

(defun bw/ce--do-preview (path)
  "Preview file at PATH in the display pane."
  (when (window-live-p bw/ce--display-window)
    (let* ((already-open (find-buffer-visiting path))
           (buf (or already-open
                    (let ((non-essential t))
                      (find-file-noselect path t)))))
      (when buf
        ;; Track temp buffers (only if we opened it)
        (unless already-open
          (bw/ce--track-temp-buffer buf))
        ;; Display
        (set-window-buffer bw/ce--display-window buf)
        (setq bw/ce--display-buffer buf)
        ;; Update pin indicator on new buffer
        (bw/ce--update-pin-indicator)
        ;; Adaptive width via vsnake
        (let* ((metrics (with-current-buffer buf
                          (bw/vsnake--analyze-content)))
               (effective (plist-get metrics :effective-width))
               (frame-w (frame-width))
               (min-preview (max 35 (floor (* frame-w 0.6))))
               (target (+ effective 7))
               (width (max min-preview (min target (- frame-w 20))))
               (current-w (window-body-width bw/ce--display-window)))
          ;; Jitter suppression: only resize if delta > 5
          (when (> (abs (- width current-w)) 5)
            (let ((delta (- width current-w)))
              (with-selected-window bw/ce--display-window
                (enlarge-window-horizontally delta)))))
        (setq bw/ce--last-previewed path)))))

(defun bw/ce--maybe-preview ()
  "Post-command-hook: auto-open file at point if conditions met."
  (when bw/code-explorer-mode
    (let ((path (get-text-property (point) 'bw/ce-path))
          (is-dir (get-text-property (point) 'bw/ce-is-dir)))
      (when (and path
                 (not is-dir)                           ; sticky on dirs
                 (not bw/ce--pinned-file)               ; pinned = locked
                 (not (string= path bw/ce--last-previewed))) ; dedup
        (if (or (bw/ce--binary-p path)
                (and (file-exists-p path)
                     (> (or (nth 7 (file-attributes path)) 0)
                        bw/ce-max-file-size)))
            (bw/ce--show-placeholder path)
          (bw/ce--do-preview path))))))

;; ═══════════════════════════════════════════════════════════════════
;; History
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--history-push ()
  "Push current state onto history stack."
  ;; Truncate forward history if we navigated back then changed
  (when (and bw/ce--history (> bw/ce--history-index -1))
    (setq bw/ce--history (nthcdr bw/ce--history-index bw/ce--history)))
  (push (list :depth bw/ce--depth
              :band bw/ce--band
              :band-parents bw/ce--band-parents
              :cursor-idx (bw/ce--cursor-entry-index))
        bw/ce--history)
  (setq bw/ce--history-index -1))

(defun bw/ce--history-back ()
  "Navigate to previous zoom state."
  (interactive)
  (let ((target-idx (1+ bw/ce--history-index)))
    (when (< target-idx (length bw/ce--history))
      ;; Save current state if at head
      (when (= bw/ce--history-index -1)
        (bw/ce--history-push)
        (setq target-idx 1))
      (let ((snapshot (nth target-idx bw/ce--history)))
        (setq bw/ce--history-index target-idx
              bw/ce--depth (plist-get snapshot :depth)
              bw/ce--band (plist-get snapshot :band)
              bw/ce--band-parents (plist-get snapshot :band-parents))
        (bw/ce--render-band bw/ce--band)
        (bw/ce--goto-entry-index (plist-get snapshot :cursor-idx))
        (recenter (/ (window-body-height) 3))))))

(defun bw/ce--history-forward ()
  "Navigate to next zoom state."
  (interactive)
  (when (> bw/ce--history-index 0)
    (cl-decf bw/ce--history-index)
    (let ((snapshot (nth bw/ce--history-index bw/ce--history)))
      (setq bw/ce--depth (plist-get snapshot :depth)
            bw/ce--band (plist-get snapshot :band)
            bw/ce--band-parents (plist-get snapshot :band-parents))
      (bw/ce--render-band bw/ce--band)
      (bw/ce--goto-entry-index (plist-get snapshot :cursor-idx))
      (recenter (/ (window-body-height) 3)))))

;; ═══════════════════════════════════════════════════════════════════
;; Navigation Commands
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--next-entry ()
  "Move to next entry, skipping divider lines."
  (interactive)
  (forward-line 1)
  ;; Skip divider lines
  (while (and (not (eobp))
              (get-text-property (point) 'bw/ce-divider))
    (forward-line 1))
  (when (eobp) (forward-line -1))
  (beginning-of-line))

(defun bw/ce--prev-entry ()
  "Move to previous entry, skipping divider lines."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp))
              (get-text-property (point) 'bw/ce-divider))
    (forward-line -1))
  (beginning-of-line))

(defun bw/ce--current-group-idx ()
  "Get the group index at point."
  (get-text-property (point) 'bw/ce-group-idx))

(defun bw/ce--next-group ()
  "Jump to first entry of next group."
  (interactive)
  (let ((cur-idx (bw/ce--current-group-idx)))
    (when cur-idx
      (let ((target (1+ cur-idx)))
        (when (< target (length bw/ce--band))
          (let ((group (nth target bw/ce--band)))
            (goto-char (point-min))
            (forward-line (1- (bw/ce-group-start-line group)))
            (beginning-of-line)))))))

(defun bw/ce--prev-group ()
  "Jump to first entry of previous group."
  (interactive)
  (let ((cur-idx (bw/ce--current-group-idx)))
    (when (and cur-idx (> cur-idx 0))
      (let ((group (nth (1- cur-idx) bw/ce--band)))
        (goto-char (point-min))
        (forward-line (1- (bw/ce-group-start-line group)))
        (beginning-of-line)))))

(defun bw/ce--group-end ()
  "Move to last entry of current group (PgDn behavior)."
  (interactive)
  (let ((cur-idx (bw/ce--current-group-idx)))
    (when cur-idx
      (let ((group (nth cur-idx bw/ce--band)))
        (goto-char (point-min))
        (forward-line (1- (bw/ce-group-end-line group)))
        (beginning-of-line)))))

(defun bw/ce--group-start ()
  "Move to first entry of current group (PgUp behavior)."
  (interactive)
  (let ((cur-idx (bw/ce--current-group-idx)))
    (when cur-idx
      (let ((group (nth cur-idx bw/ce--band)))
        (goto-char (point-min))
        (forward-line (1- (bw/ce-group-start-line group)))
        (beginning-of-line)))))

(defun bw/ce--recenter ()
  "Recenter view with cursor at anchor (1/3 from top)."
  (interactive)
  (recenter (/ (window-body-height) 3)))

(defun bw/ce--depth-increase ()
  "Increase depth: expand all directories in current band."
  (interactive)
  (let ((new-band (bw/ce--compute-deeper-band bw/ce--band)))
    (if (null new-band)
        (bw/ce--wall-flash 'right)
      (bw/ce--history-push)
      (let ((new-idx (bw/ce--zone-map bw/ce--band new-band)))
        (setq bw/ce--band new-band)
        (cl-incf bw/ce--depth)
        (bw/ce--render-band bw/ce--band)
        (bw/ce--goto-entry-index new-idx)
        (recenter (/ (window-body-height) 3))))))

(defun bw/ce--depth-decrease ()
  "Decrease depth: zoom out to parent level."
  (interactive)
  (let* ((current-dirs (mapcar #'bw/ce-group-dir bw/ce--band))
         (parents (delete-dups
                   (mapcar (lambda (d)
                             (file-name-directory (directory-file-name d)))
                           current-dirs)))
         (at-root (and (= (length parents) 1)
                       (string= (car parents) "/"))))
    (if at-root
        (bw/ce--wall-flash 'left)
      (bw/ce--history-push)
      (let* ((new-band (bw/ce--compute-shallower-band bw/ce--band))
             (new-idx (bw/ce--zone-map bw/ce--band new-band)))
        (setq bw/ce--band new-band)
        (cl-decf bw/ce--depth)
        (bw/ce--render-band bw/ce--band)
        (bw/ce--goto-entry-index new-idx)
        (recenter (/ (window-body-height) 3))))))

;; ═══════════════════════════════════════════════════════════════════
;; Pin / Unpin
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--update-pin-indicator ()
  "Update display pane header-line to show pin state."
  (when (and (window-live-p bw/ce--display-window)
             (buffer-live-p (window-buffer bw/ce--display-window)))
    (with-current-buffer (window-buffer bw/ce--display-window)
      (setq-local header-line-format
                  (when bw/ce--pinned-file
                    (list " "
                          (propertize " PIN "
                                      'face '(:background "#da8548"
                                              :foreground "#1B2229"
                                              :weight bold))
                          " "
                          (propertize (file-name-nondirectory bw/ce--pinned-file)
                                      'face '(:foreground "#bbc2cf"))))))))

(defun bw/ce--toggle-pin ()
  "Toggle pin state. RET pins current file; RET again unpins."
  (interactive)
  (if bw/ce--pinned-file
      (progn
        (setq bw/ce--pinned-file nil)
        (bw/ce--update-pin-indicator)
        (message "Unpinned")
        ;; Resume auto-open immediately
        (setq bw/ce--last-previewed nil)
        (bw/ce--maybe-preview))
    (when bw/ce--display-buffer
      (let ((file (buffer-file-name bw/ce--display-buffer)))
        (when file
          (setq bw/ce--pinned-file file)
          (bw/ce--update-pin-indicator)
          (message "Pinned: %s" (file-name-nondirectory file))))))
  (force-mode-line-update))

;; ═══════════════════════════════════════════════════════════════════
;; File Operations
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--marked-paths ()
  "Return list of all marked paths."
  (let ((paths nil))
    (maphash (lambda (k _v) (push k paths)) bw/ce--marks)
    (nreverse paths)))

(defun bw/ce--mark-count-in-band ()
  "Count marks visible in current band."
  (let ((count 0))
    (dolist (group bw/ce--band)
      (dolist (entry (bw/ce-group-entries group))
        (when (gethash (nth 1 entry) bw/ce--marks)
          (cl-incf count))))
    count))

(defun bw/ce--mark ()
  "Mark entry at point. Advance cursor."
  (interactive)
  (let ((path (get-text-property (point) 'bw/ce-path))
        (cur-line (line-number-at-pos)))
    (when path
      (puthash path t bw/ce--marks)
      (bw/ce--render-band bw/ce--band)
      ;; Restore position then advance
      (goto-char (point-min))
      (forward-line (1- cur-line))
      (bw/ce--next-entry)
      (force-mode-line-update))))

(defun bw/ce--unmark ()
  "Unmark entry at point."
  (interactive)
  (let ((path (get-text-property (point) 'bw/ce-path))
        (cur-line (line-number-at-pos)))
    (when path
      (remhash path bw/ce--marks)
      (bw/ce--render-band bw/ce--band)
      (goto-char (point-min))
      (forward-line (1- cur-line))
      (force-mode-line-update))))

(defun bw/ce--unmark-all ()
  "Unmark all entries globally."
  (interactive)
  (clrhash bw/ce--marks)
  (bw/ce--render-band bw/ce--band)
  (force-mode-line-update)
  (message "All marks cleared"))

(defun bw/ce--rename ()
  "Rename entry at point (or all marked entries individually)."
  (interactive)
  (let ((paths (if (> (hash-table-count bw/ce--marks) 0)
                   (bw/ce--marked-paths)
                 (list (get-text-property (point) 'bw/ce-path)))))
    (dolist (path paths)
      (when path
        (let ((new-name (read-string
                         (format "Rename %s to: "
                                 (file-name-nondirectory path))
                         (file-name-nondirectory path))))
          (let ((new-path (expand-file-name new-name
                                            (file-name-directory path))))
            (rename-file path new-path)
            (remhash path bw/ce--marks)
            (message "Renamed to %s" new-name)))))
    (bw/ce--refresh-band)))

(defun bw/ce--delete ()
  "Delete entry at point or all marked entries (moves to trash)."
  (interactive)
  (let* ((marked-count (hash-table-count bw/ce--marks))
         (paths (if (> marked-count 0)
                    (bw/ce--marked-paths)
                  (list (get-text-property (point) 'bw/ce-path))))
         (count (length paths))
         (here (bw/ce--mark-count-in-band))
         (elsewhere (- marked-count here)))
    (when (and paths
               (yes-or-no-p
                (if (> count 1)
                    (format "Delete %d entries? (%d at other depths) "
                            count (max 0 elsewhere))
                  (format "Delete %s? " (file-name-nondirectory (car paths))))))
      (dolist (path paths)
        (when (file-exists-p path)
          (if (file-directory-p path)
              (delete-directory path t t) ; recursive, trash
            (delete-file path t)))        ; trash
        (remhash path bw/ce--marks))
      (message "Deleted %d entries" count)
      (bw/ce--refresh-band))))

(defun bw/ce--copy-file ()
  "Copy file at point or all marked entries to destination."
  (interactive)
  (let ((paths (if (> (hash-table-count bw/ce--marks) 0)
                   (bw/ce--marked-paths)
                 (list (get-text-property (point) 'bw/ce-path))))
        (dest (read-directory-name "Copy to: ")))
    (dolist (path paths)
      (when (and path (file-exists-p path))
        (let ((target (expand-file-name (file-name-nondirectory path) dest)))
          (if (file-directory-p path)
              (copy-directory path target nil t t)
            (copy-file path target t)))))
    (message "Copied %d entries to %s" (length paths) dest)
    (bw/ce--refresh-band)))

(defun bw/ce--mkdir ()
  "Create new directory in the focus group's directory."
  (interactive)
  (let* ((group-idx (bw/ce--current-group-idx))
         (group (and group-idx (nth group-idx bw/ce--band)))
         (dir (and group (bw/ce-group-dir group))))
    (when dir
      (let ((name (read-string (format "New directory in %s: "
                                       (abbreviate-file-name dir)))))
        (make-directory (expand-file-name name dir))
        (message "Created %s" name)
        (bw/ce--refresh-band)))))

;; Copy commands (c prefix)
(defun bw/ce--copy-dir-path ()
  "Copy directory path of entry at point to kill ring."
  (interactive)
  (let ((path (get-text-property (point) 'bw/ce-path)))
    (when path
      (let ((dir (file-name-directory path)))
        (kill-new dir)
        (message "Copied: %s" dir)))))

(defun bw/ce--copy-filename ()
  "Copy filename of entry at point to kill ring."
  (interactive)
  (let ((path (get-text-property (point) 'bw/ce-path)))
    (when path
      (let ((name (file-name-nondirectory path)))
        (kill-new name)
        (message "Copied: %s" name)))))

(defun bw/ce--copy-name-no-ext ()
  "Copy filename without extension to kill ring."
  (interactive)
  (let ((path (get-text-property (point) 'bw/ce-path)))
    (when path
      (let ((name (file-name-sans-extension
                   (file-name-nondirectory path))))
        (kill-new name)
        (message "Copied: %s" name)))))

;; ═══════════════════════════════════════════════════════════════════
;; Sort Menu Commands
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--read-sort-method ()
  "Prompt user for sort method. Return symbol."
  (let ((choice (completing-read "Sort by: "
                                 '("Alphabetical (dirs first)"
                                   "Date modified"
                                   "Size"
                                   "Type (extension)")
                                 nil t)))
    (pcase choice
      ("Alphabetical (dirs first)" 'alpha-dirs-first)
      ("Date modified" 'date-modified)
      ("Size" 'size)
      ("Type (extension)" 'type-ext)
      (_ 'alpha-dirs-first))))

(defun bw/ce--sort-global ()
  "Set global default sort method."
  (interactive)
  (setq bw/ce--global-sort (bw/ce--read-sort-method))
  (message "Global sort: %s" bw/ce--global-sort)
  (bw/ce--refresh-band))

(defun bw/ce--sort-directory ()
  "Set sort method for the focus group's directory."
  (interactive)
  (let* ((group-idx (bw/ce--current-group-idx))
         (group (and group-idx (nth group-idx bw/ce--band)))
         (dir (and group (bw/ce-group-dir group))))
    (when dir
      (puthash dir (bw/ce--read-sort-method) bw/ce--sort-overrides)
      (bw/ce--save-sort-cache)
      (message "Directory sort for %s updated" (abbreviate-file-name dir))
      (bw/ce--refresh-band))))

(defun bw/ce--sort-group ()
  "Set sort method for entries within current group."
  (interactive)
  (let* ((group-idx (bw/ce--current-group-idx))
         (group (and group-idx (nth group-idx bw/ce--band)))
         (dir (and group (bw/ce-group-dir group))))
    (when dir
      (puthash dir (bw/ce--read-sort-method) bw/ce--sort-overrides)
      (bw/ce--save-sort-cache)
      (message "Group sort for %s updated" (abbreviate-file-name dir))
      (bw/ce--refresh-band))))

(defun bw/ce--sort-reset-all ()
  "Nuclear reset: clear ALL sort overrides."
  (interactive)
  (clrhash bw/ce--sort-overrides)
  (bw/ce--save-sort-cache)
  (message "All sort overrides cleared")
  (bw/ce--refresh-band))

(defun bw/ce--sort-reset-depth ()
  "Reset sort overrides at current depth and deeper."
  (interactive)
  (let ((current-dirs (mapcar #'bw/ce-group-dir bw/ce--band))
        (to-remove nil))
    (maphash (lambda (dir _method)
               ;; Remove if dir is at current depth or deeper
               ;; (i.e., is a subdirectory of any current band parent)
               (dolist (parent bw/ce--band-parents)
                 (when (string-prefix-p parent dir)
                   (push dir to-remove))))
             bw/ce--sort-overrides)
    (dolist (dir to-remove)
      (remhash dir bw/ce--sort-overrides))
    (bw/ce--save-sort-cache)
    (message "Reset %d sort overrides at depth %d+" (length to-remove) bw/ce--depth)
    (bw/ce--refresh-band)))

(defun bw/ce--sort-reset-tree ()
  "Reset sort overrides for current dir tree (recursive)."
  (interactive)
  (let* ((group-idx (bw/ce--current-group-idx))
         (group (and group-idx (nth group-idx bw/ce--band)))
         (dir (and group (bw/ce-group-dir group)))
         (to-remove nil))
    (when dir
      (maphash (lambda (d _m)
                 (when (string-prefix-p dir d)
                   (push d to-remove)))
               bw/ce--sort-overrides)
      (dolist (d to-remove)
        (remhash d bw/ce--sort-overrides))
      (bw/ce--save-sort-cache)
      (message "Reset %d sort overrides under %s"
               (length to-remove) (abbreviate-file-name dir))
      (bw/ce--refresh-band))))

;; ═══════════════════════════════════════════════════════════════════
;; Toggle Hidden / Fuzzy Search
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--toggle-hidden ()
  "Toggle visibility of dotfiles."
  (interactive)
  (setq bw/ce--show-hidden (not bw/ce--show-hidden))
  (message "Hidden files: %s" (if bw/ce--show-hidden "shown" "hidden"))
  (bw/ce--refresh-band))

(defun bw/ce--fuzzy-search ()
  "Fuzzy search across current directories and subdirs."
  (interactive)
  (let ((dirs (mapcar #'bw/ce-group-dir bw/ce--band))
        (candidates nil))
    ;; Collect files from all groups' dirs + subdirs (1 level deep for speed)
    (dolist (dir dirs)
      (condition-case nil
          (let ((files (directory-files-recursively dir "." nil t)))
            (dolist (f files)
              (push (abbreviate-file-name f) candidates)))
        (error nil)))
    (let ((choice (completing-read "Find: " (nreverse candidates) nil t)))
      (when choice
        (let ((full (expand-file-name choice)))
          ;; Preview the selected file
          (setq bw/ce--last-previewed nil)
          (bw/ce--do-preview full))))))

;; ═══════════════════════════════════════════════════════════════════
;; Refresh
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--refresh-band ()
  "Re-scan and re-render the current band, preserving cursor position."
  (let ((old-idx (bw/ce--cursor-entry-index)))
    ;; Re-scan each group's directory
    (dolist (group bw/ce--band)
      (let* ((dir (bw/ce-group-dir group))
             (entries (bw/ce--scan-directory dir))
             (sort-method (bw/ce--sort-method-for-dir dir))
             (sorted (bw/ce--sort-entries entries sort-method)))
        (setf (bw/ce-group-entries group) sorted)))
    (bw/ce--render-band bw/ce--band)
    ;; Restore cursor as close as possible
    (let ((total (bw/ce--band-total-entries bw/ce--band)))
      (bw/ce--goto-entry-index (min old-idx (max 0 (1- total)))))))

;; ═══════════════════════════════════════════════════════════════════
;; Header Line
;; ═══════════════════════════════════════════════════════════════════

(defun bw/ce--header-line ()
  "Compute header line string."
  (let* ((mark-total (hash-table-count bw/ce--marks))
         (mark-here (bw/ce--mark-count-in-band))
         (depth-str (format "Depth:%d" bw/ce--depth))
         (mark-str (if (> mark-total 0)
                       (format " | %d marked / %d here" mark-total mark-here)
                     ""))
         (pin-str (if bw/ce--pinned-file
                      (format " | PIN: %s"
                              (file-name-nondirectory bw/ce--pinned-file))
                    "")))
    (concat " " depth-str mark-str pin-str)))

;; ═══════════════════════════════════════════════════════════════════
;; Minor Mode + Keymaps
;; ═══════════════════════════════════════════════════════════════════

(defvar bw/ce-copy-map (make-sparse-keymap) "Code-explorer copy prefix.")
(define-key bw/ce-copy-map "d" #'bw/ce--copy-dir-path)
(define-key bw/ce-copy-map "f" #'bw/ce--copy-filename)
(define-key bw/ce-copy-map "n" #'bw/ce--copy-name-no-ext)

(defvar bw/ce-sort-map (make-sparse-keymap) "Code-explorer sort prefix.")
(define-key bw/ce-sort-map "g" #'bw/ce--sort-global)
(define-key bw/ce-sort-map "d" #'bw/ce--sort-directory)
(define-key bw/ce-sort-map "s" #'bw/ce--sort-group)
(define-key bw/ce-sort-map "!" #'bw/ce--sort-reset-all)
(define-key bw/ce-sort-map "~" #'bw/ce--sort-reset-depth)
(define-key bw/ce-sort-map "_" #'bw/ce--sort-reset-tree)

(defvar bw/code-explorer-mode-map (make-sparse-keymap)
  "Keymap for code-explorer minor mode.")

(define-minor-mode bw/code-explorer-mode
  "Sibling-continuous depth-zoom file browser."
  :lighter " CE"
  :keymap bw/code-explorer-mode-map
  (if bw/code-explorer-mode
      (progn
        ;; Clear stale which-key overriding map
        (setq overriding-terminal-local-map nil)
        (add-hook 'post-command-hook #'bw/ce--maybe-preview nil t)
        (setq-local cursor-type 'bar
                    truncate-lines t
                    buffer-read-only t
                    display-line-numbers nil
                    header-line-format '(:eval (bw/ce--header-line)))
        ;; Buffer-local evil bindings — override global normal state
        (when (bound-and-true-p evil-local-mode)
          (evil-local-set-key 'normal "j" #'bw/ce--next-entry)
          (evil-local-set-key 'normal "k" #'bw/ce--prev-entry)
          (evil-local-set-key 'normal "l" #'bw/ce--depth-increase)
          (evil-local-set-key 'normal "h" #'bw/ce--depth-decrease)
          (evil-local-set-key 'normal (kbd "S-<next>")  #'bw/ce--next-group)
          (evil-local-set-key 'normal (kbd "S-<prior>") #'bw/ce--prev-group)
          (evil-local-set-key 'normal (kbd "<next>")    #'bw/ce--group-end)
          (evil-local-set-key 'normal (kbd "<prior>")   #'bw/ce--group-start)
          (evil-local-set-key 'normal (kbd "RET") #'bw/ce--toggle-pin)
          (evil-local-set-key 'normal (kbd "<escape>") #'bw/ce--recenter)
          (evil-local-set-key 'normal "R" #'bw/ce--rename)
          (evil-local-set-key 'normal "D" #'bw/ce--delete)
          (evil-local-set-key 'normal "C" #'bw/ce--copy-file)
          (evil-local-set-key 'normal "+" #'bw/ce--mkdir)
          (evil-local-set-key 'normal "m" #'bw/ce--mark)
          (evil-local-set-key 'normal "u" #'bw/ce--unmark)
          (evil-local-set-key 'normal "U" #'bw/ce--unmark-all)
          (evil-local-set-key 'normal "/" #'bw/ce--fuzzy-search)
          (evil-local-set-key 'normal (kbd "M-<left>")  #'bw/ce--history-back)
          (evil-local-set-key 'normal (kbd "M-<right>") #'bw/ce--history-forward)
          (evil-local-set-key 'normal "." #'bw/ce--toggle-hidden)
          (evil-local-set-key 'normal "c" bw/ce-copy-map)
          (evil-local-set-key 'normal "," bw/ce-sort-map)
          (evil-local-set-key 'normal "q" #'bw/code-explorer-quit)))
    (remove-hook 'post-command-hook #'bw/ce--maybe-preview t)))

;; Prefix maps still on minor-mode-map for which-key discovery
(define-key bw/code-explorer-mode-map "c" bw/ce-copy-map)
(define-key bw/code-explorer-mode-map "," bw/ce-sort-map)

;; ═══════════════════════════════════════════════════════════════════
;; Which-Key Integration
;; ═══════════════════════════════════════════════════════════════════

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements bw/ce-copy-map
    "d" "📋 dir path"
    "f" "📄 filename"
    "n" "📝 name (no ext)")

  (which-key-add-keymap-based-replacements bw/ce-sort-map
    "g" "🌐 global default"
    "d" "📁 directory sort"
    "s" "📋 group sort"
    "!" "💥 reset all"
    "~" "📏 reset depth+"
    "_" "🌳 reset tree"))

(with-eval-after-load 'which-key-posframe
  (when (boundp 'bw/which-key-section-definitions)
    (puthash 'bw/ce-copy-map
             '(("📋 Copy" . ("d" "f" "n")))
             bw/which-key-section-definitions)

    (puthash 'bw/ce-sort-map
             '(("🔀 Sort Mode" . ("g" "d" "s"))
               :break
               ("🔄 Reset" . ("!" "~" "_")))
             bw/which-key-section-definitions)))

;; ═══════════════════════════════════════════════════════════════════
;; Entry Point + Exit
;; ═══════════════════════════════════════════════════════════════════

;;;###autoload
(defun bw/code-explorer ()
  "Open the sibling-continuous depth-zoom file browser."
  (interactive)
  (let* ((start-dir (or (and (project-current)
                             (project-root (project-current)))
                        default-directory))
         (prev-buf (current-buffer))
         ;; Find last editor file (same logic as bw/editor)
         (editor-buf (cl-find-if #'buffer-file-name (buffer-list)))
         (editor-file (when editor-buf (buffer-file-name editor-buf)))
         (nav-buf (get-buffer-create "*code-explorer*")))
    ;; Kill existing explorer if any
    (when (get-buffer "*code-explorer*")
      (with-current-buffer (get-buffer "*code-explorer*")
        (when bw/code-explorer-mode
          (bw/code-explorer-mode -1)))
      (kill-buffer "*code-explorer*")
      (setq nav-buf (get-buffer-create "*code-explorer*")))
    ;; Setup layout — give display pane 60% immediately
    (delete-other-windows)
    (switch-to-buffer nav-buf)
    (let* ((frame-w (frame-width))
           (nav-cols (max 25 (floor (* frame-w 0.35))))
           (display-win (split-window-right nav-cols)))
      ;; Immediately set display window to a blank buffer so it doesn't mirror navigator
      (let ((init-buf (get-buffer-create "*ce-display*")))
        (with-current-buffer init-buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "\n\n  Navigate to a file to preview it.\n"))
          (setq buffer-read-only t))
        (set-window-buffer display-win init-buf))
      ;; Initialize state
      (with-current-buffer nav-buf
        (setq bw/ce--display-window display-win
              bw/ce--start-dir (file-name-as-directory start-dir)
              bw/ce--prev-buffer prev-buf
              bw/ce--depth 0
              bw/ce--history nil
              bw/ce--history-index -1
              bw/ce--marks (make-hash-table :test 'equal)
              bw/ce--sort-overrides (make-hash-table :test 'equal)
              bw/ce--show-hidden t
              bw/ce--temp-buffers nil
              bw/ce--pinned-file nil
              bw/ce--last-previewed nil)
        ;; Load sort cache
        (bw/ce--load-sort-cache)
        ;; Compute initial band
        (let ((band (bw/ce--compute-initial-band start-dir)))
          (setq bw/ce--band band)
          ;; Render
          (bw/ce--render-band band)
          ;; Find start-dir's group and position cursor
          (let ((target-group nil)
                (idx 0))
            (dolist (group band)
              (when (string= (file-name-as-directory (bw/ce-group-dir group))
                             (file-name-as-directory start-dir))
                (setq target-group idx))
              (cl-incf idx))
            (if target-group
                (let ((g (nth target-group band)))
                  (goto-char (point-min))
                  (forward-line (1- (bw/ce-group-start-line g))))
              (goto-char (point-min))))
          ;; Anchor scroll
          (recenter (/ (window-body-height) 3))
          ;; Activate mode
          (bw/code-explorer-mode 1)
          ;; Initial preview: pin last editor file, place cursor on it
          (if editor-file
              (progn
                ;; Preview and pin
                (bw/ce--do-preview editor-file)
                (setq bw/ce--pinned-file editor-file)
                (bw/ce--update-pin-indicator)
                ;; Place cursor on the editor file's entry if visible
                (let ((target-line nil))
                  (save-excursion
                    (goto-char (point-min))
                    (while (and (not target-line) (not (eobp)))
                      (when (string= (get-text-property (point) 'bw/ce-path)
                                     editor-file)
                        (setq target-line (line-number-at-pos)))
                      (forward-line 1)))
                  (when target-line
                    (goto-char (point-min))
                    (forward-line (1- target-line))
                    (recenter (/ (window-body-height) 3)))))
            ;; Fallback: find first file entry
            (save-excursion
              (goto-char (point-min))
              (let ((found nil))
                (while (and (not found) (not (eobp)))
                  (let ((p (get-text-property (point) 'bw/ce-path))
                        (d (get-text-property (point) 'bw/ce-is-dir)))
                    (when (and p (not d))
                      (setq found p)))
                  (forward-line 1))
                (when found
                  (bw/ce--do-preview found))))))))))

(defun bw/code-explorer-quit ()
  "Exit code-explorer. Keep last displayed file as active buffer."
  (interactive)
  (let ((display-buf bw/ce--display-buffer)
        (pinned bw/ce--pinned-file)
        (temp-bufs bw/ce--temp-buffers)
        (prev-buf bw/ce--prev-buffer))
    ;; Deactivate mode
    (bw/code-explorer-mode -1)
    ;; Kill temp buffers (unmodified only)
    (dolist (buf temp-bufs)
      (when (and (buffer-live-p buf)
                 (not (buffer-modified-p buf))
                 (not (eq buf display-buf)))
        (kill-buffer buf)))
    ;; Kill placeholder
    (when (get-buffer "*ce-placeholder*")
      (kill-buffer "*ce-placeholder*"))
    ;; Kill navigator
    (kill-buffer "*code-explorer*")
    ;; Show the display buffer or fallback
    (delete-other-windows)
    (let ((target (cond
                   ((and pinned (find-buffer-visiting pinned))
                    (find-buffer-visiting pinned))
                   ((and display-buf (buffer-live-p display-buf))
                    display-buf)
                   ((and prev-buf (buffer-live-p prev-buf))
                    prev-buf)
                   (t nil))))
      (when target
        (switch-to-buffer target)))))

(provide 'code-explorer)
;;; code-explorer.el ends here
