;;; claude-edit-stream.el --- Real-time diff overlay for Claude Code edits -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, diff

;;; Commentary:

;; A global minor mode that visualizes Claude Code's Edit/Write tool calls
;; as inline diffs with accept/reject capability.  A C++ hook writes JSON
;; lines to /dev/shm/claude_edit_stream and invokes:
;;
;;   emacsclient --eval '(claude-edit-stream--process-pending)'
;;
;; This package reads the SHM file, parses each JSON line, and renders
;; overlays showing removed (old) and added (new) text inline.  The user
;; can then accept or reject individual changes or all at once.

;;; Code:

(require 'diff)
(require 'seq)

(defgroup claude-edit-stream nil
  "Real-time diff overlay for Claude Code edits."
  :group 'tools
  :prefix "claude-edit-stream-")

;;;; Faces ---------------------------------------------------------------

(defface claude-edit-stream-added
  '((t :background "#1a2e1a" :foreground "#9ece6a" :extend t))
  "Face for added/new text."
  :group 'claude-edit-stream)

(defface claude-edit-stream-removed
  '((t :background "#2e1a1a" :foreground "#f7768e" :strike-through t :extend t))
  "Face for removed/old text."
  :group 'claude-edit-stream)

(defface claude-edit-stream-header
  '((t :background "#24283b" :foreground "#7aa2f7" :weight bold :extend t))
  "Face for change header line."
  :group 'claude-edit-stream)

;;;; Variables -----------------------------------------------------------

(defvar claude-edit-stream--changes nil
  "List of active change plists.
Each element is a plist with keys:
  :overlay  - the added overlay (primary reference)
  :file     - absolute path
  :old      - old text string
  :new      - new text string
  :type     - symbol `edit' or `write'
  :group    - gensym linking the 3 overlays together")

(defvar claude-edit-stream--shm-path "/dev/shm/claude_edit_stream"
  "Path to the SHM file written by the C++ hook.")

(defvar claude-edit-stream--emacs-state-path "/dev/shm/emacs_state"
  "Path to SHM file for Emacs state feedback to sensor awareness.")

;;;; SHM state writer -----------------------------------------------------

(defun claude-edit-stream--write-state ()
  "Write current Emacs state to SHM for sensor awareness."
  (let* ((prompt-active (or (active-minibuffer-window) nil))
         (pending (length claude-edit-stream--changes))
         (current-buf (buffer-name (current-buffer)))
         (state (format "prompt=%s changes=%d buffer=%s"
                        (if prompt-active "yes" "no")
                        pending
                        (or current-buf "none"))))
    (write-region state nil claude-edit-stream--emacs-state-path nil 'silent)))

(defun claude-edit-stream--prompt-advice (orig-fn &rest args)
  "Advice to signal that Emacs is waiting for user input."
  (let ((state (format "prompt=yes type=y-or-n query=%s changes=%d buffer=%s"
                        (car args)
                        (length claude-edit-stream--changes)
                        (buffer-name (current-buffer)))))
    (write-region state nil claude-edit-stream--emacs-state-path nil 'silent))
  (unwind-protect
      (let ((last-nonmenu-event t)
            (use-dialog-box nil))
        (apply orig-fn args))
    ;; Clear prompt state after response
    (let ((state (format "prompt=no changes=%d buffer=%s"
                          (length claude-edit-stream--changes)
                          (buffer-name (current-buffer)))))
      (write-region state nil claude-edit-stream--emacs-state-path nil 'silent))))

(advice-add 'y-or-n-p :around #'claude-edit-stream--prompt-advice)
(advice-add 'yes-or-no-p :around #'claude-edit-stream--prompt-advice)
(advice-add 'read-from-minibuffer :around #'claude-edit-stream--prompt-advice)

;;;; Core dispatch -------------------------------------------------------

(defun claude-edit-stream--process-pending ()
  "Read and process all pending edit events from the SHM file.
Called by the C++ hook via emacsclient.  Defers actual work to the
next event loop iteration via `run-at-time' so emacsclient returns
immediately and never blocks the Emacs server."
  (condition-case err
      (when (file-exists-p claude-edit-stream--shm-path)
        (let ((contents (with-temp-buffer
                          (insert-file-contents claude-edit-stream--shm-path)
                          (buffer-string))))
          ;; Truncate the file immediately so events are not replayed.
          (write-region "" nil claude-edit-stream--shm-path nil 'silent)
          ;; Defer processing to the next event loop iteration.
          ;; This lets emacsclient return instantly.
          (run-at-time 0 nil #'claude-edit-stream--process-lines contents)))
    (error
     (message "claude-edit-stream: process-pending error — %s"
              (error-message-string err)))))

(defun claude-edit-stream--process-lines (contents)
  "Process CONTENTS (newline-delimited JSON lines) from the SHM file.
Runs asynchronously via `run-at-time', never in the emacsclient context."
  (dolist (line (split-string contents "\n" t "[ \t]+"))
    (condition-case line-err
        (let ((json (json-parse-string line :object-type 'plist)))
          (pcase (plist-get json :tool)
            ("Edit"  (claude-edit-stream--handle-edit json))
            ("Write" (claude-edit-stream--handle-write json))
            (_       (message "claude-edit-stream: unknown tool %s"
                              (plist-get json :tool)))))
      (error
       (message "claude-edit-stream: JSON parse error on line: %s — %s"
                (truncate-string-to-width line 80)
                (error-message-string line-err))))))

;;;; Edit handler --------------------------------------------------------

(defun claude-edit-stream--handle-edit (json)
  "Handle an Edit tool event described by JSON plist."
  (let* ((file     (plist-get json :file))
         (old-text (plist-get json :old))
         (new-text (plist-get json :new))
         (buf      (find-file-noselect file)))
    (with-current-buffer buf
      (let ((revert-without-query (list ".")))
        (set-buffer-modified-p nil)
        (revert-buffer t t t))
      (goto-char (point-min))
      (cond
       ;; Pure deletion — new-text is empty.
       ((or (null new-text) (string-empty-p new-text))
        (if (search-forward old-text nil t)
            ;; Point is now after old-text; the deletion site is at match start.
            (let ((del-pos (match-beginning 0)))
              (claude-edit-stream--show-change
               buf del-pos del-pos old-text "" file 'edit))
          (message "claude-edit-stream: could not locate old text in %s" file)))
       ;; Pure insertion — old-text is empty.
       ((or (null old-text) (string-empty-p old-text))
        (if (search-forward new-text nil t)
            (claude-edit-stream--show-change
             buf (match-beginning 0) (match-end 0) "" new-text file 'edit)
          (message "claude-edit-stream: could not locate new text in %s" file)))
       ;; Normal replacement — search for new-text (disk already has the edit applied).
       (t
        (if (search-forward new-text nil t)
            (claude-edit-stream--show-change
             buf (match-beginning 0) (match-end 0) old-text new-text file 'edit)
          (message "claude-edit-stream: could not locate new text in %s" file)))))))

;;;; Write handler -------------------------------------------------------

(defun claude-edit-stream--handle-write (json)
  "Handle a Write tool event described by JSON plist."
  (let* ((file        (plist-get json :file))
         (existing    (get-file-buffer file))
         (old-content (when existing
                        (with-current-buffer existing
                          (buffer-string))))
         (buf         (find-file-noselect file)))
    (with-current-buffer buf
      (let ((revert-without-query (list ".")))
        (set-buffer-modified-p nil)
        (revert-buffer t t t))
      (let ((new-content (buffer-string)))
        (cond
         ;; No prior buffer — brand-new file.  Show everything as added.
         ((null old-content)
          (claude-edit-stream--show-change
           buf (point-min) (point-max) "" new-content file 'write))
         ;; Content unchanged — nothing to show.
         ((string= old-content new-content)
          (message "claude-edit-stream: %s unchanged after write" file))
         ;; Diff old vs new.
         (t
          (claude-edit-stream--diff-write buf old-content new-content file)))))))

(defun claude-edit-stream--diff-write (buf old-content new-content file)
  "Diff OLD-CONTENT vs NEW-CONTENT and render overlays in BUF for FILE."
  (let ((hunks (claude-edit-stream--compute-hunks old-content new-content)))
    (if (null hunks)
        (message "claude-edit-stream: no diff hunks for %s" file)
      ;; Process hunks in reverse order so earlier positions remain valid.
      (dolist (hunk (reverse hunks))
        (let ((new-start (nth 0 hunk))
              (new-end   (nth 1 hunk))
              (old-text  (nth 2 hunk)))
          (claude-edit-stream--show-change
           buf new-start new-end old-text
           (with-current-buffer buf
             (buffer-substring-no-properties new-start new-end))
           file 'write)))
      ;; Jump to first change — guarded for headless/daemon context.
      (when-let* ((first-hunk (car hunks)))
        (when (and (frame-live-p (selected-frame))
                   (not (eq (framep (selected-frame)) t)))
          (pop-to-buffer-same-window buf)
          (delete-other-windows)
          (goto-char (nth 0 first-hunk))
          (recenter))))))

(defun claude-edit-stream--compute-hunks (old-content new-content)
  "Return list of (NEW-BEG NEW-END OLD-TEXT) by diffing OLD-CONTENT vs NEW-CONTENT.
Positions are 1-indexed buffer positions in the new content."
  (let (hunks)
    (with-temp-buffer
      (let ((old-buf (current-buffer)))
        (insert old-content)
        (with-temp-buffer
          (let ((new-buf (current-buffer)))
            (insert new-content)
            (let ((diff-output
                   (with-temp-buffer
                     (let ((diff-buf (current-buffer)))
                       (diff-no-select old-buf new-buf nil t diff-buf)
                       (buffer-string)))))
              ;; Parse unified diff output for @@ hunks.
              (with-temp-buffer
                (insert diff-output)
                (goto-char (point-min))
                (while (re-search-forward
                        "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
                        nil t)
                  (let* ((_old-line  (string-to-number (match-string 1)))
                         (_old-count (if (match-string 2)
                                         (string-to-number (match-string 2))
                                       1))
                         (new-line  (string-to-number (match-string 3)))
                         (new-count (if (match-string 4)
                                        (string-to-number (match-string 4))
                                      1))
                         ;; Collect removed lines from hunk body.
                         (removed-lines nil))
                    (forward-line 1)
                    (while (and (not (eobp))
                                (not (looking-at "^@@\\|^diff ")))
                      (cond
                       ((looking-at "^-\\(.*\\)$")
                        (push (match-string 1) removed-lines))
                       ((looking-at "^\\+") nil) ; added line — skip
                       ) ; context line — ignore
                      (forward-line 1))
                    (setq removed-lines (nreverse removed-lines))
                    (let* ((old-text (if removed-lines
                                         (mapconcat #'identity removed-lines "\n")
                                       ""))
                           ;; Convert new-line / new-count to buffer positions.
                           (new-beg (with-current-buffer new-buf
                                      (goto-char (point-min))
                                      (forward-line (1- new-line))
                                      (point)))
                           (new-end (with-current-buffer new-buf
                                      (goto-char (point-min))
                                      (forward-line (1- (+ new-line new-count)))
                                      (if (= new-count 0)
                                          new-beg
                                        (point)))))
                      (push (list new-beg new-end old-text) hunks))))))))))
    (nreverse hunks)))

;;;; Overlay rendering ---------------------------------------------------

(defun claude-edit-stream--show-change (buf beg end old-text new-text file type)
  "Create diff overlays in BUF from BEG to END.
OLD-TEXT is what was there before, NEW-TEXT is what replaced it.
FILE is the absolute path.  TYPE is `edit' or `write'."
  (let ((group (gensym "ces-")))
    (with-current-buffer buf
      ;; 1. Header overlay — spans beg..end so it is not evaporated.
      ;;    before-string renders the header line above the change.
      (let ((header-ov (make-overlay beg (max (1+ beg) end) buf nil t))
            (header-str (propertize
                         (format " Claude %s  [a]ccept  [r]eject  %s\n"
                                 (if (eq type 'edit) "Edit" "Write")
                                 (file-name-nondirectory file))
                         'face 'claude-edit-stream-header)))
        (overlay-put header-ov 'before-string header-str)
        (overlay-put header-ov 'claude-edit-stream t)
        (overlay-put header-ov 'claude-edit-stream-role 'header)
        (overlay-put header-ov 'claude-edit-stream-group group))

      ;; 2. Removed overlay — old text shown as before-string (if non-empty).
      ;;    Spans beg..end so it is not evaporated.
      (when (and old-text (not (string-empty-p old-text)))
        (let* ((prefixed (mapconcat
                          (lambda (l) (concat "- " l))
                          (split-string old-text "\n")
                          "\n"))
               (removed-str (propertize (concat prefixed "\n")
                                        'face 'claude-edit-stream-removed))
               (removed-ov (make-overlay beg (max (1+ beg) end) buf nil t)))
          (overlay-put removed-ov 'before-string removed-str)
          (overlay-put removed-ov 'claude-edit-stream t)
          (overlay-put removed-ov 'claude-edit-stream-role 'removed)
          (overlay-put removed-ov 'claude-edit-stream-group group)))

      ;; 3. Added overlay — spans beg..end on the actual buffer text.
      (let ((added-ov (if (= beg end)
                          ;; Zero-width (pure deletion) — use a marker overlay.
                          (let ((ov (make-overlay beg beg buf nil t)))
                            (overlay-put ov 'after-string
                                         (propertize " [deleted]"
                                                     'face 'claude-edit-stream-added))
                            ov)
                        (let ((ov (make-overlay beg end buf nil t)))
                          (overlay-put ov 'face 'claude-edit-stream-added)
                          ov))))
        (overlay-put added-ov 'claude-edit-stream t)
        (overlay-put added-ov 'claude-edit-stream-role 'added)
        (overlay-put added-ov 'claude-edit-stream-group group)
        (overlay-put added-ov 'claude-edit-stream-old old-text)
        (overlay-put added-ov 'claude-edit-stream-new new-text)
        (overlay-put added-ov 'claude-edit-stream-file file)
        (overlay-put added-ov 'evaporate t)

        ;; Record the change.
        (push (list :overlay added-ov
                    :file file
                    :old old-text
                    :new new-text
                    :type type
                    :group group)
              claude-edit-stream--changes)))

    ;; Display the change — only if a live frame exists (avoids blocking in
    ;; daemon/headless context where pop-to-buffer-same-window can error).
    (when (and (frame-live-p (selected-frame))
               (not (eq (framep (selected-frame)) t)))  ; not a terminal-only frame check
      (pop-to-buffer-same-window buf)
      (delete-other-windows)
      (goto-char beg)
      (recenter))

    ;; Update SHM state for sensor awareness
    (claude-edit-stream--write-state)))

;;;; Overlay group helpers -----------------------------------------------

(defun claude-edit-stream--overlays-at-point ()
  "Return all `claude-edit-stream' overlays at or near point."
  (seq-filter (lambda (ov) (overlay-get ov 'claude-edit-stream))
              (overlays-at (point))))

(defun claude-edit-stream--group-overlays (group buf)
  "Return all overlays in BUF belonging to GROUP."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (seq-filter (lambda (ov)
                    (eq (overlay-get ov 'claude-edit-stream-group) group))
                  (overlays-in (point-min) (point-max))))))

(defun claude-edit-stream--find-added-at-point ()
  "Find the `added' overlay at point, or the nearest one.
Returns the added overlay or nil."
  (let ((ovs (claude-edit-stream--overlays-at-point)))
    ;; Direct hit on added overlay.
    (or (seq-find (lambda (ov)
                    (eq (overlay-get ov 'claude-edit-stream-role) 'added))
                  ovs)
        ;; We might be on the header or removed — find the group and get added.
        (when-let* ((any-ov (car ovs))
                    (group (overlay-get any-ov 'claude-edit-stream-group)))
          (seq-find (lambda (ov)
                      (eq (overlay-get ov 'claude-edit-stream-role) 'added))
                    (claude-edit-stream--group-overlays group (current-buffer)))))))

(defun claude-edit-stream--remove-group (group buf)
  "Delete all overlays in GROUP from BUF and remove from changes list."
  (dolist (ov (claude-edit-stream--group-overlays group buf))
    (delete-overlay ov))
  (setq claude-edit-stream--changes
        (seq-remove (lambda (c) (eq (plist-get c :group) group))
                    claude-edit-stream--changes)))

;;;; Accept / Reject -----------------------------------------------------

(defun claude-edit-stream--accept ()
  "Accept the change at point — keep the new text, remove overlays."
  (interactive)
  (if-let* ((added-ov (claude-edit-stream--find-added-at-point)))
      (let ((group (overlay-get added-ov 'claude-edit-stream-group)))
        (claude-edit-stream--remove-group group (current-buffer))
        (claude-edit-stream--write-state)
        (message "Change accepted"))
    (message "No claude-edit-stream change at point")))

(defun claude-edit-stream--reject ()
  "Reject the change at point — restore old text, remove overlays."
  (interactive)
  (if-let* ((added-ov (claude-edit-stream--find-added-at-point)))
      (let* ((group    (overlay-get added-ov 'claude-edit-stream-group))
             (old-text (overlay-get added-ov 'claude-edit-stream-old))
             (beg      (overlay-start added-ov))
             (end      (overlay-end added-ov)))
        ;; Remove overlays first so they don't interfere with buffer edits.
        (claude-edit-stream--remove-group group (current-buffer))
        ;; Replace new text with old text.
        (cond
         ;; Pure insertion was made — delete the inserted text.
         ((or (null old-text) (string-empty-p old-text))
          (delete-region beg end))
         ;; Pure deletion — insert old text back at the deletion point.
         ((= beg end)
          (goto-char beg)
          (insert old-text))
         ;; Normal replacement.
         (t
          (delete-region beg end)
          (goto-char beg)
          (insert old-text)))
        (let ((save-silently t))
          (save-buffer 0))
        (claude-edit-stream--write-state)
        (message "Change rejected"))
    (message "No claude-edit-stream change at point")))

(defun claude-edit-stream--accept-all ()
  "Accept all pending changes — keep new text, remove all overlays."
  (interactive)
  (let ((count (length claude-edit-stream--changes)))
    (dolist (change claude-edit-stream--changes)
      (let* ((ov  (plist-get change :overlay))
             (buf (overlay-buffer ov)))
        (when (and ov (buffer-live-p buf))
          (let ((group (plist-get change :group)))
            (dolist (gov (claude-edit-stream--group-overlays group buf))
              (delete-overlay gov))))))
    (setq claude-edit-stream--changes nil)
    (claude-edit-stream--write-state)
    (message "Accepted all %d changes" count)))

(defun claude-edit-stream--reject-all ()
  "Reject all pending changes in reverse order (newest first)."
  (interactive)
  (let ((count (length claude-edit-stream--changes)))
    ;; Process in reverse (newest first) to preserve buffer positions.
    (dolist (change (reverse claude-edit-stream--changes))
      (let* ((ov       (plist-get change :overlay))
             (buf      (overlay-buffer ov))
             (old-text (plist-get change :old))
             (group    (plist-get change :group)))
        (when (and ov (buffer-live-p buf))
          (with-current-buffer buf
            (let ((beg (overlay-start ov))
                  (end (overlay-end ov)))
              ;; Remove overlays.
              (dolist (gov (claude-edit-stream--group-overlays group buf))
                (delete-overlay gov))
              ;; Restore old text.
              (cond
               ((or (null old-text) (string-empty-p old-text))
                (delete-region beg end))
               ((= beg end)
                (goto-char beg)
                (insert old-text))
               (t
                (delete-region beg end)
                (goto-char beg)
                (insert old-text)))
              (let ((save-silently t))
                (save-buffer 0)))))))
    (setq claude-edit-stream--changes nil)
    (claude-edit-stream--write-state)
    (message "Rejected all %d changes" count)))

;;;; Navigation ----------------------------------------------------------

(defun claude-edit-stream--next-change ()
  "Jump to the next pending change."
  (interactive)
  (if (null claude-edit-stream--changes)
      (message "No pending changes")
    (let* ((current-pos (point))
           (current-buf (current-buffer))
           (found nil))
      ;; Find the first change after point in current buffer,
      ;; or the first change in any buffer.
      (dolist (change claude-edit-stream--changes)
        (let* ((ov  (plist-get change :overlay))
               (buf (overlay-buffer ov))
               (beg (and ov (buffer-live-p buf) (overlay-start ov))))
          (when (and beg (not found))
            (if (and (eq buf current-buf) (> beg current-pos))
                (setq found change)
              (unless (eq buf current-buf)
                (setq found change))))))
      ;; Fallback: wrap to first change.
      (unless found
        (setq found (car claude-edit-stream--changes)))
      (when found
        (let* ((ov  (plist-get found :overlay))
               (buf (overlay-buffer ov)))
          (when (buffer-live-p buf)
            (pop-to-buffer-same-window buf)
            (goto-char (overlay-start ov))
            (recenter)))))))

(defun claude-edit-stream--prev-change ()
  "Jump to the previous pending change."
  (interactive)
  (if (null claude-edit-stream--changes)
      (message "No pending changes")
    (let* ((current-pos (point))
           (current-buf (current-buffer))
           (found nil))
      ;; Find the last change before point in current buffer.
      (dolist (change claude-edit-stream--changes)
        (let* ((ov  (plist-get change :overlay))
               (buf (overlay-buffer ov))
               (beg (and ov (buffer-live-p buf) (overlay-start ov))))
          (when (and beg (eq buf current-buf) (< beg current-pos))
            (setq found change))))
      ;; Fallback: wrap to last change.
      (unless found
        (setq found (car (last claude-edit-stream--changes))))
      (when found
        (let* ((ov  (plist-get found :overlay))
               (buf (overlay-buffer ov)))
          (when (buffer-live-p buf)
            (pop-to-buffer-same-window buf)
            (goto-char (overlay-start ov))
            (recenter)))))))

;;;; Keymap --------------------------------------------------------------

(defvar claude-edit-stream-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e a") #'claude-edit-stream--accept)
    (define-key map (kbd "C-c e r") #'claude-edit-stream--reject)
    (define-key map (kbd "C-c e A") #'claude-edit-stream--accept-all)
    (define-key map (kbd "C-c e R") #'claude-edit-stream--reject-all)
    (define-key map (kbd "C-c e n") #'claude-edit-stream--next-change)
    (define-key map (kbd "C-c e p") #'claude-edit-stream--prev-change)
    map)
  "Keymap for `claude-edit-stream-mode'.")

;;;; Evil integration ----------------------------------------------------

(declare-function evil-define-key "evil-core" (state keymap &rest bindings))

(defun claude-edit-stream--setup-evil ()
  "Set up evil keybindings for claude-edit-stream if evil is loaded."
  (when (bound-and-true-p evil-mode)
    (evil-define-key 'normal claude-edit-stream-mode-map
      (kbd "]e") #'claude-edit-stream--next-change
      (kbd "[e") #'claude-edit-stream--prev-change)))

;;;; which-key integration ------------------------------------------------

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c e" "claude-edit"
    "C-c e a" "accept"
    "C-c e r" "reject"
    "C-c e A" "accept-all"
    "C-c e R" "reject-all"
    "C-c e n" "next-change"
    "C-c e p" "prev-change"))

;;;; Mode definition -----------------------------------------------------

;;;###autoload
(define-minor-mode claude-edit-stream-mode
  "Global minor mode for real-time Claude Code edit visualization.
When enabled, the C++ hook can invoke `claude-edit-stream--process-pending'
to display inline diff overlays for Edit and Write tool calls."
  :global t
  :group 'claude-edit-stream
  :lighter " CES"
  :keymap claude-edit-stream-mode-map
  (if claude-edit-stream-mode
      (progn
        (claude-edit-stream--setup-evil)
        (message "Claude Edit Stream enabled"))
    (claude-edit-stream--accept-all)
    (message "Claude Edit Stream disabled")))

(provide 'claude-edit-stream)
;;; claude-edit-stream.el ends here
