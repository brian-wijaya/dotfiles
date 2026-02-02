;;; memory-protection.el --- global memory guards -*- lexical-binding: t; -*-
;;
;; Loaded before any profile. Prevents OOM from large file undo.

;; Reasonable undo limits (default undo-outer-limit is 24MB)
(setq undo-limit 80000000         ; 80MB per-buffer undo
      undo-strong-limit 120000000 ; 120MB before GC
      undo-outer-limit 300000000) ; 300MB absolute max

;; Disable undo for binary and database files
(defun bw/maybe-disable-undo ()
  "Disable undo for binary files and databases to prevent memory leaks."
  (when (or (string-match-p "\\.\\(db\\|sqlite3?\\|bin\\|dat\\|img\\|iso\\)$"
                            (or buffer-file-name ""))
            (eq buffer-file-coding-system 'no-conversion)
            (and buffer-file-name
                 (> (buffer-size) 10000000)))  ; >10MB
    (buffer-disable-undo)))

(add-hook 'find-file-hook #'bw/maybe-disable-undo)

;; Suppress undo warnings for known large operations
(add-to-list 'warning-suppress-types '(undo discard-info))

;; Prevent auto-revert spam for database files
(defun bw/maybe-disable-auto-revert ()
  "Disable auto-revert for database files to prevent constant revert messages."
  (when (and buffer-file-name
             (string-match-p "\\.\\(db\\|sqlite3?\\)$" buffer-file-name))
    (auto-revert-mode -1)))

(add-hook 'find-file-hook #'bw/maybe-disable-auto-revert)

;; Completely ignore these specific files in recentf and auto-revert
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude "vault\\.db")
  (add-to-list 'recentf-exclude "chroma\\.sqlite3"))
