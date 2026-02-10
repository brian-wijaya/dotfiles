;;; breadcrumb.el --- headerline with green min-prefix path + imenu crumbs -*- lexical-binding: t; -*-

;; Keep breadcrumb for imenu crumb tracking
(use-package breadcrumb
  :demand t
  :config
  (breadcrumb-mode 1))

;; ═══════════════════════════════════════════════════════════════════
;; Min-prefix algorithm — shared with completion.el for minibuffer
;; ═══════════════════════════════════════════════════════════════════

(defun bw/min-prefix-length (name siblings)
  "Return min chars of NAME to be unique among SIBLINGS (case-sensitive).
SIBLINGS should include NAME itself; it is filtered out internally."
  (let ((others (cl-remove name siblings :test #'string=))
        (len 1))
    (while (and (<= len (length name))
                (cl-some (lambda (s)
                           (string-prefix-p (substring name 0 len) s nil))
                         others))
      (cl-incf len))
    len))

;; ═══════════════════════════════════════════════════════════════════
;; Header-line path computation
;; ═══════════════════════════════════════════════════════════════════

(defvar-local bw/header-line--cached-path nil
  "Cached propertized path string for the header line.")
(defvar-local bw/header-line--cached-file nil
  "The buffer-file-name this cache was computed for.")

(defun bw/header-line--compute-path (filepath)
  "Compute propertized path for FILEPATH with green min-prefix chars.
Each directory component gets its shortest unique prefix colored with
`breadcrumb-project-leaf-face'; the rest uses `breadcrumb-project-crumbs-face'.
The filename is fully colored with `breadcrumb-project-leaf-face'."
  (let* ((abbreviated (abbreviate-file-name filepath))
         (parts (split-string abbreviated "/" t))
         (filename (car (last parts)))
         (dirs (butlast parts))
         (result-parts '())
         (rebuild ""))
    ;; Handle leading ~/
    (when (string-prefix-p "~/" abbreviated)
      (push (propertize "~" 'face 'breadcrumb-project-leaf-face) result-parts)
      (setq rebuild "~/"))
    ;; Handle leading / (non-home absolute)
    (when (and (string-prefix-p "/" abbreviated)
               (not (string-prefix-p "~/" abbreviated)))
      (setq rebuild "/"))
    ;; Remove ~ from dirs if present (already handled above)
    (when (and dirs (string= (car dirs) "~"))
      (setq dirs (cdr dirs)))
    ;; Process each directory component
    (dolist (dir dirs)
      (let* ((parent (expand-file-name rebuild))
             (entries (when (file-directory-p parent)
                        (cl-remove-if
                         (lambda (f) (member f '("." "..")))
                         (directory-files parent nil nil t))))
             (min-len (if entries
                         (bw/min-prefix-length dir entries)
                       1))
             (prefix (substring dir 0 (min min-len (length dir))))
             (rest (substring dir (min min-len (length dir)))))
        (push (concat
               (propertize prefix 'face 'breadcrumb-project-leaf-face)
               (propertize rest 'face 'breadcrumb-project-crumbs-face))
              result-parts)
        (setq rebuild (concat rebuild dir "/"))))
    ;; Add filename (fully green)
    (push (propertize filename 'face 'breadcrumb-project-leaf-face) result-parts)
    ;; Join with / separators
    (mapconcat #'identity (nreverse result-parts)
               (propertize "/" 'face 'breadcrumb-project-crumbs-face))))

(defun bw/header-line-path ()
  "Return propertized file path with green min-prefix highlighting.
Caches per buffer; recomputes only when `buffer-file-name' changes."
  (if (and bw/header-line--cached-path
           (equal bw/header-line--cached-file buffer-file-name))
      bw/header-line--cached-path
    (setq bw/header-line--cached-file buffer-file-name
          bw/header-line--cached-path
          (if buffer-file-name
              (bw/header-line--compute-path buffer-file-name)
            (propertize (buffer-name) 'face 'breadcrumb-project-leaf-face)))))

(defun bw/header-line ()
  "Header line: full path with green prefixes + imenu breadcrumbs."
  (let ((path (bw/header-line-path))
        (imenu (ignore-errors (breadcrumb-imenu-crumbs))))
    (if (and imenu (not (string-empty-p imenu)))
        (concat " " path
                (propertize " : " 'face 'breadcrumb-face)
                imenu)
      (concat " " path))))

;; ═══════════════════════════════════════════════════════════════════
;; Hook into file-visiting buffers
;; ═══════════════════════════════════════════════════════════════════

(defun bw/setup-header-line ()
  "Set custom header line for file-visiting buffers.
Removes breadcrumb's own header-line entry so only ours shows."
  (when buffer-file-name
    ;; Remove breadcrumb's header-line entry if present
    (when (listp header-line-format)
      (setq header-line-format
            (delete '(:eval (breadcrumb--header-line)) header-line-format)))
    (setq header-line-format '(:eval (bw/header-line)))))

(add-hook 'find-file-hook #'bw/setup-header-line)
(add-hook 'after-change-major-mode-hook #'bw/setup-header-line)
