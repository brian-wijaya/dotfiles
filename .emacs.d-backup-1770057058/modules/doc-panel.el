;;; doc-panel.el --- persistent documentation panel -*- lexical-binding: t; -*-

;; --- Configuration ---
(defvar bw/doc-panel-buffer-name "*doc-panel*")
(defvar bw/doc-panel-width 0.35
  "Width of the doc panel as a fraction of the frame.")
(defvar bw/doc-panel-idle-delay 0.3
  "Seconds of idle time before updating the doc panel.")
(defvar bw/doc-panel-pin nil
  "When non-nil, doc panel does not auto-update. Holds the pinned symbol.")
(defvar bw/doc-panel--timer nil)
(defvar bw/doc-panel--last-symbol nil)

;; --- Buffer creation ---
(defun bw/doc-panel--get-buffer ()
  "Get or create the doc panel buffer."
  (let ((buf (get-buffer-create bw/doc-panel-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'bw/doc-panel-mode)
        (bw/doc-panel-mode)))
    buf))

;; --- Major mode ---
(define-derived-mode bw/doc-panel-mode special-mode "DocPanel"
  "Major mode for the documentation panel."
  (setq-local cursor-type nil
              truncate-lines nil
              word-wrap t
              buffer-read-only t))

;; --- Content rendering ---
(defun bw/doc-panel--render (symbol-name sections)
  "Render documentation SECTIONS for SYMBOL-NAME into the doc panel buffer.
SECTIONS is an alist of (HEADER . CONTENT) pairs."
  (let ((buf (bw/doc-panel--get-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Title
        (insert (propertize symbol-name 'face '(:height 1.3 :weight bold))
                "\n"
                (make-string 40 ?─)
                "\n\n")
        ;; Sections
        (dolist (section sections)
          (let ((header (car section))
                (content (cdr section)))
            (when content
              (insert (propertize header 'face '(:weight bold :foreground "#51afef"))
                      "\n")
              (insert content "\n\n"))))
        (goto-char (point-min))))))

;; --- Data gathering ---
(defun bw/doc-panel--gather-info ()
  "Gather documentation info for the symbol at point.
Returns (SYMBOL-NAME . SECTIONS-ALIST) or nil."
  (let* ((symbol-name (or (thing-at-point 'symbol t) ""))
         (sections nil))
    (unless (string-empty-p symbol-name)

    ;; 1. Eldoc documentation (hover info from eglot/eldoc)
    (let ((eldoc-buf (get-buffer " *eldoc*")))
      (when (and eldoc-buf (buffer-live-p eldoc-buf))
        (let ((doc (with-current-buffer eldoc-buf (buffer-string))))
          (unless (string-empty-p doc)
            (push (cons "📖 Documentation" doc) sections)))))

    ;; 2. Type / signature (from eglot hover or elisp)
    (when (fboundp 'eglot-managed-p)
      (when (eglot-managed-p)
        ;; eglot hover is already in eldoc, but we can add type info
        (condition-case nil
            (let* ((server (eglot-current-server))
                   (resp (when server
                           (jsonrpc-request
                            server :textDocument/hover
                            (eglot--TextDocumentPositionParams)))))
              (when resp
                (let* ((contents (plist-get resp :contents))
                       (value (if (stringp contents) contents
                                (plist-get contents :value))))
                  (when value
                    (unless (assoc "📖 Documentation" sections)
                      (push (cons "📖 Hover" value) sections))))))
          (error nil))))

    ;; 3. Source location
    (condition-case nil
        (let ((loc (find-function-noselect (intern-soft symbol-name))))
          (when (and loc (car loc) (cdr loc))
            (let* ((buf (car loc))
                   (pos (cdr loc))
                   (file (buffer-file-name buf))
                   (line (with-current-buffer buf
                           (save-excursion
                             (goto-char pos)
                             (line-number-at-pos)))))
              (when file
                (push (cons "📍 Source"
                            (format "%s:%d" (abbreviate-file-name file) line))
                      sections)))))
      (error nil))

    ;; 4. Describe-function summary (for elisp)
    (condition-case nil
        (let ((sym (intern-soft symbol-name)))
          (when (and sym (fboundp sym))
            (let ((doc (documentation sym t)))
              (when doc
                (let ((first-line (car (split-string doc "\n"))))
                  (unless (assoc "📖 Documentation" sections)
                    (push (cons "📖 Description" first-line) sections)))))))
      (error nil))

    ;; 5. Xref definitions count
    (condition-case nil
        (let ((defs (xref-backend-definitions (xref-find-backend) symbol-name)))
          (when defs
            (push (cons "🔗 Definitions"
                        (format "%d definition(s) found" (length defs)))
                  sections)))
      (error nil))

    (when sections
      (cons symbol-name (nreverse sections))))))

;; --- Update logic ---
(defun bw/doc-panel--update ()
  "Update the doc panel content if visible and not pinned."
  (when (and (get-buffer-window bw/doc-panel-buffer-name)
             (not bw/doc-panel-pin))
    (let ((info (bw/doc-panel--gather-info)))
      (when (and info (not (equal (car info) bw/doc-panel--last-symbol)))
        (setq bw/doc-panel--last-symbol (car info))
        (bw/doc-panel--render (car info) (cdr info))))))

;; --- Toggle ---
(defun bw/doc-panel-toggle ()
  "Toggle the documentation panel."
  (interactive)
  (let ((win (get-buffer-window bw/doc-panel-buffer-name)))
    (if win
        (progn
          (when bw/doc-panel--timer
            (cancel-timer bw/doc-panel--timer)
            (setq bw/doc-panel--timer nil))
          (delete-window win)
          (message "Doc panel closed"))
      ;; Open panel
      (let ((buf (bw/doc-panel--get-buffer)))
        (display-buffer-in-side-window buf
          `((side . right)
            (window-width . ,bw/doc-panel-width)
            (slot . 0)
            (dedicated . t)))
        ;; Start idle timer for auto-update
        (setq bw/doc-panel--timer
              (run-with-idle-timer bw/doc-panel-idle-delay t
                                  #'bw/doc-panel--update))
        ;; Initial update
        (bw/doc-panel--update)
        (message "Doc panel opened")))))

;; --- Pin/unpin ---
(defun bw/doc-panel-pin-toggle ()
  "Toggle pinning the current doc panel content."
  (interactive)
  (if bw/doc-panel-pin
      (progn
        (setq bw/doc-panel-pin nil)
        (message "Doc panel unpinned — auto-updating"))
    (setq bw/doc-panel-pin bw/doc-panel--last-symbol)
    (message "Doc panel pinned on: %s" bw/doc-panel-pin)))
