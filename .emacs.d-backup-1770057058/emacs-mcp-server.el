;;; emacs-mcp-server.el --- HTTP server for Claude Code MCP integration -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (simple-httpd "1.5.1"))
;; Keywords: tools, mcp, ai
;; URL: https://github.com/anthropics/claude-code

;;; Commentary:

;; This package provides an HTTP server that exposes Emacs functionality
;; to Claude Code via the Model Context Protocol (MCP).  It allows Claude
;; to inspect buffers, execute elisp, capture screenshots, and interact
;; with Emacs programmatically.
;;
;; Usage:
;;   M-x emacs-mcp-server-start   ; Start the server on 127.0.0.1:8585
;;   M-x emacs-mcp-server-stop    ; Stop the server
;;
;; Endpoints:
;;   GET  /mcp/screenshot     - Capture frame as base64 PNG
;;   POST /mcp/eval           - Execute elisp code
;;   GET  /mcp/buffers        - List all buffers with metadata
;;   GET  /mcp/windows        - List window configuration
;;   GET  /mcp/buffer/:name   - Read buffer contents
;;   POST /mcp/navigate       - Open file or jump to location
;;   POST /mcp/type           - Insert text at point
;;   POST /mcp/key            - Send key sequence
;;   GET  /mcp/images         - Extract image paths from current buffer
;;   GET  /mcp/events         - SSE stream for events (keystrokes, buffer changes)

;;; Code:

(require 'json)
(require 'cl-lib)

;; Defer simple-httpd loading until server start
(declare-function httpd-start "simple-httpd")
(declare-function httpd-stop "simple-httpd")
(declare-function httpd-send-header "simple-httpd")
(declare-function httpd-error "simple-httpd")
(declare-function httpd-log "simple-httpd")
(defvar httpd-port)
(defvar httpd-host)

;;; Customization

(defgroup emacs-mcp-server nil
  "HTTP server for Claude Code MCP integration."
  :group 'tools
  :prefix "emacs-mcp-server-")

(defcustom emacs-mcp-server-port 8585
  "Port for the MCP server."
  :type 'integer
  :group 'emacs-mcp-server)

(defcustom emacs-mcp-server-host "127.0.0.1"
  "Host address for the MCP server (localhost only for security)."
  :type 'string
  :group 'emacs-mcp-server)

(defcustom emacs-mcp-server-max-buffer-size (* 1024 1024)
  "Maximum buffer content size to return (1MB default)."
  :type 'integer
  :group 'emacs-mcp-server)

(defcustom emacs-mcp-server-screenshot-chunk-height 1080
  "Height of screenshot chunks for pagination."
  :type 'integer
  :group 'emacs-mcp-server)

;;; Internal State

(defvar emacs-mcp-server--running nil
  "Non-nil if the MCP server is running.")

(defvar emacs-mcp-server--event-clients nil
  "List of SSE client processes.")

(defvar emacs-mcp-server--event-buffer nil
  "Buffer for collecting events before sending.")

(defvar emacs-mcp-server--keystroke-hook-installed nil
  "Non-nil if keystroke hook is installed.")

;;; Utility Functions

(defun emacs-mcp-server--json-response (proc data &optional status)
  "Send JSON DATA to PROC with optional STATUS code."
  (let* ((json-str (json-encode data))
         (status-code (or status 200))
         (headers (format "HTTP/1.1 %d %s\r\nContent-Type: application/json; charset=utf-8\r\nContent-Length: %d\r\nAccess-Control-Allow-Origin: *\r\nConnection: close\r\n\r\n"
                          status-code
                          (emacs-mcp-server--status-message status-code)
                          (string-bytes json-str))))
    (process-send-string proc headers)
    (process-send-string proc json-str)))

(defun emacs-mcp-server--status-message (code)
  "Return HTTP status message for CODE."
  (pcase code
    (200 "OK")
    (201 "Created")
    (400 "Bad Request")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (500 "Internal Server Error")
    (_ "Unknown")))

(defun emacs-mcp-server--error-response (proc status message)
  "Send error response to PROC with STATUS and MESSAGE."
  (emacs-mcp-server--json-response
   proc
   `((error . t)
     (status . ,status)
     (message . ,message))
   status))

(defun emacs-mcp-server--parse-path (uri)
  "Parse URI into path and query parameters."
  (let* ((parts (split-string uri "?" t))
         (path (car parts))
         (query-string (cadr parts))
         (query-params nil))
    (when query-string
      (dolist (pair (split-string query-string "&"))
        (let ((kv (split-string pair "=")))
          (when (= (length kv) 2)
            (push (cons (car kv) (cadr kv)) query-params)))))
    (cons path (nreverse query-params))))

(defun emacs-mcp-server--parse-json-body (body)
  "Parse JSON BODY, returning nil on error."
  (condition-case err
      (json-read-from-string body)
    (error
     (message "MCP Server: JSON parse error: %s" (error-message-string err))
     nil)))

(defun emacs-mcp-server--url-decode (str)
  "URL decode STR."
  (decode-coding-string
   (url-unhex-string (replace-regexp-in-string "\\+" " " str))
   'utf-8))

;;; Screenshot Implementation

(defun emacs-mcp-server--capture-frame (&optional frame)
  "Capture FRAME as PNG, return base64 encoded string.
Requires Emacs 29+ built with Cairo support."
  (unless (fboundp 'x-export-frames)
    (error "x-export-frames not available (requires Emacs 29+ with Cairo)"))
  (let* ((frame (or frame (selected-frame)))
         (png-data (x-export-frames frame 'png)))
    (base64-encode-string png-data t)))

(defun emacs-mcp-server--capture-with-scroll (&optional chunk)
  "Capture screenshot with scroll position for CHUNK (0-indexed).
Returns base64 PNG and total chunk count."
  (let* ((frame (selected-frame))
         (frame-height (frame-pixel-height frame))
         (chunk-height emacs-mcp-server-screenshot-chunk-height)
         (total-chunks (max 1 (ceiling (/ (float frame-height) chunk-height))))
         (chunk-idx (or chunk 0)))
    ;; For now, just capture the visible frame
    ;; Full scroll capture would require manipulating window scroll positions
    (list :image (emacs-mcp-server--capture-frame frame)
          :chunk chunk-idx
          :total_chunks total-chunks
          :frame_height frame-height)))

;;; Buffer Operations

(defun emacs-mcp-server--buffer-info (buf)
  "Return metadata alist for buffer BUF."
  (with-current-buffer buf
    `((name . ,(buffer-name))
      (file . ,(or (buffer-file-name) :null))
      (mode . ,(symbol-name major-mode))
      (modified . ,(if (buffer-modified-p) t :false))
      (size . ,(buffer-size))
      (readonly . ,(if buffer-read-only t :false))
      (visible . ,(if (get-buffer-window buf 'visible) t :false)))))

(defun emacs-mcp-server--list-buffers ()
  "Return list of all buffer metadata."
  (mapcar #'emacs-mcp-server--buffer-info (buffer-list)))

(defun emacs-mcp-server--read-buffer (name &optional start end)
  "Read contents of buffer NAME, optionally from START to END lines."
  (let ((buf (get-buffer name)))
    (unless buf
      (error "Buffer not found: %s" name))
    (with-current-buffer buf
      (let* ((total-lines (count-lines (point-min) (point-max)))
             (start-line (or start 1))
             (end-line (or end total-lines))
             (start-pos (save-excursion
                          (goto-char (point-min))
                          (forward-line (1- start-line))
                          (point)))
             (end-pos (save-excursion
                        (goto-char (point-min))
                        (forward-line end-line)
                        (point)))
             (content (buffer-substring-no-properties start-pos end-pos)))
        ;; Truncate if too large
        (when (> (length content) emacs-mcp-server-max-buffer-size)
          (setq content (substring content 0 emacs-mcp-server-max-buffer-size)))
        `((name . ,name)
          (content . ,content)
          (start_line . ,start-line)
          (end_line . ,end-line)
          (total_lines . ,total-lines)
          (truncated . ,(> (length content) emacs-mcp-server-max-buffer-size)))))))

;;; Window Operations

(defun emacs-mcp-server--window-info (win)
  "Return metadata for window WIN."
  (let ((buf (window-buffer win)))
    `((buffer . ,(buffer-name buf))
      (file . ,(or (buffer-file-name buf) :null))
      (point . ,(window-point win))
      (start . ,(window-start win))
      (end . ,(window-end win t))
      (width . ,(window-width win))
      (height . ,(window-height win))
      (left . ,(nth 0 (window-edges win)))
      (top . ,(nth 1 (window-edges win)))
      (dedicated . ,(window-dedicated-p win))
      (selected . ,(eq win (selected-window))))))

(defun emacs-mcp-server--list-windows ()
  "Return list of all window metadata."
  (let ((windows nil))
    (walk-windows
     (lambda (win)
       (push (emacs-mcp-server--window-info win) windows))
     nil t)
    (nreverse windows)))

;;; Navigation

(defun emacs-mcp-server--navigate (file &optional line column)
  "Open FILE and optionally go to LINE and COLUMN."
  (find-file file)
  (when line
    (goto-char (point-min))
    (forward-line (1- line))
    (when column
      (forward-char (1- column))))
  `((success . t)
    (file . ,file)
    (line . ,(line-number-at-pos))
    (column . ,(current-column))
    (buffer . ,(buffer-name))))

;;; Input Simulation

(defun emacs-mcp-server--type-text (text)
  "Insert TEXT at point in current buffer."
  (insert text)
  `((success . t)
    (inserted . ,text)
    (buffer . ,(buffer-name))
    (point . ,(point))))

(defun emacs-mcp-server--send-keys (keys)
  "Execute key sequence KEYS."
  (let ((key-seq (kbd keys)))
    (execute-kbd-macro key-seq)
    `((success . t)
      (keys . ,keys)
      (buffer . ,(buffer-name))
      (point . ,(point)))))

;;; Image Extraction

(defun emacs-mcp-server--extract-images ()
  "Extract image file paths from current buffer."
  (let ((images nil))
    (save-excursion
      (goto-char (point-min))
      ;; Look for image display properties
      (while (not (eobp))
        (let ((display (get-text-property (point) 'display)))
          (when (and display (listp display))
            (let ((image-spec (if (eq (car display) 'image)
                                  display
                                (and (eq (car display) 'space)
                                       (plist-get (cdr display) :image)))))
              (when image-spec
                (let ((file (plist-get (cdr image-spec) :file)))
                  (when file
                    (push file images)))))))
        (forward-char 1)))
    ;; Also check for org-mode image links
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\(file:\\)?\\([^]]+\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\)\\)\\]" nil t)
        (let ((path (match-string 2)))
          (when (file-exists-p path)
            (push path images)))))
    ;; Check for markdown image syntax
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "!\\[[^]]*\\](\\([^)]+\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\)\\))" nil t)
        (let ((path (match-string 1)))
          (when (file-exists-p path)
            (push path images)))))
    `((buffer . ,(buffer-name))
      (images . ,(vconcat (delete-dups (nreverse images)))))))

;;; Eval Implementation

(defun emacs-mcp-server--eval-code (code)
  "Evaluate elisp CODE and return result."
  (condition-case err
      (let* ((form (read code))
             (result (eval form t))
             (printed (prin1-to-string result)))
        `((success . t)
          (result . ,printed)
          (type . ,(symbol-name (type-of result)))))
    (error
     `((success . :false)
       (error . ,(error-message-string err))
       (error_type . ,(symbol-name (car err)))))))

;;; SSE Events Implementation

(defvar emacs-mcp-server--last-buffer nil
  "Last observed buffer for change detection.")

(defvar emacs-mcp-server--last-point nil
  "Last observed point position.")

(defun emacs-mcp-server--send-sse-event (event-type data)
  "Send SSE event with EVENT-TYPE and DATA to all connected clients."
  (let ((msg (format "event: %s\ndata: %s\n\n"
                     event-type
                     (json-encode data))))
    (dolist (client (copy-sequence emacs-mcp-server--event-clients))
      (condition-case nil
          (process-send-string client msg)
        (error
         (setq emacs-mcp-server--event-clients
               (delq client emacs-mcp-server--event-clients)))))))

(defun emacs-mcp-server--keystroke-hook ()
  "Hook function to capture keystrokes."
  (when (and this-command emacs-mcp-server--event-clients)
    (emacs-mcp-server--send-sse-event
     "keystroke"
     `((key . ,(key-description (this-command-keys)))
       (command . ,(symbol-name this-command))
       (buffer . ,(buffer-name))
       (timestamp . ,(float-time))))))

(defun emacs-mcp-server--buffer-change-hook ()
  "Hook function to detect buffer changes."
  (when emacs-mcp-server--event-clients
    (let ((current-buffer-name (buffer-name))
          (current-point (point)))
      (when (or (not (equal current-buffer-name emacs-mcp-server--last-buffer))
                (not (equal current-point emacs-mcp-server--last-point)))
        (emacs-mcp-server--send-sse-event
         "buffer_change"
         `((buffer . ,current-buffer-name)
           (point . ,current-point)
           (file . ,(or (buffer-file-name) :null))
           (mode . ,(symbol-name major-mode))
           (timestamp . ,(float-time))))
        (setq emacs-mcp-server--last-buffer current-buffer-name
              emacs-mcp-server--last-point current-point)))))

(defun emacs-mcp-server--setup-event-hooks ()
  "Install hooks for event streaming."
  (unless emacs-mcp-server--keystroke-hook-installed
    (add-hook 'pre-command-hook #'emacs-mcp-server--keystroke-hook)
    (add-hook 'post-command-hook #'emacs-mcp-server--buffer-change-hook)
    (setq emacs-mcp-server--keystroke-hook-installed t)))

(defun emacs-mcp-server--teardown-event-hooks ()
  "Remove event hooks."
  (when emacs-mcp-server--keystroke-hook-installed
    (remove-hook 'pre-command-hook #'emacs-mcp-server--keystroke-hook)
    (remove-hook 'post-command-hook #'emacs-mcp-server--buffer-change-hook)
    (setq emacs-mcp-server--keystroke-hook-installed nil)))

(defun emacs-mcp-server--handle-sse-connection (proc)
  "Set up SSE connection for PROC."
  (push proc emacs-mcp-server--event-clients)
  (emacs-mcp-server--setup-event-hooks)
  ;; Send SSE headers
  (process-send-string proc "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\r\nConnection: keep-alive\r\nAccess-Control-Allow-Origin: *\r\n\r\n")
  ;; Send initial connection event
  (process-send-string proc (format "event: connected\ndata: %s\n\n"
                                    (json-encode `((status . "connected")
                                                   (timestamp . ,(float-time))))))
  ;; Set up sentinel for cleanup
  (set-process-sentinel
   proc
   (lambda (process _event)
     (setq emacs-mcp-server--event-clients
           (delq process emacs-mcp-server--event-clients))
     (when (null emacs-mcp-server--event-clients)
       (emacs-mcp-server--teardown-event-hooks)))))

;;; HTTP Request Handler

(defun emacs-mcp-server--handle-request (proc request)
  "Handle HTTP REQUEST on PROC."
  (let* ((lines (split-string request "\r\n"))
         (request-line (car lines))
         (parts (split-string request-line " "))
         (method (nth 0 parts))
         (uri (nth 1 parts))
         (parsed (emacs-mcp-server--parse-path uri))
         (path (car parsed))
         (query (cdr parsed))
         ;; Extract body for POST requests
         (body-start (string-match "\r\n\r\n" request))
         (body (when body-start
                 (substring request (+ body-start 4)))))

    (httpd-log `(mcp ,method ,path))

    (condition-case err
        (pcase (list method path)
          ;; Health check
          (`("GET" "/mcp/health")
           (emacs-mcp-server--json-response
            proc `((status . "ok")
                   (version . "1.0.0")
                   (emacs_version . ,emacs-version))))

          ;; Screenshot
          (`("GET" "/mcp/screenshot")
           (let* ((chunk (when-let ((c (cdr (assoc "chunk" query))))
                           (string-to-number c)))
                  (result (emacs-mcp-server--capture-with-scroll chunk)))
             (emacs-mcp-server--json-response proc result)))

          ;; Eval
          (`("POST" "/mcp/eval")
           (let* ((json-data (emacs-mcp-server--parse-json-body body))
                  (code (cdr (assoc 'code json-data))))
             (if code
                 (emacs-mcp-server--json-response
                  proc (emacs-mcp-server--eval-code code))
               (emacs-mcp-server--error-response proc 400 "Missing 'code' field"))))

          ;; List buffers
          (`("GET" "/mcp/buffers")
           (emacs-mcp-server--json-response
            proc `((buffers . ,(vconcat (emacs-mcp-server--list-buffers))))))

          ;; List windows
          (`("GET" "/mcp/windows")
           (emacs-mcp-server--json-response
            proc `((windows . ,(vconcat (emacs-mcp-server--list-windows))))))

          ;; Read buffer - handle /mcp/buffer/:name pattern
          (`("GET" ,(pred (lambda (p) (string-prefix-p "/mcp/buffer/" p))))
           (let* ((name-encoded (substring path (length "/mcp/buffer/")))
                  (name (emacs-mcp-server--url-decode name-encoded))
                  (start (when-let ((s (cdr (assoc "start" query))))
                           (string-to-number s)))
                  (end (when-let ((e (cdr (assoc "end" query))))
                         (string-to-number e))))
             (condition-case err
                 (emacs-mcp-server--json-response
                  proc (emacs-mcp-server--read-buffer name start end))
               (error
                (emacs-mcp-server--error-response proc 404 (error-message-string err))))))

          ;; Navigate
          (`("POST" "/mcp/navigate")
           (let* ((json-data (emacs-mcp-server--parse-json-body body))
                  (file (cdr (assoc 'file json-data)))
                  (line (cdr (assoc 'line json-data)))
                  (column (cdr (assoc 'column json-data))))
             (if file
                 (emacs-mcp-server--json-response
                  proc (emacs-mcp-server--navigate file line column))
               (emacs-mcp-server--error-response proc 400 "Missing 'file' field"))))

          ;; Type text
          (`("POST" "/mcp/type")
           (let* ((json-data (emacs-mcp-server--parse-json-body body))
                  (text (cdr (assoc 'text json-data))))
             (if text
                 (emacs-mcp-server--json-response
                  proc (emacs-mcp-server--type-text text))
               (emacs-mcp-server--error-response proc 400 "Missing 'text' field"))))

          ;; Send keys
          (`("POST" "/mcp/key")
           (let* ((json-data (emacs-mcp-server--parse-json-body body))
                  (keys (cdr (assoc 'keys json-data))))
             (if keys
                 (emacs-mcp-server--json-response
                  proc (emacs-mcp-server--send-keys keys))
               (emacs-mcp-server--error-response proc 400 "Missing 'keys' field"))))

          ;; Extract images
          (`("GET" "/mcp/images")
           (emacs-mcp-server--json-response
            proc (emacs-mcp-server--extract-images)))

          ;; SSE events stream
          (`("GET" "/mcp/events")
           (emacs-mcp-server--handle-sse-connection proc))

          ;; CORS preflight
          (`("OPTIONS" ,_)
           (process-send-string
            proc
            "HTTP/1.1 200 OK\r\nAccess-Control-Allow-Origin: *\r\nAccess-Control-Allow-Methods: GET, POST, OPTIONS\r\nAccess-Control-Allow-Headers: Content-Type\r\nContent-Length: 0\r\n\r\n"))

          ;; 404 for unknown paths
          (_
           (emacs-mcp-server--error-response proc 404 (format "Unknown endpoint: %s %s" method path))))

      (error
       (message "MCP Server error: %s" (error-message-string err))
       (emacs-mcp-server--error-response proc 500 (error-message-string err))))))

;;; Server Process Management

(defvar emacs-mcp-server--process nil
  "The server network process.")

(defun emacs-mcp-server--filter (proc string)
  "Filter function for PROC, handling incoming STRING."
  (let ((pending (process-get proc :pending)))
    (setq pending (concat pending string))
    ;; Check if we have a complete HTTP request
    (when (string-match "\r\n\r\n" pending)
      (let ((content-length 0)
            (headers-end (match-end 0)))
        ;; Check for Content-Length
        (when (string-match "Content-Length: \\([0-9]+\\)" pending)
          (setq content-length (string-to-number (match-string 1 pending))))
        ;; Check if we have the full body
        (when (>= (length pending) (+ headers-end content-length))
          (let ((request (substring pending 0 (+ headers-end content-length))))
            (setq pending (substring pending (+ headers-end content-length)))
            (emacs-mcp-server--handle-request proc request)))))
    (process-put proc :pending pending)))

(defun emacs-mcp-server--sentinel (proc event)
  "Sentinel for PROC handling EVENT."
  (when (string-match-p "\\(deleted\\|connection broken\\|finished\\)" event)
    (setq emacs-mcp-server--event-clients
          (delq proc emacs-mcp-server--event-clients))))

(defun emacs-mcp-server--accept (_server client _message)
  "Accept CLIENT connection on server."
  (set-process-filter client #'emacs-mcp-server--filter)
  (set-process-sentinel client #'emacs-mcp-server--sentinel)
  (process-put client :pending ""))

;;;###autoload
(defun emacs-mcp-server-start ()
  "Start the Emacs MCP server."
  (interactive)
  (if emacs-mcp-server--running
      (message "MCP Server already running. Use emacs-mcp-server-stop first.")
    ;; Ensure simple-httpd is available for logging
    (require 'simple-httpd)

  (condition-case err
      (progn
        (setq emacs-mcp-server--process
              (make-network-process
               :name "emacs-mcp-server"
               :buffer nil
               :server t
               :host emacs-mcp-server-host
               :service emacs-mcp-server-port
               :family 'ipv4
               :filter #'emacs-mcp-server--filter
               :sentinel #'emacs-mcp-server--sentinel
               :log #'emacs-mcp-server--accept
               :noquery t))
        (setq emacs-mcp-server--running t)
        (message "MCP Server started on %s:%d"
                 emacs-mcp-server-host
                 emacs-mcp-server-port))
      (error
       (message "Failed to start MCP Server: %s" (error-message-string err))))))

;;;###autoload
(defun emacs-mcp-server-stop ()
  "Stop the Emacs MCP server."
  (interactive)
  (when emacs-mcp-server--process
    (delete-process emacs-mcp-server--process)
    (setq emacs-mcp-server--process nil))
  ;; Clean up SSE clients
  (dolist (client emacs-mcp-server--event-clients)
    (ignore-errors (delete-process client)))
  (setq emacs-mcp-server--event-clients nil)
  ;; Remove hooks
  (emacs-mcp-server--teardown-event-hooks)
  (setq emacs-mcp-server--running nil)
  (message "MCP Server stopped."))

(defun emacs-mcp-server-status ()
  "Display MCP server status."
  (interactive)
  (if emacs-mcp-server--running
      (message "MCP Server running on %s:%d (%d SSE clients)"
               emacs-mcp-server-host
               emacs-mcp-server-port
               (length emacs-mcp-server--event-clients))
    (message "MCP Server is not running.")))

(provide 'emacs-mcp-server)
;;; emacs-mcp-server.el ends here
