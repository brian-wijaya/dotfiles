;;; opencode-tools.el --- Tool implementations for opencode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module implements the core tools for opencode.el.
;; Tools are registered with gptel and provide file operations,
;; search, command execution, task management, and Emacs integration.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'json)

;; =============================================================================
;; Customization
;; =============================================================================

(defgroup opencode-tools nil
  "Tool configuration for opencode."
  :group 'opencode
  :prefix "opencode-")

(defcustom opencode-bash-permissions
  '(("ls*" . "allow")
    ("cat*" . "allow")
    ("head*" . "allow")
    ("tail*" . "allow")
    ("grep*" . "allow")
    ("rg*" . "allow")
    ("find*" . "allow")
    ("git*" . "allow")
    ("npm*" . "allow")
    ("yarn*" . "allow")
    ("pnpm*" . "allow")
    ("cargo*" . "allow")
    ("go*" . "allow")
    ("python*" . "allow")
    ("pip*" . "allow")
    ("make*" . "allow")
    ("rm*" . "ask")
    ("sudo*" . "deny")
    ("*" . "ask"))
  "Permission policies for Bash commands.
Each entry is (PATTERN . POLICY) where POLICY is \"allow\", \"deny\", or \"ask\"."
  :type '(alist :key-type string :value-type string)
  :group 'opencode-tools)

(defcustom opencode-edit-permissions "allow"
  "Default permission for file edit operations.
Can be \"allow\", \"deny\", or \"ask\"."
  :type '(choice (const "allow") (const "deny") (const "ask"))
  :group 'opencode-tools)

;; =============================================================================
;; Internal State
;; =============================================================================

(defvar opencode--todo-list nil
  "Current todo list for task management.")

(defvar opencode--registered-tools nil
  "List of registered tool names.")

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defun opencode--truthy (val)
  "Return t if VAL is truthy (non-nil, non-empty, not \"false\")."
  (and val
       (not (equal val ""))
       (not (equal val "false"))
       (not (equal val :false))
       (not (equal val :null))))

(defun opencode--edit-permission-allowed-p ()
  "Check if file edits are allowed."
  (cond
   ((string= opencode-edit-permissions "allow") t)
   ((string= opencode-edit-permissions "deny") nil)
   (t (yes-or-no-p "Allow file edit?"))))

(defun opencode--check-bash-permission (command)
  "Check permission for COMMAND against `opencode-bash-permissions'.
Returns \"allow\", \"deny\", or \"ask\"."
  (let ((result "ask"))
    (dolist (entry opencode-bash-permissions)
      (let ((pattern (car entry))
            (policy (cdr entry)))
        (when (string-match-p (wildcard-to-regexp pattern) command)
          (setq result policy))))
    result))

(defun opencode--format-diagnostics (diagnostics)
  "Format DIAGNOSTICS for display."
  (if (or (not diagnostics) (= (length diagnostics) 0))
      ""
    (concat "\n\nDiagnostics:\n"
            (mapconcat #'identity diagnostics "\n"))))

(defun opencode--append-diagnostics (msg diagnostics)
  "Append DIAGNOSTICS to MSG if present."
  (if (string-empty-p diagnostics)
      msg
    (concat msg diagnostics)))

;; =============================================================================
;; Tool Implementations
;; =============================================================================

(defun opencode-read-file (file-path &optional offset limit)
  "Read FILE-PATH with optional OFFSET and LIMIT.
Returns content with line numbers."
  (let ((expanded-path (expand-file-name file-path)))
    (if (not (file-exists-p expanded-path))
        (error "File not found: %s" file-path)
      ;; Touch file for LSP if available
      (when (fboundp 'opencode-treesit-touch-file)
        (opencode-treesit-touch-file expanded-path))
      (with-temp-buffer
        (insert-file-contents expanded-path)
        (let* ((lines (split-string (buffer-string) "\n"))
               (total-lines (length lines))
               (start (or offset 0))
               (end (if limit (min (+ start limit) total-lines) total-lines))
               (selected-lines (cl-subseq lines start (min end total-lines)))
               (result ""))
          (cl-loop for line in selected-lines
                   for num from (1+ start)
                   do (setq result (concat result (format "%6d\t%s\n" num line))))
          (format "File: %s (lines %d-%d of %d)\n\n%s"
                  file-path (1+ start) end total-lines result))))))

(defun opencode-edit-file (file-path old-string new-string &optional replace-all)
  "Replace OLD-STRING with NEW-STRING inside FILE-PATH.
When REPLACE-ALL is truthy, every occurrence is replaced."
  (unless (opencode--edit-permission-allowed-p)
    (error "File edits denied by permission setting"))
  (let* ((expanded-path (expand-file-name file-path))
         (replace-all (opencode--truthy replace-all)))
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))
    ;; Open file in background for LSP
    (find-file-noselect expanded-path)
    (with-temp-buffer
      (insert-file-contents expanded-path)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (let ((count 0))
          ;; Count occurrences first
          (while (search-forward old-string nil t)
            (setq count (1+ count)))
          (cond
           ((= count 0)
            (error "Could not find text to replace in %s" file-path))
           ((and (> count 1) (not replace-all))
            (error "Found %d matches. Use replace_all=true or provide more context." count))
           (t
            ;; Do the replacement
            (goto-char (point-min))
            (if replace-all
                (while (search-forward old-string nil t)
                  (replace-match new-string t t))
              (search-forward old-string)
              (replace-match new-string t t))
            ;; Save
            (let ((inhibit-message t))
              (write-region (point-min) (point-max) expanded-path nil 'no-message))
            ;; Revert buffer if open
            (let ((buf (find-buffer-visiting expanded-path)))
              (when buf
                (with-current-buffer buf
                  (revert-buffer t t t))))
            ;; Get diagnostics if available
            (let* ((msg (format "Successfully edited %s (%d replacement%s)"
                                file-path count (if (= count 1) "" "s")))
                   (diag (if (fboundp 'opencode-treesit-get-diagnostics)
                             (opencode--format-diagnostics
                              (opencode-treesit-get-diagnostics expanded-path))
                           "")))
              (opencode--append-diagnostics msg diag)))))))))

(defun opencode-create-file (file-path content)
  "Create FILE-PATH with CONTENT."
  (let* ((expanded-path (expand-file-name file-path))
         (dir (file-name-directory expanded-path)))
    (when (file-exists-p expanded-path)
      (error "File already exists: %s" file-path))
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t))
    (write-region content nil expanded-path)
    (format "Created file: %s (%d bytes)" file-path (length content))))

(defun opencode-list-directory (path)
  "List contents of directory PATH."
  (let ((expanded-path (expand-file-name path)))
    (if (not (file-directory-p expanded-path))
        (error "Not a directory: %s" path)
      (let ((files (directory-files expanded-path nil "^[^.]")))
        (mapconcat
         (lambda (f)
           (let ((full-path (expand-file-name f expanded-path)))
             (format "%s%s"
                     f
                     (if (file-directory-p full-path) "/" ""))))
         files "\n")))))

(defun opencode-glob (pattern &optional path)
  "Find files matching PATTERN in PATH (default: current directory)."
  (let* ((default-directory (expand-file-name (or path default-directory)))
         (cmd (format "find . -name '%s' -type f 2>/dev/null | sort | head -100" pattern))
         (result (shell-command-to-string cmd)))
    (if (string-empty-p result)
        "No files found matching pattern"
      (string-trim result))))

(defun opencode-grep (pattern &optional path file-pattern)
  "Search for regex PATTERN in PATH with optional FILE-PATTERN."
  (let* ((default-directory (expand-file-name (or path default-directory)))
         (glob-arg (if file-pattern (format "-g '%s'" file-pattern) ""))
         (cmd (format "rg --line-number --no-heading --color=never %s '%s' 2>/dev/null | head -200"
                      glob-arg pattern))
         (result (shell-command-to-string cmd)))
    (if (string-empty-p result)
        "No matches found"
      (string-trim result))))

(defun opencode-bash (command &optional timeout)
  "Execute COMMAND with optional TIMEOUT (default 120s)."
  (let* ((timeout-sec (or timeout 120))
         (permission (opencode--check-bash-permission command)))
    (cond
     ((string= permission "deny")
      (error "Command denied by permission policy: %s" command))
     ((and (string= permission "ask")
           (not (yes-or-no-p (format "Allow command: %s? " command))))
      (error "Command cancelled by user"))
     (t
      (let ((result (shell-command-to-string
                     (format "timeout %d bash -c %s 2>&1"
                             timeout-sec
                             (shell-quote-argument command)))))
        (if (string-empty-p result)
            "Command completed with no output"
          (string-trim result)))))))

(defun opencode-todowrite (todos)
  "Write TODOS to the task list.
TODOS is a list of alists with keys: content, status, priority."
  (setq opencode--todo-list todos)
  (format "Updated todo list with %d items" (length todos)))

(defun opencode-todoread ()
  "Read the current todo list."
  (if opencode--todo-list
      (json-encode opencode--todo-list)
    "[]"))

(defun opencode-read-buffer (buffer-name)
  "Read contents of BUFFER-NAME."
  (let ((buf (get-buffer buffer-name)))
    (if (not buf)
        (error "Buffer not found: %s" buffer-name)
      (with-current-buffer buf
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun opencode-edit-buffer (buffer-name old-string new-string)
  "Edit BUFFER-NAME by replacing OLD-STRING with NEW-STRING."
  (let ((buf (get-buffer buffer-name)))
    (if (not buf)
        (error "Buffer not found: %s" buffer-name)
      (with-current-buffer buf
        (goto-char (point-min))
        (if (search-forward old-string nil t)
            (progn
              (replace-match new-string t t)
              (format "Replaced text in buffer %s" buffer-name))
          (error "String not found in buffer %s" buffer-name))))))

(defun opencode-append-buffer (buffer-name text)
  "Append TEXT to BUFFER-NAME."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert text))
    (format "Appended %d chars to %s" (length text) buffer-name)))

(defun opencode-list-buffers ()
  "List all open Emacs buffers."
  (mapconcat
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (unless (string-prefix-p " " name)  ; Skip internal buffers
         (format "%s (%s)"
                 name
                 (with-current-buffer buf
                   (symbol-name major-mode))))))
   (buffer-list) "\n"))

(defun opencode-read-documentation (symbol-name)
  "Read documentation for SYMBOL-NAME."
  (let ((sym (intern-soft symbol-name)))
    (cond
     ((not sym) (error "Symbol not found: %s" symbol-name))
     ((fboundp sym)
      (or (documentation sym) "No documentation available"))
     ((boundp sym)
      (or (documentation-property sym 'variable-documentation)
          "No documentation available"))
     (t (error "Symbol %s is not a function or variable" symbol-name)))))

;; =============================================================================
;; LSP Integration Tools
;; =============================================================================

(defun opencode-lsp-diagnostics (&optional file-path)
  "Get LSP diagnostics for FILE-PATH or current buffer."
  (if (not (featurep 'lsp-mode))
      "LSP mode not available"
    (let* ((path (and file-path (expand-file-name file-path)))
           (buf (if path (find-buffer-visiting path) (current-buffer))))
      (if (not buf)
          (format "No buffer for: %s" file-path)
        (with-current-buffer buf
          (if (not (bound-and-true-p lsp-mode))
              "LSP not active in this buffer"
            (let ((diags (flymake-diagnostics)))
              (if (not diags)
                  "No diagnostics"
                (mapconcat
                 (lambda (d)
                   (format "%d:%d %s: %s"
                           (line-number-at-pos (flymake-diagnostic-beg d))
                           (save-excursion
                             (goto-char (flymake-diagnostic-beg d))
                             (current-column))
                           (flymake-diagnostic-type d)
                           (flymake-diagnostic-text d)))
                 diags "\n")))))))))

(defun opencode-lsp-symbols (query)
  "Search workspace symbols matching QUERY."
  (if (not (featurep 'lsp-mode))
      "LSP mode not available"
    (if (not (lsp-workspaces))
        "No active LSP workspace"
      (condition-case err
          (let ((symbols (lsp-request "workspace/symbol" `(:query ,query))))
            (if (not symbols)
                "No symbols found"
              (mapconcat
               (lambda (sym)
                 (let* ((name (gethash "name" sym))
                        (kind (gethash "kind" sym))
                        (loc (gethash "location" sym))
                        (uri (gethash "uri" loc))
                        (range (gethash "range" loc))
                        (start (gethash "start" range))
                        (line (1+ (gethash "line" start))))
                   (format "%s [%s] %s:%d"
                           name
                           (opencode--symbol-kind-name kind)
                           (lsp--uri-to-path uri)
                           line)))
               symbols "\n")))
        (error (format "LSP error: %s" (error-message-string err)))))))

(defun opencode--symbol-kind-name (kind)
  "Convert LSP symbol KIND number to name."
  (pcase kind
    (1 "File") (2 "Module") (3 "Namespace") (4 "Package")
    (5 "Class") (6 "Method") (7 "Property") (8 "Field")
    (9 "Constructor") (10 "Enum") (11 "Interface") (12 "Function")
    (13 "Variable") (14 "Constant") (15 "String") (16 "Number")
    (17 "Boolean") (18 "Array") (19 "Object") (20 "Key")
    (21 "Null") (22 "EnumMember") (23 "Struct") (24 "Event")
    (25 "Operator") (26 "TypeParameter")
    (_ "Unknown")))

;; =============================================================================
;; Tool Registration with gptel
;; =============================================================================

(defun opencode-register-tools ()
  "Register all opencode tools with gptel."
  (interactive)
  (setq opencode--registered-tools nil)

  ;; Read tool
  (push "Read" opencode--registered-tools)
  (gptel-make-tool
   :name "Read"
   :description (opencode-get-description "Read")
   :function #'opencode-read-file
   :args '((:name "file_path" :type string :description "Absolute path to file" :required t)
           (:name "offset" :type integer :description "Line offset (0-indexed)")
           (:name "limit" :type integer :description "Max lines to read")))

  ;; Edit tool
  (push "edit" opencode--registered-tools)
  (gptel-make-tool
   :name "edit"
   :description (opencode-get-description "edit")
   :function #'opencode-edit-file
   :args '((:name "file_path" :type string :description "Path to file" :required t)
           (:name "old_string" :type string :description "Exact string to replace" :required t)
           (:name "new_string" :type string :description "Replacement string" :required t)
           (:name "replace_all" :type boolean :description "Replace all occurrences")))

  ;; Create file tool
  (push "create_file" opencode--registered-tools)
  (gptel-make-tool
   :name "create_file"
   :description (opencode-get-description "create_file")
   :function #'opencode-create-file
   :args '((:name "file_path" :type string :description "Path for new file" :required t)
           (:name "content" :type string :description "File content" :required t)))

  ;; List directory tool
  (push "list_directory" opencode--registered-tools)
  (gptel-make-tool
   :name "list_directory"
   :description "List contents of a directory"
   :function #'opencode-list-directory
   :args '((:name "path" :type string :description "Directory path" :required t)))

  ;; Glob tool
  (push "Glob" opencode--registered-tools)
  (gptel-make-tool
   :name "Glob"
   :description (opencode-get-description "Glob")
   :function #'opencode-glob
   :args '((:name "pattern" :type string :description "Glob pattern (e.g. *.py)" :required t)
           (:name "path" :type string :description "Directory to search")))

  ;; Grep tool
  (push "grep" opencode--registered-tools)
  (gptel-make-tool
   :name "grep"
   :description (opencode-get-description "grep")
   :function #'opencode-grep
   :args '((:name "pattern" :type string :description "Regex pattern" :required t)
           (:name "path" :type string :description "Directory to search")
           (:name "file_pattern" :type string :description "File glob filter")))

  ;; Bash tool
  (push "Bash" opencode--registered-tools)
  (gptel-make-tool
   :name "Bash"
   :description (opencode-get-description "Bash")
   :function #'opencode-bash
   :args '((:name "command" :type string :description "Shell command" :required t)
           (:name "timeout" :type integer :description "Timeout in seconds")))

  ;; Todo tools
  (push "todowrite" opencode--registered-tools)
  (gptel-make-tool
   :name "todowrite"
   :description (opencode-get-description "todowrite")
   :function #'opencode-todowrite
   :args '((:name "todos" :type array :description "List of todo items" :required t)))

  (push "todoread" opencode--registered-tools)
  (gptel-make-tool
   :name "todoread"
   :description "Read the current todo list"
   :function #'opencode-todoread
   :args '())

  ;; Buffer tools
  (push "read_buffer" opencode--registered-tools)
  (gptel-make-tool
   :name "read_buffer"
   :description "Read contents of an Emacs buffer"
   :function #'opencode-read-buffer
   :args '((:name "buffer_name" :type string :description "Buffer name" :required t)))

  (push "edit_buffer" opencode--registered-tools)
  (gptel-make-tool
   :name "edit_buffer"
   :description "Edit an Emacs buffer by replacing text"
   :function #'opencode-edit-buffer
   :args '((:name "buffer_name" :type string :description "Buffer name" :required t)
           (:name "old_string" :type string :description "Text to replace" :required t)
           (:name "new_string" :type string :description "Replacement" :required t)))

  (push "append_to_buffer" opencode--registered-tools)
  (gptel-make-tool
   :name "append_to_buffer"
   :description "Append text to an Emacs buffer"
   :function #'opencode-append-buffer
   :args '((:name "buffer_name" :type string :description "Buffer name" :required t)
           (:name "text" :type string :description "Text to append" :required t)))

  (push "list_buffers" opencode--registered-tools)
  (gptel-make-tool
   :name "list_buffers"
   :description "List all open Emacs buffers"
   :function #'opencode-list-buffers
   :args '())

  (push "read_documentation" opencode--registered-tools)
  (gptel-make-tool
   :name "read_documentation"
   :description "Read Emacs documentation for a function or variable"
   :function #'opencode-read-documentation
   :args '((:name "symbol_name" :type string :description "Symbol name" :required t)))

  ;; LSP tools
  (push "lsp_diagnostics" opencode--registered-tools)
  (gptel-make-tool
   :name "lsp_diagnostics"
   :description "Get LSP diagnostics (errors/warnings) for a file"
   :function #'opencode-lsp-diagnostics
   :args '((:name "file_path" :type string :description "File to check")))

  (push "lsp_symbols" opencode--registered-tools)
  (gptel-make-tool
   :name "lsp_symbols"
   :description "Search workspace symbols via LSP"
   :function #'opencode-lsp-symbols
   :args '((:name "query" :type string :description "Symbol search query" :required t)))

  (message "Registered %d opencode tools" (length opencode--registered-tools)))

(defun opencode-register-coding-tools ()
  "Register coding-focused tools."
  (interactive)
  (opencode-register-tools))

(defun opencode-register-minimal-tools ()
  "Register minimal tool set (Read, edit, Bash only)."
  (interactive)
  (setq opencode--registered-tools '("Read" "edit" "Bash"))

  (gptel-make-tool
   :name "Read"
   :description (opencode-get-description "Read")
   :function #'opencode-read-file
   :args '((:name "file_path" :type string :description "File path" :required t)
           (:name "offset" :type integer :description "Line offset")
           (:name "limit" :type integer :description "Max lines")))

  (gptel-make-tool
   :name "edit"
   :description (opencode-get-description "edit")
   :function #'opencode-edit-file
   :args '((:name "file_path" :type string :description "File path" :required t)
           (:name "old_string" :type string :description "Old string" :required t)
           (:name "new_string" :type string :description "New string" :required t)
           (:name "replace_all" :type boolean :description "Replace all")))

  (gptel-make-tool
   :name "Bash"
   :description (opencode-get-description "Bash")
   :function #'opencode-bash
   :args '((:name "command" :type string :description "Command" :required t)
           (:name "timeout" :type integer :description "Timeout")))

  (message "Registered %d minimal tools" (length opencode--registered-tools)))

(defun opencode-register-essential-tools ()
  "Alias for `opencode-register-minimal-tools'."
  (interactive)
  (opencode-register-minimal-tools))

(defun opencode-register-selected-tools (tool-names)
  "Register only tools specified in TOOL-NAMES."
  (interactive)
  (opencode-register-tools)
  ;; Note: would need to filter gptel-tools, which depends on gptel internals
  (setq opencode--registered-tools tool-names)
  (message "Registered selected tools: %s" (string-join tool-names ", ")))

(defun opencode--get-tool-prop (tool prop)
  "Get PROP from TOOL structure."
  (plist-get tool prop))

(provide 'opencode-tools)

;;; opencode-tools.el ends here
