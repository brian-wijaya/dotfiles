;;; opencode-descriptions.el --- Rich tool descriptions for opencode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides comprehensive tool descriptions that guide LLM behavior.
;; Each description includes usage patterns, constraints, and examples.

;;; Code:

(require 'cl-lib)

(defconst opencode-tool-descriptions
  '(("Read" . "Read a file from the filesystem with line numbers.

USAGE:
- Provide an absolute path to read the file
- Use offset and limit for large files (offset is 0-indexed line number)
- Returns content with line numbers for precise editing

IMPORTANT:
- Always read a file before editing it
- For large files (>500 lines), use offset/limit to read sections
- Line numbers in output start at 1

EXAMPLES:
- Read entire file: {\"file_path\": \"/path/to/file.py\"}
- Read lines 100-150: {\"file_path\": \"/path/to/file.py\", \"offset\": 99, \"limit\": 50}")

    ("edit" . "Edit a file by replacing exact string matches.

USAGE:
- Provide the exact string to find (old_string) and replacement (new_string)
- old_string must match EXACTLY including whitespace and indentation
- Use replace_all=true to replace all occurrences

IMPORTANT:
- ALWAYS read the file first before editing
- Include enough context in old_string to ensure unique match
- If multiple matches found and replace_all is false, the edit will fail
- Preserve exact indentation in both old_string and new_string

COMMON ERRORS:
- \"Could not find text\": old_string doesn't match exactly (check whitespace)
- \"Found N matches\": Need more context or use replace_all=true")

    ("create_file" . "Create a new file with the specified content.

USAGE:
- Provide absolute path and complete file content
- Parent directories are created automatically
- Will error if file already exists (use edit for existing files)")

    ("Glob" . "Find files matching a glob pattern.

USAGE:
- Use shell glob patterns: *, ?, [abc]
- Optional path parameter to search in specific directory
- Returns up to 100 matching files

PATTERNS:
- \"*.py\": All Python files in current directory
- \"test_*.py\": Files starting with test_")

    ("grep" . "Search file contents using ripgrep.

USAGE:
- pattern: Regular expression to search for
- path: Directory to search in (default: current directory)
- file_pattern: Glob to filter files (e.g., \"*.py\")

OUTPUT:
- Returns matching lines with file:line:content format
- Limited to 200 results")

    ("Bash" . "Execute a shell command.

USAGE:
- command: The shell command to execute
- timeout: Optional timeout in seconds (default: 120)

SECURITY:
- Some commands require user approval
- Dangerous commands (sudo, rm -rf) may be blocked

IMPORTANT:
- Prefer specialized tools (Read, edit, Glob) over shell equivalents
- Use absolute paths when possible")

    ("todowrite" . "Create or update a structured todo list for complex tasks.

USAGE:
- todos: Array of todo items with content, status, and priority

STRUCTURE:
Each todo item should have:
- content: Description of the task
- status: \"pending\", \"in_progress\", \"completed\", or \"cancelled\"
- priority: \"high\", \"medium\", or \"low\" (optional)

WHEN TO USE:
- Breaking down complex multi-step tasks
- Tracking progress on large changes")

    ("todoread" . "Read the current todo list to check progress.")

    ("lsp_diagnostics" . "Get errors and warnings from the language server.

USAGE:
- file_path: Optional path to check specific file
- Returns compiler errors, type errors, and warnings

WHEN TO USE:
- After editing a file to verify no errors introduced
- When investigating build failures")

    ("lsp_symbols" . "Search for symbols across the workspace.

USAGE:
- query: Search string to match symbol names

RETURNS:
- Symbol name, kind, file location, line number

WHEN TO USE:
- Finding where a function/class is defined
- Understanding code structure"))
  "Alist mapping tool names to their detailed descriptions.")

(defun opencode-get-description (tool-name)
  "Get the description for TOOL-NAME."
  (or (cdr (assoc tool-name opencode-tool-descriptions))
      (format "Tool: %s" tool-name)))

(provide 'opencode-descriptions)

;;; opencode-descriptions.el ends here
