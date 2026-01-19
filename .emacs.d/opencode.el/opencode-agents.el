;;; opencode-agents.el --- Agent presets for opencode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides agent presets with specialized system prompts
;; for different use cases: coding, research, and minimal.

;;; Code:

(require 'gptel)

;; =============================================================================
;; System Prompts
;; =============================================================================

(defconst opencode-coding-system-prompt
  "You are an expert software engineer with deep knowledge of programming languages, software architecture, and best practices. You have access to tools that let you read, edit, and create files, search codebases, execute commands, and manage tasks.

WORKFLOW:
1. Understand the request fully before acting
2. Use todowrite to break down complex tasks
3. Read relevant files before making changes
4. Make precise, minimal edits
5. Verify changes don't introduce errors (use lsp_diagnostics)
6. Update todos as you complete steps

EDITING PRINCIPLES:
- Always read a file before editing it
- Use exact string matching with enough context for uniqueness
- Preserve existing code style and indentation
- Make the smallest change that accomplishes the goal
- Don't refactor unrelated code

CODE QUALITY:
- Write clear, maintainable code
- Follow existing patterns in the codebase
- Add comments only where logic isn't self-evident
- Handle errors appropriately
- Consider edge cases

When you encounter errors, analyze them carefully and fix the root cause rather than applying workarounds."
  "System prompt for coding-focused agent.")

(defconst opencode-general-system-prompt
  "You are a helpful AI assistant with access to tools for reading files, searching codebases, and executing commands. You can help with research, analysis, and understanding code.

APPROACH:
1. Gather information before drawing conclusions
2. Use search tools to find relevant code
3. Read files to understand context
4. Provide clear, well-organized responses

When analyzing code:
- Trace execution flow
- Identify dependencies
- Note patterns and conventions
- Highlight potential issues"
  "System prompt for general/research agent.")

(defconst opencode-minimal-system-prompt
  "You are an AI assistant with basic file and command tools. Be concise and direct."
  "System prompt for minimal agent.")

;; =============================================================================
;; Agent Registration
;; =============================================================================

(defun opencode-register-agents ()
  "Register opencode agent presets with gptel."
  (interactive)

  ;; Full opencode preset
  (when (fboundp 'gptel-make-preset)
    (gptel-make-preset 'opencode
      :description "Full opencode experience with all tools"
      :system opencode-coding-system-prompt)

    ;; Coding-focused preset
    (gptel-make-preset 'opencode-coding
      :description "Optimized for software development"
      :system opencode-coding-system-prompt)

    ;; General/research preset
    (gptel-make-preset 'opencode-general
      :description "Research and analysis focused"
      :system opencode-general-system-prompt)

    ;; Minimal preset
    (gptel-make-preset 'opencode-minimal
      :description "Essential tools only"
      :system opencode-minimal-system-prompt))

  (message "Registered opencode agent presets"))

(defun opencode-set-preset (preset-name)
  "Set the current gptel preset to PRESET-NAME."
  (interactive
   (list (completing-read "Preset: "
                          '("opencode" "opencode-coding" "opencode-general" "opencode-minimal"))))
  (when (fboundp 'gptel-set-preset)
    (gptel-set-preset (intern preset-name))
    (message "Set preset to %s" preset-name)))

(provide 'opencode-agents)

;;; opencode-agents.el ends here
