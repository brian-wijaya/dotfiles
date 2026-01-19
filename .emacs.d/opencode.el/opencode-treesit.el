;;; opencode-treesit.el --- Tree-sitter and LSP integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides tree-sitter integration for code understanding
;; and LSP helpers for diagnostics and symbol search.

;;; Code:

(require 'cl-lib)

;; =============================================================================
;; Customization
;; =============================================================================

(defgroup opencode-treesit nil
  "Tree-sitter and LSP integration for opencode."
  :group 'opencode
  :prefix "opencode-treesit-")

(defcustom opencode-lsp-enabled t
  "Whether to enable LSP integration."
  :type 'boolean
  :group 'opencode-treesit)

(defcustom opencode-lsp-auto-start t
  "Whether to auto-start LSP when accessing files."
  :type 'boolean
  :group 'opencode-treesit)

(defcustom opencode-lsp-show-diagnostics t
  "Whether to show diagnostics after file operations."
  :type 'boolean
  :group 'opencode-treesit)

;; =============================================================================
;; LSP Helpers
;; =============================================================================

(defun opencode-treesit-touch-file (file-path)
  "Touch FILE-PATH to warm up LSP client.
Opens the file in background to trigger LSP initialization."
  (when (and opencode-lsp-enabled opencode-lsp-auto-start)
    (let ((buf (find-file-noselect file-path)))
      (with-current-buffer buf
        ;; Try to start LSP if available
        (when (and (featurep 'lsp-mode)
                   (not (bound-and-true-p lsp-mode)))
          (ignore-errors (lsp-deferred)))
        ;; Also try eglot
        (when (and (featurep 'eglot)
                   (not (bound-and-true-p eglot--managed-mode)))
          (ignore-errors (eglot-ensure)))))))

(defun opencode-treesit-get-diagnostics (file-path)
  "Get diagnostics for FILE-PATH.
Returns a list of diagnostic strings."
  (when opencode-lsp-show-diagnostics
    (let ((buf (find-buffer-visiting file-path)))
      (when buf
        (with-current-buffer buf
          (cond
           ;; Try flymake first (works with eglot and lsp-mode)
           ((and (bound-and-true-p flymake-mode)
                 (fboundp 'flymake-diagnostics))
            (let ((diags (flymake-diagnostics)))
              (mapcar
               (lambda (d)
                 (format "%d: %s"
                         (line-number-at-pos (flymake-diagnostic-beg d))
                         (flymake-diagnostic-text d)))
               diags)))
           ;; Try flycheck
           ((and (bound-and-true-p flycheck-mode)
                 (fboundp 'flycheck-overlay-errors-at))
            (let ((errors flycheck-current-errors))
              (mapcar
               (lambda (e)
                 (format "%d: %s"
                         (flycheck-error-line e)
                         (flycheck-error-message e)))
               errors)))
           ;; No diagnostics available
           (t nil)))))))

(defun opencode-treesit-ensure-lsp (file-path)
  "Ensure LSP is running for FILE-PATH."
  (when opencode-lsp-enabled
    (let ((buf (find-file-noselect file-path)))
      (with-current-buffer buf
        (cond
         ((featurep 'lsp-mode)
          (unless (bound-and-true-p lsp-mode)
            (lsp-deferred)))
         ((featurep 'eglot)
          (unless (bound-and-true-p eglot--managed-mode)
            (eglot-ensure))))))))

;; =============================================================================
;; Tree-sitter Helpers
;; =============================================================================

(defun opencode-treesit-available-p ()
  "Check if tree-sitter is available."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

(defun opencode-treesit-get-node-at-point ()
  "Get the tree-sitter node at point if available."
  (when (and (opencode-treesit-available-p)
             (treesit-parser-list))
    (treesit-node-at (point))))

(defun opencode-treesit-get-defun-name ()
  "Get the name of the function/method at point using tree-sitter."
  (when-let ((node (opencode-treesit-get-node-at-point)))
    (let ((defun-node (treesit-defun-at-point)))
      (when defun-node
        (treesit-defun-name defun-node)))))

(defun opencode-treesit-list-definitions (file-path)
  "List all function/class definitions in FILE-PATH."
  (let ((buf (find-file-noselect file-path)))
    (with-current-buffer buf
      (when (and (opencode-treesit-available-p)
                 (treesit-parser-list))
        (let ((root (treesit-buffer-root-node))
              (results nil))
          ;; This is a simplified version - would need language-specific queries
          ;; for full functionality
          (when root
            (dolist (child (treesit-node-children root t))
              (when (member (treesit-node-type child)
                            '("function_definition" "method_definition"
                              "class_definition" "function_declaration"))
                (push (format "%s: %s (line %d)"
                              (treesit-node-type child)
                              (or (treesit-defun-name child) "anonymous")
                              (line-number-at-pos (treesit-node-start child)))
                      results))))
          (nreverse results))))))

;; =============================================================================
;; Code Analysis
;; =============================================================================

(defun opencode-treesit-analyze-file (file-path)
  "Analyze FILE-PATH and return summary information."
  (let ((buf (find-file-noselect file-path)))
    (with-current-buffer buf
      (list
       :file file-path
       :mode major-mode
       :lines (count-lines (point-min) (point-max))
       :treesit (opencode-treesit-available-p)
       :lsp (or (bound-and-true-p lsp-mode)
                (bound-and-true-p eglot--managed-mode))
       :diagnostics (length (or (opencode-treesit-get-diagnostics file-path) nil))))))

(provide 'opencode-treesit)

;;; opencode-treesit.el ends here
