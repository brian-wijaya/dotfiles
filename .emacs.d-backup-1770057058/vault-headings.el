;;; vault-headings.el --- GPT-powered heading generation -*- lexical-binding: t; -*-

;; Author: Brian Wijaya
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (gptel "0.9"))

;;; Commentary:
;; Generate org-mode headings for selected text using gptel/Claude.
;;
;; Usage:
;;   1. Select a region of text
;;   2. M-x vault-generate-heading
;;   3. A heading is generated and inserted above the selection

;;; Code:

(require 'gptel)

(defgroup vault-headings nil
  "GPT-powered heading generation."
  :group 'org)

(defcustom vault-heading-directive
  "You are a content analyst. Generate a single concise, descriptive heading (3-7 words) for the given text. Return ONLY the heading text - no asterisks, no markdown, no quotes, no explanations. Just the plain heading text."
  "System directive for heading generation."
  :type 'string
  :group 'vault-headings)

(defcustom vault-heading-level 1
  "Default org heading level (number of asterisks)."
  :type 'integer
  :group 'vault-headings)

(defvar vault-heading--pending nil
  "Marker for pending heading insertion point.")

(defun vault-heading--callback (response info)
  "Handle gptel RESPONSE for heading generation.
INFO contains request metadata."
  (if (stringp response)
      (let ((heading (string-trim response))
            (buf (plist-get info :buffer))
            (marker (plist-get info :context)))
        ;; Clean up any markdown artifacts
        (setq heading (replace-regexp-in-string "^[#*\"]+" "" heading))
        (setq heading (replace-regexp-in-string "[\"]+$" "" heading))
        (setq heading (string-trim heading))
        (when (and buf (buffer-live-p buf) marker)
          (with-current-buffer buf
            (save-excursion
              (goto-char marker)
              (beginning-of-line)
              (insert (make-string vault-heading-level ?*) " " heading "\n\n")
              (message "Heading inserted: %s" heading)))))
    (message "Heading generation failed: %s"
             (plist-get info :status))))

(defun vault-generate-heading ()
  "Generate a heading for the selected region using gptel.
The heading will be inserted above the selection."
  (interactive)
  (unless (region-active-p)
    (user-error "No region selected. Select text to generate a heading for"))
  (unless (fboundp 'gptel-request)
    (user-error "gptel is not available"))

  (let* ((start (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties start end))
         (marker (copy-marker start)))

    ;; Truncate if too long
    (when (> (length text) 2000)
      (setq text (concat (substring text 0 2000) "...")))

    (message "Generating heading...")
    (gptel-request
     (format "Generate a heading for this content:\n\n%s" text)
     :system vault-heading-directive
     :buffer (current-buffer)
     :context marker
     :callback #'vault-heading--callback)))

(defun vault-generate-heading-at-point ()
  "Generate a heading for the current paragraph.
Uses the paragraph at point if no region is selected."
  (interactive)
  (if (region-active-p)
      (vault-generate-heading)
    (save-excursion
      (let (start end)
        ;; Find paragraph bounds
        (backward-paragraph)
        (setq start (point))
        (forward-paragraph)
        (setq end (point))
        ;; Select and generate
        (goto-char start)
        (set-mark end)
        (vault-generate-heading)))))

(provide 'vault-headings)

;;; vault-headings.el ends here
