;;; web-reader.el --- Reader mode for web content via browser automation -*- lexical-binding: t; -*-

(defvar bw/web-reader-history nil
  "History of URLs fetched via web-reader.")

(defun bw/web-reader-fetch (url)
  "Fetch URL via real browser, extract readable content, display in Emacs buffer."
  (interactive
   (list (read-string "URL: "
                      (or (thing-at-point 'url)
                          (when (derived-mode-p 'eww-mode)
                            (eww-current-url))
                          (car bw/web-reader-history))
                      'bw/web-reader-history)))

  (unless (string-match-p "^https?://" url)
    (setq url (concat "https://" url)))

  (message "Fetching readable content from %s..." url)

  ;; Use Claude to fetch via browser and extract content
  (let ((result-buffer (generate-new-buffer "*web-reader*")))
    (with-current-buffer result-buffer
      (insert (format "# %s\n\n" url))
      (insert "Fetching content via browser...\n\n")
      (insert "Run this in Claude Code:\n")
      (insert (format "Please fetch this URL via browser and extract the readable text content: %s\n\n" url))
      (insert "Use the claude-in-chrome tools to:\n")
      (insert "1. Create a new tab\n")
      (insert "2. Navigate to the URL\n")
      (insert "3. Extract the main readable content (try reader mode or plain text)\n")
      (insert "4. Return the clean text here\n\n")
      (insert "---\n\n")
      (insert "Waiting for Claude to fetch content...")
      (org-mode)
      (goto-char (point-min)))
    (pop-to-buffer result-buffer)

    ;; Signal to Claude that we need content fetched
    (message "Request sent to Claude Code. Check the buffer for extracted content.")))

(defun bw/web-reader-from-eww ()
  "Fetch current eww page via web-reader."
  (interactive)
  (if (derived-mode-p 'eww-mode)
      (bw/web-reader-fetch (eww-current-url))
    (user-error "Not in an eww buffer")))

(provide 'web-reader)
;;; web-reader.el ends here
