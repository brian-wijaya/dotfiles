;;; vault-sidebar.el --- Sidebar for browsing vault org files -*- lexical-binding: t; -*-

;; Author: Brian Wijaya
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;; A Day One / Evernote-style sidebar for browsing org files in ~/vault/org/.
;; Shows files grouped by directory with title + preview text.
;;
;; Usage:
;;   M-x vault-sidebar-toggle
;;
;; Navigation:
;;   n/j     - move down
;;   p/k     - move up
;;   RET     - open file
;;   TAB     - toggle directory collapse
;;   g       - refresh
;;   /       - filter
;;   q       - quit

;;; Code:

(require 'cl-lib)

(defgroup vault-sidebar nil
  "Sidebar for vault org files."
  :group 'org)

(defcustom vault-sidebar-root "~/vault/org/"
  "Root directory for vault org files."
  :type 'directory
  :group 'vault-sidebar)

(defcustom vault-sidebar-preview-length 150
  "Number of characters to show in preview."
  :type 'integer
  :group 'vault-sidebar)

(defcustom vault-sidebar-width 0.25
  "Width of sidebar as fraction of frame."
  :type 'float
  :group 'vault-sidebar)

(defvar vault-sidebar-buffer-name "*Vault*"
  "Name of the vault sidebar buffer.")

(defvar-local vault-sidebar--files nil
  "List of files in the sidebar.")

(defvar-local vault-sidebar--collapsed-dirs nil
  "List of collapsed directory paths.")

(defvar-local vault-sidebar--filter ""
  "Current filter string.")

;; Faces
(defface vault-sidebar-directory
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for directory names."
  :group 'vault-sidebar)

(defface vault-sidebar-file
  '((t (:inherit default)))
  "Face for file names."
  :group 'vault-sidebar)

(defface vault-sidebar-preview
  '((t (:inherit font-lock-comment-face)))
  "Face for preview text."
  :group 'vault-sidebar)

(defface vault-sidebar-selected
  '((t (:inherit highlight)))
  "Face for selected item."
  :group 'vault-sidebar)

;; File operations
(defun vault-sidebar--get-files ()
  "Get all org files in vault, sorted by modification time."
  (let ((files (directory-files-recursively
                (expand-file-name vault-sidebar-root)
                "\\.org$")))
    (sort files (lambda (a b)
                  (time-less-p (nth 5 (file-attributes b))
                               (nth 5 (file-attributes a)))))))

(defun vault-sidebar--extract-title (filepath)
  "Extract title from org file FILEPATH."
  (with-temp-buffer
    (insert-file-contents filepath nil 0 500)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+TITLE:[ \t]*\\(.+\\)$" nil t)
        (match-string 1)
      (file-name-base filepath))))

(defun vault-sidebar--extract-preview (filepath)
  "Extract preview text from org file FILEPATH."
  (with-temp-buffer
    (insert-file-contents filepath nil 0 2000)
    (goto-char (point-min))
    ;; Skip headers
    (while (and (not (eobp))
                (looking-at "^[#*:]"))
      (forward-line 1))
    ;; Get content
    (let ((start (point))
          (content ""))
      (forward-char (min vault-sidebar-preview-length
                         (- (point-max) (point))))
      (setq content (buffer-substring-no-properties start (point)))
      ;; Clean up
      (setq content (replace-regexp-in-string "[\n\t]+" " " content))
      (setq content (replace-regexp-in-string "  +" " " content))
      (string-trim content))))

(defun vault-sidebar--relative-path (filepath)
  "Get path relative to vault root for FILEPATH."
  (file-relative-name filepath (expand-file-name vault-sidebar-root)))

(defun vault-sidebar--get-directory (filepath)
  "Get parent directory name for FILEPATH relative to vault root."
  (let ((rel (vault-sidebar--relative-path filepath)))
    (if (string-match "/" rel)
        (file-name-directory rel)
      "")))

;; Rendering
(defun vault-sidebar--render ()
  "Render the sidebar contents."
  (let ((inhibit-read-only t)
        (files (vault-sidebar--get-files))
        (current-dir nil)
        (line-num 1))
    (erase-buffer)
    (insert (propertize "  VAULT\n" 'face 'bold))
    (insert (propertize (format "  %d files" (length files))
                        'face 'vault-sidebar-preview))
    (when (not (string-empty-p vault-sidebar--filter))
      (insert (propertize (format " [filter: %s]" vault-sidebar--filter)
                          'face 'vault-sidebar-preview)))
    (insert "\n\n")

    ;; Filter files
    (when (not (string-empty-p vault-sidebar--filter))
      (setq files (cl-remove-if-not
                   (lambda (f)
                     (let ((title (vault-sidebar--extract-title f)))
                       (string-match-p (regexp-quote vault-sidebar--filter)
                                       (downcase title))))
                   files)))

    (setq vault-sidebar--files nil)

    (dolist (file files)
      (let* ((dir (vault-sidebar--get-directory file))
             (title (vault-sidebar--extract-title file))
             (preview (vault-sidebar--extract-preview file))
             (collapsed (member dir vault-sidebar--collapsed-dirs)))

        ;; Directory header
        (when (and (not (string-empty-p dir))
                   (not (equal dir current-dir)))
          (setq current-dir dir)
          (insert (propertize (format "%s %s\n"
                                      (if collapsed "▶" "▼")
                                      (string-trim-right dir "/"))
                              'face 'vault-sidebar-directory
                              'vault-sidebar-dir dir
                              'vault-sidebar-type 'directory)))

        ;; File entry (if not collapsed)
        (unless (and collapsed (not (string-empty-p dir)))
          (let ((start (point)))
            (insert (propertize (format "  %s\n" title)
                                'face 'vault-sidebar-file
                                'vault-sidebar-file file
                                'vault-sidebar-type 'file))
            (when (not (string-empty-p preview))
              (insert (propertize (format "    %s\n"
                                          (if (> (length preview) 80)
                                              (concat (substring preview 0 80) "...")
                                            preview))
                                  'face 'vault-sidebar-preview
                                  'vault-sidebar-file file
                                  'vault-sidebar-type 'file-preview)))
            (push (cons line-num file) vault-sidebar--files)
            (cl-incf line-num)))))

    (goto-char (point-min))
    (forward-line 3)))

;; Commands
(defun vault-sidebar-toggle ()
  "Toggle the vault sidebar."
  (interactive)
  (let ((buf (get-buffer vault-sidebar-buffer-name)))
    (if (and buf (get-buffer-window buf))
        (delete-window (get-buffer-window buf))
      (vault-sidebar-open))))

(defun vault-sidebar-open ()
  "Open the vault sidebar."
  (interactive)
  (let ((buf (get-buffer-create vault-sidebar-buffer-name)))
    (with-current-buffer buf
      (vault-sidebar-mode)
      (vault-sidebar--render))
    (display-buffer-in-side-window
     buf
     `((side . left)
       (window-width . ,vault-sidebar-width)
       (window-parameters . ((no-delete-other-windows . t)))))
    (select-window (get-buffer-window buf))))

(defun vault-sidebar-refresh ()
  "Refresh the sidebar."
  (interactive)
  (vault-sidebar--render))

(defun vault-sidebar-open-file ()
  "Open the file at point."
  (interactive)
  (let ((file (get-text-property (point) 'vault-sidebar-file)))
    (when file
      (let ((win (or (window-right (selected-window))
                     (window-left (selected-window))
                     (split-window-right))))
        (select-window win)
        (find-file file)))))

(defun vault-sidebar-toggle-directory ()
  "Toggle collapse state of directory at point."
  (interactive)
  (let ((dir (get-text-property (point) 'vault-sidebar-dir)))
    (when dir
      (if (member dir vault-sidebar--collapsed-dirs)
          (setq vault-sidebar--collapsed-dirs
                (delete dir vault-sidebar--collapsed-dirs))
        (push dir vault-sidebar--collapsed-dirs))
      (vault-sidebar--render))))

(defun vault-sidebar-next ()
  "Move to next file entry."
  (interactive)
  (forward-line 1)
  (while (and (not (eobp))
              (not (get-text-property (point) 'vault-sidebar-file)))
    (forward-line 1)))

(defun vault-sidebar-prev ()
  "Move to previous file entry."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp))
              (not (get-text-property (point) 'vault-sidebar-file)))
    (forward-line -1)))

(defun vault-sidebar-filter ()
  "Filter files by title."
  (interactive)
  (setq vault-sidebar--filter
        (read-string "Filter: " vault-sidebar--filter))
  (vault-sidebar--render))

(defun vault-sidebar-clear-filter ()
  "Clear the filter."
  (interactive)
  (setq vault-sidebar--filter "")
  (vault-sidebar--render))

(defun vault-sidebar-quit ()
  "Quit the sidebar."
  (interactive)
  (quit-window))

;; Mode definition
(defvar vault-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'vault-sidebar-next)
    (define-key map (kbd "j") #'vault-sidebar-next)
    (define-key map (kbd "p") #'vault-sidebar-prev)
    (define-key map (kbd "k") #'vault-sidebar-prev)
    (define-key map (kbd "RET") #'vault-sidebar-open-file)
    (define-key map (kbd "TAB") #'vault-sidebar-toggle-directory)
    (define-key map (kbd "g") #'vault-sidebar-refresh)
    (define-key map (kbd "/") #'vault-sidebar-filter)
    (define-key map (kbd "C") #'vault-sidebar-clear-filter)
    (define-key map (kbd "q") #'vault-sidebar-quit)
    map)
  "Keymap for `vault-sidebar-mode'.")

(define-derived-mode vault-sidebar-mode special-mode "Vault"
  "Major mode for vault sidebar.

\\{vault-sidebar-mode-map}"
  (setq-local truncate-lines t)
  (setq-local cursor-type nil)
  (setq-local vault-sidebar--files nil)
  (setq-local vault-sidebar--collapsed-dirs nil)
  (setq-local vault-sidebar--filter "")
  (hl-line-mode 1))

(provide 'vault-sidebar)

;;; vault-sidebar.el ends here
