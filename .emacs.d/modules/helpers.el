;;; helpers.el --- utility functions -*- lexical-binding: t; -*-

(defun reload-init ()
  "Eval init.el to reload config."
  (interactive)
  (load-file "~/.emacs.d/init.el")
  (message "init.el reloaded!"))

(defun bw/restart-emacs ()
  "Save buffers, eval current buffer if .el, restart daemon with new frame."
  (interactive)
  (save-some-buffers)
  (when (and buffer-file-name (string-match "\\.el\\'" buffer-file-name))
    (eval-buffer)
    (message "Evaluated %s" buffer-file-name))
  (condition-case err
      (desktop-save-in-desktop-dir)
    (error (message "Desktop save skipped: %s" (error-message-string err))))
  (call-process-shell-command
   "nohup sh -c 'sleep 0.5 && emacs --daemon && emacsclient -c' >/dev/null 2>&1 &")
  (kill-emacs))

(defun bw/open-cheatsheet ()
  "Open cheatsheet."
  (interactive)
  (find-file "~/vault/org/cheatsheet.org"))

(defun bw/open-palette ()
  "Open palette."
  (interactive)
  (find-file "~/vault/org/palette.org"))

(defun bw/edit-init ()
  "Edit init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun bw/dired-init ()
  "Dired to init.el."
  (interactive)
  (dired-jump nil "~/.emacs.d/init.el"))

(defun bw/duplicate-line ()
  "Duplicate the current line below."
  (interactive)
  (let ((col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (move-to-column col)))

(defun bw/editor ()
  "Open last visited file buffer, or scratch if none."
  (interactive)
  (let ((file-buf (cl-find-if #'buffer-file-name (buffer-list))))
    (if file-buf
        (switch-to-buffer file-buf)
      (switch-to-buffer "*scratch*"))))

(defun bw/email ()
  "Open mu4e email client."
  (interactive)
  (require 'mu4e)
  (mu4e))

(defun bw/yank-file-path ()
  "Copy the current buffer's file path to clipboard."
  (interactive)
  (if buffer-file-name
      (progn
        (kill-new buffer-file-name)
        (message "Copied: %s" buffer-file-name))
    (message "Buffer has no file")))

(defun bw/new-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*new*")))
    (switch-to-buffer buf)
    (setq buffer-offer-save t)))

(defun bw/reload-theme ()
  "Reload the current theme."
  (interactive)
  (load-theme (car custom-enabled-themes) t)
  (message "Reloaded theme: %s" (car custom-enabled-themes)))

(defun bw/search-symbol-at-point ()
  "Search for symbol at point in buffer."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun bw/zen-mode ()
  "Toggle a minimal distraction-free writing mode."
  (interactive)
  (if (bound-and-true-p olivetti-mode)
      (progn
        (olivetti-mode -1)
        (display-line-numbers-mode 1))
    (olivetti-mode 1)
    (display-line-numbers-mode -1)))
