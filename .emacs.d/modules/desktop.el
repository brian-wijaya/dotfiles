;;; desktop.el --- desktop save mode -*- lexical-binding: t; -*-
(setq desktop-dirname "~/.emacs.d/"
      desktop-base-file-name ".emacs.desktop"
      desktop-save t
      desktop-load-locked-desktop t
      desktop-restore-eager 10
      desktop-auto-save-timeout 60)
(desktop-save-mode 1)

;;; Restore focus to the buffer that was current when Emacs quit.
;;; desktop-save-mode restores buffers but does not preserve which one
;;; had focus.  We save that information on exit and restore it after
;;; the desktop finishes loading.

(defvar bw/desktop-last-buffer-file
  (expand-file-name ".last-buffer" user-emacs-directory)
  "File that records the path of the buffer current at exit.")

(defun bw/desktop-save-last-buffer ()
  "Write the current buffer's file path to `bw/desktop-last-buffer-file'."
  (when-let ((path (buffer-file-name (current-buffer))))
    (with-temp-file bw/desktop-last-buffer-file
      (insert path))))

(defun bw/desktop-restore-last-buffer ()
  "Switch to the buffer recorded in `bw/desktop-last-buffer-file'.
Uses an idle timer so that desktop lazy-loading has a chance to
create the buffer first.  Called from `window-setup-hook' which
fires after the frame is fully configured."
  (when (file-exists-p bw/desktop-last-buffer-file)
    (let ((path (string-trim
                 (with-temp-buffer
                   (insert-file-contents bw/desktop-last-buffer-file)
                   (buffer-string)))))
      (when (and (not (string-empty-p path))
                 (file-exists-p path))
        (run-with-idle-timer
         0.5 nil
         (lambda ()
           (let ((buf (find-buffer-visiting path)))
             (when (not buf)
               ;; Force desktop to restore this buffer if it's in the
               ;; desktop file but hasn't been lazily loaded yet.
               (setq buf (find-file-noselect path)))
             (when buf
               (switch-to-buffer buf)
               (select-window (get-buffer-window buf))))))))))

(add-hook 'kill-emacs-hook #'bw/desktop-save-last-buffer)
(add-hook 'window-setup-hook #'bw/desktop-restore-last-buffer)
