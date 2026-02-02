;;; vsnake.el --- newspaper column view -*- lexical-binding: t; -*-

;;; Commentary:
;; Newspaper-style column viewing mode using follow-mode as the scroll-sync engine.
;; Entry point: `bw/vsnake-toggle' (bound to SPC w V in bindings-doom-full.el).

;;; Code:

(require 'follow)

(defvar bw/vsnake-column-width 80
  "Target column width for vsnake columns.")

(defvar-local bw/vsnake--saved-config nil
  "Window configuration saved before entering vsnake mode.")

(defvar-local bw/vsnake--source-buffer nil
  "The buffer being viewed in vsnake mode.")

(define-minor-mode bw/vsnake-reading-mode
  "Read-only navigation mode for vsnake column view."
  :lighter " vsnake"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Exit
            (define-key map (kbd "q") #'bw/vsnake-exit)
            (define-key map (kbd "Q") #'bw/vsnake-exit)
            ;; Scroll (follow-mode syncs)
            (define-key map (kbd "j") #'evil-next-line)
            (define-key map (kbd "k") #'evil-previous-line)
            (define-key map (kbd "n") #'evil-next-line)
            (define-key map (kbd "p") #'evil-previous-line)
            (define-key map (kbd "<down>") #'evil-next-line)
            (define-key map (kbd "<up>") #'evil-previous-line)
            (define-key map (kbd "<next>") #'evil-scroll-page-down)
            (define-key map (kbd "<prior>") #'evil-scroll-page-up)
            (define-key map (kbd "SPC") #'evil-scroll-page-down)
            ;; Column cursor jump
            (define-key map (kbd "C-<right>") #'windmove-right)
            (define-key map (kbd "C-<left>") #'windmove-left)
            ;; Horizontal shift
            (define-key map (kbd "S-<right>") #'scroll-left)
            (define-key map (kbd "S-<left>") #'scroll-right)
            (define-key map (kbd "S-<next>") (lambda () (interactive) (scroll-left 20)))
            (define-key map (kbd "S-<prior>") (lambda () (interactive) (scroll-right 20)))
            ;; Top/bottom
            (define-key map (kbd "g") nil)
            (define-key map (kbd "g g") #'evil-goto-first-line)
            (define-key map (kbd "G") #'evil-goto-line)
            ;; Search
            (define-key map (kbd "C-s") #'isearch-forward)
            (define-key map (kbd "C-r") #'isearch-backward)
            ;; Prevent editing
            (define-key map [remap self-insert-command] #'ignore)
            map))

(defun bw/vsnake-exit ()
  "Exit vsnake mode and restore previous window configuration."
  (interactive)
  (bw/vsnake-reading-mode -1)
  (follow-mode -1)
  (when bw/vsnake--saved-config
    (set-window-configuration bw/vsnake--saved-config)
    (setq bw/vsnake--saved-config nil)))

(defun bw/vsnake-toggle ()
  "Toggle newspaper-style column reading mode for current buffer.
Splits the frame into side-by-side columns showing the same buffer
with `follow-mode' for synchronized scrolling."
  (interactive)
  ;; If already active, exit
  (if (bound-and-true-p bw/vsnake-reading-mode)
      (bw/vsnake-exit)
  (let ((buf (current-buffer))
        (config (current-window-configuration)))
    ;; Save state
    (setq bw/vsnake--saved-config config)
    (setq bw/vsnake--source-buffer buf)
    ;; Setup columns — force-delete all other windows including side windows
    (dolist (win (window-list))
      (set-window-dedicated-p win nil)
      (set-window-parameter win 'no-delete-other-windows nil))
    (delete-other-windows)
    (let* ((frame-cols (window-total-width))
           (num-cols (max 1 (floor (/ (float frame-cols) bw/vsnake-column-width)))))
      ;; Split into N columns
      (dotimes (_ (1- num-cols))
        (split-window-right bw/vsnake-column-width))
      ;; All windows show same buffer (skip minibuffer/dedicated)
      (dolist (win (window-list))
        (unless (or (window-minibuffer-p win) (window-dedicated-p win))
          (set-window-buffer win buf)))
      ;; Balance column widths
      (balance-windows))
    ;; Enable follow-mode
    (follow-mode 1)
    ;; Reading comfort
    (setq-local truncate-lines t)
    ;; Activate minor mode
    (bw/vsnake-reading-mode 1)
    (message "vsnake: %d columns — q to exit"
             (length (cl-remove-if
                      (lambda (w) (or (window-minibuffer-p w)
                                      (window-dedicated-p w)))
                      (window-list)))))))

;; Alias for the binding in bindings-doom-full.el
(defalias 'bw/vsnake-mode #'bw/vsnake-toggle)

(provide 'vsnake)
;;; vsnake.el ends here
