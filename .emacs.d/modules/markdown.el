;;; markdown.el --- Markdown editing enhancements -*- lexical-binding: t; -*-

;; Option C: outline-minor-mode with custom regex for ASCII tree folding
;; Matches markdown headings AND tree structure characters (├ └ │)
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq-local outline-regexp
                        "^#+ \\|^[[:space:]]*[├└]")
            (outline-minor-mode 1)))

;; Option B: selective-display is built into Emacs (C-x $ N).
;; Add convenient Evil-friendly binding: SPC t f (toggle fold level)
(defun bw/set-selective-display-interactive ()
  "Prompt for column and hide lines indented beyond it. 0 to show all."
  (interactive)
  (let ((col (read-number "Hide lines indented beyond column (0=show all): " 0)))
    (set-selective-display (if (zerop col) nil col))))
