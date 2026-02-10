;;; yasnippet.el --- snippet expansion with auto-expanding LaTeX support -*- lexical-binding: t; -*-

(use-package yasnippet
  :demand t
  :config
  (setq yas-triggers-in-field t
        yas-wrap-around-region t
        yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-global-mode 1)

  ;; Suffix-matching functions for short triggers embedded in words.
  ;; Without these, typing "xsr" won't find the "sr" snippet because
  ;; YASnippet sees "xsr" as one word.  These try 2-char and 3-char
  ;; suffixes explicitly.  From karthinks' config.
  (defun bw/yas-try-2-suffix (_start-point)
    "Try to expand a 2-character snippet trigger at point."
    (when (>= (point) (+ (point-min) 2))
      (backward-char 2)
      nil))

  (defun bw/yas-try-3-suffix (_start-point)
    "Try to expand a 3-character snippet trigger at point."
    (when (>= (point) (+ (point-min) 3))
      (backward-char 3)
      nil))

  (add-to-list 'yas-key-syntaxes #'bw/yas-try-2-suffix)
  (add-to-list 'yas-key-syntaxes #'bw/yas-try-3-suffix)

  ;; Suppress backquote-change warnings from snippets with embedded elisp
  (require 'warnings)
  (cl-pushnew '(yasnippet backquote-change) warning-suppress-types :test 'equal)

  ;; Auto-expanding snippets: fire on every keystroke for snippets with
  ;; condition containing 'auto.  Core mechanism from karthinks' approach.
  ;; The double quoting is intentional — it produces a quoted cons pair.
  (defun bw/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))
  (add-hook 'post-self-insert-hook #'bw/yas-try-expanding-auto-snippets))
