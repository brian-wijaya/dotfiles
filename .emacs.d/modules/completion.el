;;; completion.el --- vertico, marginalia, orderless, consult, corfu, hotfuzz -*- lexical-binding: t; -*-
(use-package vertico :demand t :config (vertico-mode 1))
(use-package marginalia :demand t :config (marginalia-mode 1))
(use-package orderless :demand t :config (setq completion-styles '(orderless basic)))
(use-package consult :demand t)
(use-package corfu
  :demand t
  :config
  (global-corfu-mode 1)
  (setq corfu-auto t corfu-auto-delay 0.2 corfu-auto-prefix 2))

(use-package hotfuzz
  :demand t
  :config
  ;; Enable fuzzy matching for file completion too
  (setq completion-styles '(hotfuzz basic)
        completion-category-overrides '((file (styles hotfuzz basic)))))

;; M-DEL in file prompts: clear to home directory
(define-key minibuffer-local-filename-completion-map (kbd "M-DEL")
  (lambda () (interactive)
    (delete-minibuffer-contents)
    (insert "~/")))
