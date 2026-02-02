;;; eglot.el --- LSP via eglot -*- lexical-binding: t; -*-
(require 'eglot)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)

;; Spec 10: compose all doc sources eagerly
(setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
