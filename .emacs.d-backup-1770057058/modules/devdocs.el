;;; devdocs.el --- devdocs integration -*- lexical-binding: t; -*-
(add-hook 'python-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
(add-hook 'python-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
(add-hook 'rust-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("rust"))))
(add-hook 'typescript-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("typescript" "node"))))
(add-hook 'js-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("javascript" "node"))))
(add-hook 'go-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("go"))))
