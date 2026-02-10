;;; org.el --- org-mode config -*- lexical-binding: t; -*-
(use-package org-visibility
  :demand t
  :after org
  :hook (org-mode . org-visibility-mode))

(setq org-agenda-files '("~/vault/org/"))
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/vault/org/todo.org")
         "* TODO %?\n  %U")))

;; CDLaTeX in org-mode for fast math input
(add-hook 'org-mode-hook #'org-cdlatex-mode)

;; LaTeX preview configuration — prefer SVG for crisp scaling
(setq org-preview-latex-default-process 'dvisvgm)
