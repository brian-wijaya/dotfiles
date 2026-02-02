;;; hl-todo.el --- highlight TODO keywords -*- lexical-binding: t; -*-
(use-package hl-todo
  :demand t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("HACK"   . "#FFA500")
          ("NOTE"   . "#1E90FF")
          ("REVIEW" . "#1E90FF"))))
