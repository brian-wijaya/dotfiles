;;; imenu.el --- imenu-list sidebar -*- lexical-binding: t; -*-

(use-package imenu-list
  :demand t
  :config
  (setq imenu-list-position 'left
        imenu-list-size 0.2
        imenu-list-focus-after-activation t))
