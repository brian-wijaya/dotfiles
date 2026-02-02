;;; imenu.el --- imenu-list sidebar -*- lexical-binding: t; -*-

(use-package imenu-list
  :demand t
  :config
  (setq imenu-list-position 'left
        imenu-list-size 0.12  ; Reduced from 0.2 to minimize horizontal waste
        imenu-list-focus-after-activation t))
