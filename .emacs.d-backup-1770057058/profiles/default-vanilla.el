;;; default-vanilla.el --- Stock Emacs profile -*- lexical-binding: t; -*-
;;
;; Profile: default-vanilla
;; No evil, no custom packages, gray theme, stock UI.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Stock UI (all chrome visible)
(menu-bar-mode 1)
(tool-bar-mode 1)
(scroll-bar-mode 1)
(blink-cursor-mode 1)

;; Basic settings
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(fido-mode -1)
(icomplete-mode -1)

;; which-key to help discover keys
(when (require 'which-key nil t)
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5))
