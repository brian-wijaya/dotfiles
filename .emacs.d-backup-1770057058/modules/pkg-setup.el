;;; pkg-setup.el --- package manager bootstrap -*- lexical-binding: t; -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package gcmh
  :demand t
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 1024 1024 1024))
  (gcmh-mode 1))
