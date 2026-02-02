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
  (setq completion-styles '(hotfuzz basic)))
