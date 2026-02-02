;;; ibuffer.el --- ibuffer config -*- lexical-binding: t; -*-
(require 'ibuffer)
(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))
