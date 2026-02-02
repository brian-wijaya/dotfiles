;;; ui-doom.el --- doom theme, modeline, solaire, font -*- lexical-binding: t; -*-
(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package doom-modeline
  :demand t
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-height 25))

(use-package solaire-mode
  :demand t
  :config
  (solaire-global-mode 1))

(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"
                    :height 110)
(set-face-attribute 'fixed-pitch nil
                    :family "JetBrainsMono Nerd Font")
(set-face-attribute 'variable-pitch nil
                    :family "JetBrainsMono Nerd Font")
