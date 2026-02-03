;;; ui-doom.el --- Modern dark theme, modeline, solaire, font -*- lexical-binding: t; -*-
(use-package doom-themes
  :demand t
  :config
  ;; Use doom-tokyo-night to match ghostty/nvim theme
  (load-theme 'doom-tokyo-night t)
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
                    :height 100)
(set-face-attribute 'fixed-pitch nil
                    :family "JetBrainsMono Nerd Font")
(set-face-attribute 'variable-pitch nil
                    :family "JetBrainsMono Nerd Font")
