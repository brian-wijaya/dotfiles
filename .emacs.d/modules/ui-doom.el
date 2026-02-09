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

;; Fix mode-line stripes after theme loads
(defun bw/fix-modeline-faces ()
  "Remove box and match modeline bg to default background."
  (let ((bg (face-attribute 'default :background)))
    (set-face-attribute 'mode-line nil :box nil :background bg)
    (set-face-attribute 'mode-line-inactive nil :box nil :background bg)
    (when (facep 'mode-line-active)
      (set-face-attribute 'mode-line-active nil :box nil :background bg))))

(add-hook 'after-init-hook #'bw/fix-modeline-faces)
(add-hook 'doom-load-theme-hook #'bw/fix-modeline-faces 90)

(use-package solaire-mode
  :demand t
  :config
  (solaire-global-mode 1))

(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"
                    :height 140)
(set-face-attribute 'fixed-pitch nil
                    :family "JetBrainsMono Nerd Font")
(set-face-attribute 'variable-pitch nil
                    :family "JetBrainsMono Nerd Font")
