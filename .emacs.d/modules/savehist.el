;;; savehist.el --- savehist + consult-dir -*- lexical-binding: t; -*-
(use-package savehist
  :demand t
  :ensure nil
  :config
  (setq savehist-file "~/.emacs.d/savehist"
        savehist-save-minibuffer-history t
        history-length 1000
        history-delete-duplicates t
        savehist-additional-variables '(search-ring
                                        regexp-search-ring
                                        extended-command-history))
  (savehist-mode 1))

;; Track recently opened files (needed by consult-recent-file, consult-buffer)
(recentf-mode 1)
(setq recentf-max-saved-items 200)

(use-package consult-dir
  :demand t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))
