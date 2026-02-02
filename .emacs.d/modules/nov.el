;;; nov.el --- EPUB reader with org-noter integration -*- lexical-binding: t; -*-

(use-package nov
  :demand t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80)  ; Match vsnake column width
  (setq nov-variable-pitch nil))  ; Use fixed-pitch for tech books

(use-package org-noter
  :demand t
  :config
  (setq org-noter-notes-search-path '("~/vault/org/books/notes/")))

;; Evil keybindings for nov-mode
(with-eval-after-load 'nov
  (with-eval-after-load 'evil
    (evil-define-key 'normal nov-mode-map
      (kbd "n") 'nov-next-document
      (kbd "p") 'nov-previous-document
      (kbd "g g") 'nov-goto-toc
      (kbd "RET") 'nov-browse-url
      (kbd "j") 'evil-next-line
      (kbd "k") 'evil-previous-line
      (kbd "SPC") 'evil-scroll-page-down
      (kbd "S-SPC") 'evil-scroll-page-up)))

(provide 'nov)
;;; nov.el ends here
