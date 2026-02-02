;;; evil-collection.el --- evil-collection + vterm bindings -*- lexical-binding: t; -*-
(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init)
  (define-key evil-normal-state-map (kbd "C-y") 'yank)
  (define-key evil-visual-state-map (kbd "C-y") 'yank)
  (define-key evil-insert-state-map (kbd "C-y") 'yank))

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-l") 'vterm-clear)
  (evil-define-key 'normal vterm-mode-map (kbd "g g") 'evil-goto-first-line)
  (evil-define-key 'normal vterm-mode-map (kbd "G") 'evil-goto-line))
