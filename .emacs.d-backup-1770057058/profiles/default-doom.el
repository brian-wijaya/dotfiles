;;; default-doom.el --- Doom defaults profile -*- lexical-binding: t; -*-
;;
;; Profile: default-doom
;; Evil + SPC, doom-one theme, doom-modeline, basic completion.
;; Uses evil-define-key directly (no prefix keymap structure).

(let ((mod (expand-file-name "modules/" user-emacs-directory)))
  (load (expand-file-name "pkg-setup" mod))
  (load (expand-file-name "evil-core" mod))
  (load (expand-file-name "ui-minimal" mod))
  (load (expand-file-name "ui-doom" mod))
  (load (expand-file-name "completion" mod))
  (load (expand-file-name "git" mod))
  (load (expand-file-name "bindings-doom-basic" mod)))

;; which-key (basic config, inline)
(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3))
