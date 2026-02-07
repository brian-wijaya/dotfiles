;;; dirvish.el --- ranger-style file manager -*- lexical-binding: t; -*-

(use-package dirvish
  :demand t
  :config
  ;; Use dirvish globally (replaces dired)
  (dirvish-override-dired-mode 1)

  ;; Layout: parent | current | preview (1:3:5 ratio recommended)
  (setq dirvish-default-layout '(1 0.3 0.5))

  ;; Attributes shown in file list - simplified to reduce visual clutter
  (setq dirvish-attributes
        '(hl-line nerd-icons file-size))

  ;; Preview settings
  (setq dirvish-preview-dispatchers
        '(image gif video audio epub archive pdf fallback))

  ;; Use built-in mode line instead of custom dirvish mode line
  (setq dirvish-mode-line-format nil)
  (setq dirvish-header-line-format nil)

  ;; Quick access paths
  (setq dirvish-quick-access-entries
        '(("h" "~/"                  "Home")
          ("d" "~/Downloads/"        "Downloads")
          ("p" "~/projects/"         "Projects")
          ("v" "~/vault/"            "Vault")
          ("c" "~/.config/"          "Config")
          ("e" "~/.emacs.d/"         "Emacs")))

  ;; Dirvish-mode-map keybindings (file-manager-internal)
  (define-key dirvish-mode-map (kbd "TAB") 'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "q")   'dirvish-quit)
  (define-key dirvish-mode-map (kbd "a")   'dirvish-quick-access)
  (define-key dirvish-mode-map (kbd "f")   'dirvish-file-info-menu)
  (define-key dirvish-mode-map (kbd "y")   'dirvish-yank-menu)
  (define-key dirvish-mode-map (kbd "s")   'dirvish-quicksort)
  (define-key dirvish-mode-map (kbd "F")   'dirvish-layout-toggle)
  (define-key dirvish-mode-map (kbd "/")   'dirvish-narrow)
  (define-key dirvish-mode-map (kbd "h")   'dired-up-directory)
  (define-key dirvish-mode-map (kbd "l")   'dired-find-file)
  ;; History navigation (matches neovim jump list)
  (define-key dirvish-mode-map (kbd "H")   'dired-hist-go-back)
  (define-key dirvish-mode-map (kbd "L")   'dired-hist-go-forward))

;; nerd-icons-dired for file icons (dirvish uses this)
(use-package nerd-icons-dired
  :demand t
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'dirvish)
;;; dirvish.el ends here

;; dired-hist for history navigation
(use-package dired-hist
  :demand t
  :config
  (dired-hist-mode 1))
