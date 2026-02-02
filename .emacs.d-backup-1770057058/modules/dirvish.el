;;; dirvish.el --- ranger-style file manager -*- lexical-binding: t; -*-

(use-package dirvish
  :demand t
  :config
  ;; Use dirvish globally (replaces dired)
  (dirvish-override-dired-mode 1)

  ;; Layout: parent | current | preview
  (setq dirvish-default-layout '(0 0.4 0.6))

  ;; Attributes shown in file list
  (setq dirvish-attributes
        '(hl-line subtree-state nerd-icons collapse git-msg file-time file-size))

  ;; Preview settings
  (setq dirvish-preview-dispatchers
        '(image gif video audio epub archive pdf))

  ;; Header line shows path breadcrumb
  (setq dirvish-header-line-format
        '(:left (path) :right (free-space)))

  ;; Mode line
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))

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
  (define-key dirvish-mode-map (kbd "l")   'dired-find-file))

;; nerd-icons-dired for file icons (dirvish uses this)
(use-package nerd-icons-dired
  :demand t
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'dirvish)
;;; dirvish.el ends here
