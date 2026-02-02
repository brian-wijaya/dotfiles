;;; keyhints.el --- discoverable prefix keys in side window -*- lexical-binding: t; -*-
(defvar bw/keyhints-buffer nil "Buffer for keyhints content.")

(defun bw/discover-prefixes ()
  "Discover available prefix keys for current context."
  (let* ((bindings (which-key--get-current-bindings))
         (prefixes '())
         (has-bracket-open nil)
         (has-bracket-close nil)
         (has-z nil))
    (when (and (bound-and-true-p evil-mode)
               (keymapp (lookup-key evil-normal-state-map (kbd "SPC"))))
      (push '("SPC" . "👑leader") prefixes))
    (dolist (b bindings)
      (let ((key (car b))
            (desc (cdr b)))
        (when (or (string= desc "prefix")
                  (string-suffix-p "-prefix" desc)
                  (string-suffix-p "-command" desc))
          (cond
           ((string= key "C-x") (push '("C-x" . "⚙️emacs") prefixes))
           ((string= key "C-c") (push '("C-c" . "🎛️mode") prefixes))
           ((string= key "C-h") (push '("C-h" . "❓help") prefixes))
           ((string= key "M-x") (push '("M-x" . "🚀cmd") prefixes))
           ((string= key "M-g") (push '("M-g" . "🎯goto") prefixes))
           ((string= key "M-s") (push '("M-s" . "🔍search") prefixes))
           ((string= key "g") (push '("g" . "🏃go") prefixes))
           ((string= key "z") (setq has-z t))
           ((string= key "[") (setq has-bracket-open t))
           ((string= key "]") (setq has-bracket-close t))))))
    (let ((result (nreverse prefixes)))
      (when has-bracket-open (setq result (append result '(("[" . "⬅️")))))
      (when has-bracket-close (setq result (append result '(("]" . "➡️")))))
      (when has-z (setq result (append result '(("z" . "📁fold")))))
      result)))

(defun bw/format-keyhints ()
  "Format discovered prefixes as a string."
  (mapconcat (lambda (p) (format "%s:%s" (car p) (cdr p)))
             (bw/discover-prefixes) " "))

(defun bw/keyhints-create ()
  "Create the keyhints side window (above echo area)."
  (setq bw/keyhints-buffer (get-buffer-create "*keyhints*"))
  (with-current-buffer bw/keyhints-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (bw/format-keyhints)
                         'face '(:foreground "#7c828d"))))
    (setq mode-line-format nil
          header-line-format nil
          cursor-type nil
          buffer-read-only t))
  (let ((win (display-buffer-in-side-window
              bw/keyhints-buffer
              '((side . bottom)
                (slot . 0)
                (window-height . 1)
                (window-parameters . ((no-delete-other-windows . t)
                                     (mode-line-format . none)))))))
    (when win
      (set-window-dedicated-p win t))))

(defun bw/keyhints-update ()
  "Update keyhints content based on current context."
  (when (buffer-live-p bw/keyhints-buffer)
    (with-current-buffer bw/keyhints-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (bw/format-keyhints)
                           'face '(:foreground "#7c828d")))))))

;; Initialize keyhints after which-key loads
(with-eval-after-load 'which-key
  (if after-init-time
      (bw/keyhints-create)
    (add-hook 'emacs-startup-hook #'bw/keyhints-create)))

;; Spec 06 fix: belt-and-suspenders ensure keyhints show
(add-hook 'after-init-hook
  (lambda () (run-with-idle-timer 1 nil #'bw/keyhints-create)))

;; Update on relevant events
(add-hook 'buffer-list-update-hook #'bw/keyhints-update)
(add-hook 'window-configuration-change-hook #'bw/keyhints-update)
(with-eval-after-load 'evil
  (add-hook 'evil-normal-state-entry-hook #'bw/keyhints-update)
  (add-hook 'evil-insert-state-entry-hook #'bw/keyhints-update)
  (add-hook 'evil-visual-state-entry-hook #'bw/keyhints-update))

;; Mode-specific hooks for robustness
(with-eval-after-load 'magit
  (add-hook 'magit-mode-hook #'bw/keyhints-update))
(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook #'bw/keyhints-update))
(add-hook 'dired-mode-hook #'bw/keyhints-update)

;; Force update after which-key cache rebuild
(with-eval-after-load 'which-key
  (advice-add 'which-key--update :after #'bw/keyhints-update))

;; Handle transient keymaps (magit popups, etc.)
(with-eval-after-load 'transient
  (add-hook 'transient-exit-hook #'bw/keyhints-update))
