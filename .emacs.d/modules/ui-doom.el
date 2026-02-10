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
  (setq doom-modeline-height 25)

  ;; ═══════════════════════════════════════════════════════════════
  ;; Custom path segment with green min-prefix + smart truncation
  ;; ═══════════════════════════════════════════════════════════════

  (defun bw/modeline--build-components (filepath)
    "Build list of (DIR . MIN-PREFIX-LEN) for FILEPATH, plus filename.
Returns (:dirs ((name . prefix-len) ...) :filename name :tilde t/nil)."
    (let* ((abbreviated (abbreviate-file-name filepath))
           (parts (split-string abbreviated "/" t))
           (filename (car (last parts)))
           (dirs (butlast parts))
           (has-tilde (string-prefix-p "~/" abbreviated))
           (rebuild (if has-tilde "~/" "/"))
           (result '()))
      (when (and dirs has-tilde (string= (car dirs) "~"))
        (setq dirs (cdr dirs)))
      (dolist (dir dirs)
        (let* ((parent (expand-file-name rebuild))
               (entries (when (file-directory-p parent)
                          (cl-remove-if
                           (lambda (f) (member f '("." "..")))
                           (directory-files parent nil nil t))))
               (min-len (if entries
                            (bw/min-prefix-length dir entries)
                          1)))
          (push (cons dir min-len) result)
          (setq rebuild (concat rebuild dir "/"))))
      (list :dirs (nreverse result) :filename filename :tilde has-tilde)))

  (defun bw/modeline--render-component (name prefix-len face-prefix face-rest)
    "Render directory NAME with PREFIX-LEN green chars, rest in another face."
    (let ((plen (min prefix-len (length name))))
      (concat
       (propertize (substring name 0 plen) 'face face-prefix)
       (propertize (substring name plen) 'face face-rest))))

  (defun bw/modeline--render-truncated (name prefix-len face-prefix face-rest)
    "Render NAME truncated: keep prefix chars in green, then '..'."
    (let ((plen (min prefix-len (length name))))
      (concat
       (propertize (substring name 0 plen) 'face face-prefix)
       (propertize ".." 'face face-rest))))

  (defun bw/modeline--path-string (components max-width face-prefix face-rest face-file face-sep)
    "Render path from COMPONENTS, truncating middle-out if exceeds MAX-WIDTH.
FACE-PREFIX for green prefix chars, FACE-REST for non-prefix, FACE-FILE for filename."
    (let* ((dirs (plist-get components :dirs))
           (filename (plist-get components :filename))
           (tilde (plist-get components :tilde))
           (ndirs (length dirs))
           (sep (propertize "/" 'face face-sep))
           ;; Build full-length rendered parts
           (dir-parts (mapcar (lambda (d)
                                (bw/modeline--render-component
                                 (car d) (cdr d) face-prefix face-rest))
                              dirs))
           (file-part (propertize filename 'face face-file))
           (tilde-part (when tilde (propertize "~" 'face face-prefix)))
           ;; Calculate full length
           (full-str (concat (when tilde-part (concat tilde-part sep))
                             (mapconcat #'identity dir-parts sep)
                             (when dirs sep)
                             file-part))
           (full-len (length full-str)))
      (if (<= full-len max-width)
          full-str
        ;; Need to truncate: middle-out strategy
        ;; Build a mutable vector of dir states: :full, :prefix-dots, :dots
        (let* ((states (make-vector ndirs :full))
               (trunc-parts (copy-sequence (vconcat dir-parts))))
          ;; Generate truncation order: middle out
          (let ((order '()))
            (if (= ndirs 0)
                nil
              (let ((mid (/ (1- ndirs) 2)))
                ;; Alternating from middle outward, skip first dir (index 0)
                (cl-loop for offset from 0
                         for left = (- mid offset)
                         for right = (+ mid offset 1)
                         while (or (>= left 0) (< right ndirs))
                         do (progn
                              (when (and (>= left 1) (<= left (1- ndirs)))
                                (push left order))
                              (when (and (>= right 1) (< right ndirs))
                                (push right order))))
                (setq order (nreverse order))))
            ;; Iteratively truncate components until it fits
            (catch 'done
              (dolist (idx order)
                (let ((d (nth idx dirs)))
                  ;; Phase 1: truncate to prefix..
                  (aset trunc-parts idx
                        (bw/modeline--render-truncated
                         (car d) (cdr d) face-prefix face-rest))
                  (aset states idx :prefix-dots)
                  (let ((str (concat (when tilde-part (concat tilde-part sep))
                                     (mapconcat #'identity
                                                (append (cl-coerce trunc-parts 'list) nil)
                                                sep)
                                     (when dirs sep)
                                     file-part)))
                    (when (<= (length str) max-width)
                      (throw 'done str))))
                ;; Phase 2: collapse to ..
                (let ((d (nth idx dirs)))
                  (aset trunc-parts idx (propertize ".." 'face face-rest))
                  (aset states idx :dots)
                  (let ((str (concat (when tilde-part (concat tilde-part sep))
                                     (mapconcat #'identity
                                                (append (cl-coerce trunc-parts 'list) nil)
                                                sep)
                                     (when dirs sep)
                                     file-part)))
                    (when (<= (length str) max-width)
                      (throw 'done str)))))
              ;; If still too long after all truncation, return what we have
              (concat (when tilde-part (concat tilde-part sep))
                      (mapconcat #'identity
                                 (append (cl-coerce trunc-parts 'list) nil)
                                 sep)
                      (when dirs sep)
                      file-part)))))))

  ;; Cache per buffer
  (defvar-local bw/modeline--cached-path nil)
  (defvar-local bw/modeline--cached-file nil)
  (defvar-local bw/modeline--cached-width nil)
  (defvar-local bw/modeline--cached-active nil)

  (doom-modeline-def-segment bw-buffer-path
    "File path with green min-prefix highlighting and smart truncation."
    (let* ((active (doom-modeline--active))
           (face-file (if active 'doom-modeline-buffer-file
                        '(:inherit (doom-modeline-buffer-file mode-line-inactive)))))
      (if buffer-file-name
          (let* ((width (max 30 (- (window-width) 45)))
                 (face-prefix (if active 'doom-modeline-info
                                '(:inherit (doom-modeline-info mode-line-inactive))))
                 (face-rest (if active 'doom-modeline-buffer-path
                              '(:inherit (doom-modeline-buffer-path mode-line-inactive))))
                 (face-sep (if active 'doom-modeline-buffer-path
                             '(:inherit (doom-modeline-buffer-path mode-line-inactive)))))
            ;; Cache: recompute when file, width, or active state changes
            (when (or (not bw/modeline--cached-path)
                      (not (equal bw/modeline--cached-file buffer-file-name))
                      (not (= bw/modeline--cached-width width))
                      (not (eq bw/modeline--cached-active active)))
              (let ((components (bw/modeline--build-components buffer-file-name)))
                (setq bw/modeline--cached-file buffer-file-name
                      bw/modeline--cached-width width
                      bw/modeline--cached-active active
                      bw/modeline--cached-path
                      (bw/modeline--path-string
                       components width face-prefix face-rest face-file face-sep))))
            (concat " " bw/modeline--cached-path " "))
        ;; Non-file buffers: show buffer name
        (concat " " (propertize (buffer-name) 'face face-file) " "))))

  ;; Redefine main modeline: replace buffer-info with bw-buffer-path,
  ;; move buffer-position to the right side
  (doom-modeline-def-modeline 'main
    '(eldoc bar window-state modals matches follow bw-buffer-path remote-host word-count parrot selection-info)
    '(compilation objed-state misc-info buffer-position project-name persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check time)))

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
