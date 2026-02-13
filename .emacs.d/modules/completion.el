;;; completion.el --- vertico, marginalia, orderless, consult, corfu, hotfuzz -*- lexical-binding: t; -*-
(use-package vertico :demand t :config (vertico-mode 1))
(use-package marginalia :demand t :config (marginalia-mode 1))
(use-package orderless :demand t :config (setq completion-styles '(orderless basic)))
(use-package consult :demand t)
(use-package corfu
  :demand t
  :config
  (global-corfu-mode 1)
  (setq corfu-auto t corfu-auto-delay 0.2 corfu-auto-prefix 2))

(use-package hotfuzz
  :demand t
  :config
  ;; Enable fuzzy matching for file completion too
  (setq completion-styles '(hotfuzz basic)
        completion-category-overrides '((file (styles hotfuzz basic)))))

;; M-DEL in file prompts: clear to home directory
(defun bw/file-minibuffer-clear-to-home ()
  "Clear minibuffer and insert ~/."
  (interactive)
  (delete-minibuffer-contents)
  (insert "~/"))

(defun bw/file-minibuffer-bind-keys ()
  "Bind M-DEL in file completion minibuffers (works with vertico)."
  (when (and minibuffer-completion-table
             (eq (completion-metadata-get
                  (completion-metadata "" minibuffer-completion-table
                                      minibuffer-completion-predicate)
                  'category)
                 'file))
    (local-set-key (kbd "M-DEL") #'bw/file-minibuffer-clear-to-home)))

(add-hook 'minibuffer-setup-hook #'bw/file-minibuffer-bind-keys)

;; ═══════════════════════════════════════════════════════════════════
;; Green min-prefix overlays in file completion minibuffer
;; ═══════════════════════════════════════════════════════════════════

(defun bw/minibuffer-green-prefix--propertize ()
  "Apply green min-prefix overlays to completed path components in minibuffer."
  (when (minibufferp)
    (let* ((field-start (minibuffer-prompt-end))
           (contents (buffer-substring-no-properties field-start (point-max)))
           (last-slash (cl-position ?/ contents :from-end t)))
      ;; Only process if there's at least one / (a directory component)
      (when last-slash
        ;; Remove old overlays
        (remove-overlays field-start (point-max) 'bw/green-prefix t)
        ;; Parse the path and apply overlays
        (let ((rebuild "")
              (scan-pos field-start))
          ;; Handle ~/ prefix — color ~ as prefix
          (when (string-prefix-p "~/" contents)
            (let ((ov (make-overlay field-start (+ field-start 1))))
              (overlay-put ov 'face 'breadcrumb-project-leaf-face)
              (overlay-put ov 'bw/green-prefix t)
              (overlay-put ov 'priority 100))
            (setq rebuild "~/")
            (setq scan-pos (+ field-start 2)))
          ;; Handle bare / prefix
          (when (and (not (string-prefix-p "~/" contents))
                     (string-prefix-p "/" contents))
            (setq rebuild "/")
            (setq scan-pos (+ field-start 1)))
          ;; Walk each completed directory component (up to the last /)
          (let ((dir-end (+ field-start last-slash 1))) ; position after last /
            (while (< scan-pos dir-end)
              ;; Find the next /
              (let ((next-slash (or (cl-position ?/ contents
                                                :start (- scan-pos field-start))
                                   last-slash)))
                (when next-slash
                  (let* ((part-start scan-pos)
                         (part-end (+ field-start next-slash))
                         (part (buffer-substring-no-properties part-start part-end)))
                    (when (> (length part) 0)
                      (let* ((parent-dir (expand-file-name rebuild))
                             (entries (when (file-directory-p parent-dir)
                                        (cl-remove-if
                                         (lambda (f) (member f '("." "..")))
                                         (directory-files parent-dir nil nil t))))
                             (min-len (if entries
                                         (bw/min-prefix-length part entries)
                                       1))
                             (prefix-end (+ part-start (min min-len (length part))))
                             (ov (make-overlay part-start prefix-end)))
                        (overlay-put ov 'face 'breadcrumb-project-leaf-face)
                        (overlay-put ov 'bw/green-prefix t)
                        (overlay-put ov 'priority 100)))
                    (setq rebuild (concat rebuild part "/"))
                    ;; Move past part + the /
                    (setq scan-pos (+ part-end 1))))))))))))

(defun bw/minibuffer-green-prefix--setup ()
  "Enable green prefix highlighting when in file completion."
  (when (and minibuffer-completion-table
             (eq (completion-metadata-get
                  (completion-metadata "" minibuffer-completion-table
                                      minibuffer-completion-predicate)
                  'category)
                 'file))
    (add-hook 'post-command-hook
              #'bw/minibuffer-green-prefix--propertize nil t)))

(add-hook 'minibuffer-setup-hook #'bw/minibuffer-green-prefix--setup)
