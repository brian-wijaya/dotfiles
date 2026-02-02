;;; vault-rag.el --- Vault-RAG resource control -*- lexical-binding: t; -*-
;;
;; Control vault-rag indexing resources from Emacs

(defun vault-rag-index ()
  "Force aggressive vault-rag indexing mode (32GB RAM, 100% CPU).
Use when stepping away and want indexing to power through backlogs."
  (interactive)
  (let ((result (shell-command-to-string "vault-rag index")))
    (message "%s" (string-trim result))))

(defun vault-rag-auto ()
  "Return vault-rag to automatic mode (6-hour idle threshold).
Restores conservative limits and re-enables automatic switching."
  (interactive)
  (let ((result (shell-command-to-string "vault-rag auto")))
    (message "%s" (string-trim result))))

(defun vault-rag-status ()
  "Show current vault-rag mode and resource limits."
  (interactive)
  (let ((status (shell-command-to-string "vault-rag status")))
    (with-current-buffer (get-buffer-create "*Vault-RAG Status*")
      (erase-buffer)
      (insert status)
      (special-mode)
      (display-buffer (current-buffer)))))

;; Keybindings (SPC v prefix for vault operations)
(with-eval-after-load 'evil-leader
  (define-key bw/leader-v-map (kbd "i") 'vault-rag-index)
  (define-key bw/leader-v-map (kbd "a") 'vault-rag-auto)
  (define-key bw/leader-v-map (kbd "s") 'vault-rag-status))

;; Which-key descriptions
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements bw/leader-map
    "v" '("vault" . "Vault operations"))
  (which-key-add-keymap-based-replacements bw/leader-v-map
    "i" '("index" . "Force aggressive indexing")
    "a" '("auto" . "Return to automatic mode")
    "s" '("status" . "Show resource status")))

(provide 'vault-rag)
;;; vault-rag.el ends here
