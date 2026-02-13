;;; mermaid.el --- Mermaid diagram support -*- lexical-binding: t; -*-

(use-package mermaid-mode
  :mode "\\.mmd\\'"
  :config
  (setq mermaid-mmdc-location (expand-file-name "~/.local/bin/mmdc"))
  (setq mermaid-output-format ".svg"))

(defun bw/mermaid-render ()
  "Render current Mermaid file to SVG and display it."
  (interactive)
  (let* ((in (buffer-file-name))
         (out (concat (file-name-sans-extension in) ".svg")))
    (save-buffer)
    (if (zerop (call-process (expand-file-name "~/.local/bin/mmdc")
                             nil "*mermaid-output*" nil
                             "-i" in "-o" out))
        (find-file-other-window out)
      (pop-to-buffer "*mermaid-output*"))))

(with-eval-after-load 'mermaid-mode
  (define-key mermaid-mode-map (kbd "C-c C-c") #'bw/mermaid-render))
