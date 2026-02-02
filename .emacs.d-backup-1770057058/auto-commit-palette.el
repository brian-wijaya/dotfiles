;;; auto-commit-palette.el --- Auto-commit palette.org on save -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatically commits palette.org to git every time it's saved.
;; This ensures you never lose ideas when editing.
;;
;; To enable:
;;   (require 'auto-commit-palette)
;;
;; To view history:
;;   cd ~/vault && git log --oneline org/palette.org

;;; Code:

(defun bw/auto-commit-palette ()
  "Auto-commit palette.org to git on save."
  (when (and (buffer-file-name)
             (string-match-p "palette\\.org$" (buffer-file-name)))
    (let ((default-directory (expand-file-name "~/vault/")))
      ;; Stage the file
      (start-process "git-add-palette" nil
                     "git" "add" "org/palette.org")
      ;; Commit with timestamp (after a tiny delay to ensure add completes)
      (run-at-time 0.1 nil
                   (lambda ()
                     (let ((default-directory (expand-file-name "~/vault/")))
                       (start-process "git-commit-palette" nil
                                      "git" "commit" "-m"
                                      (format "auto: palette.org %s"
                                              (format-time-string "%Y-%m-%d %H:%M"))
                                      "--no-verify")))))))

(add-hook 'after-save-hook #'bw/auto-commit-palette)

(provide 'auto-commit-palette)
;;; auto-commit-palette.el ends here
