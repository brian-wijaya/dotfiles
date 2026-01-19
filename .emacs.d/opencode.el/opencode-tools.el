                  (format "Repo map generated successfully in buffer %s" buffer-name))
              (error "Failed to generate repo map (exit code %d): %s"
                     exit-code (buffer-string)))))))))

(defun opencode-edit-file (file-path old-string new-string &optional replace-all)
  "Replace OLD-STRING with NEW-STRING inside FILE-PATH.
When REPLACE-ALL is truthy, every occurrence is replaced."
  (unless (opencode--edit-permission-allowed-p)
    (error "File edits denied by `opencode-edit-permissions'"))
  (let* ((expanded-path (expand-file-name file-path))
         (replace-all (opencode--truthy replace-all)))
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))
    (find-file-noselect expanded-path)
    (with-temp-buffer
      (insert-file-contents expanded-path)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward old-string nil t)
            (setq count (1+ count)))
          (cond
           ((= count 0)
            (error "Could not find text to replace in %s" file-path))
           ((and (> count 1) (not replace-all))
            (error "Found %d matches. Use replaceAll=true or give more context." count))
           (t
            (goto-char (point-min))
            (if replace-all
                (while (search-forward old-string nil t)
                  (replace-match new-string t t))
              (search-forward old-string)
              (replace-match new-string t t))
            (let ((inhibit-message t))
              (write-region (point-min) (point-max) expanded-path nil 'no-message))
            (let* ((msg (format "Successfully edited %s (%d replacement%s)"
                                file-path count (if (= count 1) "" "s")))
                   (diag (opencode--format-diagnostics
                          (opencode-treesit-get-diagnostics expanded-path))))
              (opencode--append-diagnostics msg diag)))))))))

;; Todo system implementation
(defvar opencode--todo-list nil
  "Current todo list for the session.")
