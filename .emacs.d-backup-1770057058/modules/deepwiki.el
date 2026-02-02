;;; deepwiki.el --- DeepWiki code intelligence -*- lexical-binding: t; -*-

(defvar bw/deepwiki-api-url "http://localhost:8001"
  "URL of the self-hosted DeepWiki API.")

(defvar bw/deepwiki-frontend-url "http://localhost:3000"
  "URL of the DeepWiki frontend.")

;; --- Detect current repo ---

(defun bw/deepwiki--current-repo ()
  "Detect the current repository name from project.el or magit.
Returns the directory basename of the project root."
  (let ((root (cond
               ((and (fboundp 'magit-toplevel) (magit-toplevel))
                (magit-toplevel))
               ((project-current)
                (project-root (project-current)))
               (t nil))))
    (when root
      (file-name-nondirectory (directory-file-name root)))))

;; --- Render wiki pages ---

(defun bw/deepwiki--render-pages (wiki-data)
  "Render WIKI-DATA pages as markdown in current buffer."
  (let ((pages (plist-get wiki-data :generated_pages)))
    (insert "# DeepWiki\n\n")
    (when pages
      (maphash
       (lambda (_page-id page)
         (let ((title (plist-get page :title))
               (content (plist-get page :content)))
           (when title
             (insert (format "## %s\n\n" title)))
           (when content
             (insert content "\n\n"))))
       pages))))

;; --- Mermaid diagram rendering ---

(defun bw/deepwiki--render-mermaid-blocks ()
  "Find mermaid code blocks in current buffer and render as inline SVG.
Requires mermaid-cli: npm install -g @mermaid-js/mermaid-cli"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "```mermaid\n\\(\\(?:.*\n\\)*?\\)```" nil t)
      (let* ((mermaid-src (match-string 1))
             (beg (match-beginning 0))
             (end (match-end 0))
             (tmp-in (make-temp-file "mermaid" nil ".mmd"))
             (tmp-out (make-temp-file "mermaid" nil ".svg")))
        (with-temp-file tmp-in (insert mermaid-src))
        (when (= 0 (call-process "mmdc" nil nil nil
                                  "-i" tmp-in "-o" tmp-out
                                  "-t" "dark" "-b" "transparent"))
          (let ((inhibit-read-only t))
            (goto-char beg)
            (delete-region beg end)
            (insert-image (create-image tmp-out 'svg nil
                                        :max-width 800
                                        :max-height 600))
            (insert "\n")))
        (delete-file tmp-in)))))

;; --- View cached wiki in Emacs buffer ---

(defun bw/deepwiki-view ()
  "View the DeepWiki for the current project (from cache).
Opens a read-only *deepwiki: {repo}* buffer in markdown-mode."
  (interactive)
  (let* ((name (or (bw/deepwiki--current-repo)
                   (user-error "Not in a project")))
         (url (format "%s/api/wiki_cache?owner=local&repo=%s&repo_type=local&language=en"
                      bw/deepwiki-api-url name))
         (buf-name (format "*deepwiki: %s*" name)))
    (url-retrieve url
      (lambda (_status)
        (goto-char url-http-end-of-headers)
        (let* ((json (ignore-errors (json-parse-buffer :object-type 'plist)))
               (buf (get-buffer-create buf-name)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (if (null json)
                  (insert "No wiki cached for this project.\n\n"
                          "Generate one:\n"
                          "  SPC c W  — open DeepWiki frontend in browser\n"
                          "  Or ask Claude Code: \"generate a deepwiki for this repo\"\n")
                (bw/deepwiki--render-pages json)
                (bw/deepwiki--render-mermaid-blocks))
              (goto-char (point-min)))
            (markdown-mode)
            (read-only-mode 1)
            ;; Interactive keybindings in read-only buffer
            (local-set-key (kbd "q") #'quit-window)
            (local-set-key (kbd "n") #'markdown-outline-next)
            (local-set-key (kbd "p") #'markdown-outline-previous)
            (local-set-key (kbd "TAB") #'markdown-cycle)
            (local-set-key (kbd "g r") #'bw/deepwiki-view))
          (display-buffer buf)))
      nil t)))

;; --- Ask a question via RAG ---

(defun bw/deepwiki-ask (question)
  "Ask QUESTION about the current project via DeepWiki RAG.
Response appears in *deepwiki-chat* buffer."
  (interactive "sAsk DeepWiki: ")
  (let* ((name (or (bw/deepwiki--current-repo)
                   (user-error "Not in a project")))
         (proj (project-current t))
         (root (project-root proj))
         (url (format "%s/chat/completions/stream" bw/deepwiki-api-url))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode `(:repo_url ,root
                         :messages [(:role "user" :content ,question)])))
         (buf (get-buffer-create "*deepwiki-chat*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n" (propertize (format "Q: %s\n\n" question)
                                 'face '(:weight bold)))
        (insert "Retrieving...\n"))
      (markdown-mode)
      (read-only-mode 1)
      (display-buffer buf))
    (url-retrieve url
      (lambda (_status)
        (goto-char url-http-end-of-headers)
        (let ((response (buffer-substring-no-properties (point) (point-max))))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              ;; Remove "Retrieving..." line
              (save-excursion
                (goto-char (point-max))
                (forward-line -1)
                (when (looking-at "Retrieving\\.\\.\\.")
                  (delete-region (point) (point-max))))
              (goto-char (point-max))
              (insert response "\n")
              (goto-char (point-max))))))
      nil t)))

;; --- Open frontend in browser ---

(defun bw/deepwiki-browse ()
  "Open DeepWiki frontend in the default browser."
  (interactive)
  (browse-url bw/deepwiki-frontend-url))

;; --- Health check ---

(defun bw/deepwiki-status ()
  "Check if the DeepWiki server is running."
  (interactive)
  (url-retrieve (format "%s/health" bw/deepwiki-api-url)
    (lambda (_status)
      (if (and (boundp 'url-http-response-status)
               (= url-http-response-status 200))
          (message "DeepWiki running at %s" bw/deepwiki-api-url)
        (message "DeepWiki NOT reachable at %s" bw/deepwiki-api-url)))
    nil t))

;; --- TOC sidebar ---

(defun bw/deepwiki-toc ()
  "Show wiki table of contents in a side window."
  (interactive)
  (let ((wiki-buf (seq-find (lambda (b) (string-prefix-p "*deepwiki:" (buffer-name b)))
                            (buffer-list))))
    (unless wiki-buf
      (user-error "No deepwiki buffer open. Run SPC c m first"))
    (let ((toc-buf (get-buffer-create "*deepwiki-toc*")))
      (with-current-buffer toc-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "DeepWiki TOC\n" 'face '(:weight bold :height 1.2))
                  (make-string 20 ?─) "\n\n")
          (with-current-buffer wiki-buf
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward "^## \\(.+\\)$" nil t)
                (let ((heading (match-string 1))
                      (pos (match-beginning 0)))
                  (with-current-buffer toc-buf
                    (insert-text-button heading
                      'action (lambda (_btn)
                                (let ((target-pos (button-get _btn 'target-pos))
                                      (target-buf (button-get _btn 'target-buf)))
                                  (pop-to-buffer target-buf)
                                  (goto-char target-pos)
                                  (recenter 0)))
                      'target-pos pos
                      'target-buf wiki-buf
                      'face '(:foreground "#51afef" :underline t))
                    (insert "\n")))))))
        (goto-char (point-min))
        (special-mode))
      (display-buffer-in-side-window toc-buf
        '((side . left) (window-width . 0.25) (slot . 1))))))

;; --- Generate wiki (progress buffer) ---

(defun bw/deepwiki-generate ()
  "Generate a DeepWiki for the current project.
Opens the DeepWiki frontend in a browser for the generation flow."
  (interactive)
  (let* ((name (or (bw/deepwiki--current-repo)
                   (user-error "Not in a project")))
         (proj (project-current t))
         (root (project-root proj))
         (buf (get-buffer-create "*deepwiki-progress*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Generating DeepWiki for %s\n" name)
                            'face '(:weight bold :height 1.2))
                (make-string 40 ?─) "\n\n"
                "The frontend orchestrates multi-step wiki generation:\n"
                "  1. Clone + index repo (deterministic)\n"
                "  2. Generate wiki structure (LLM)\n"
                "  3. Generate each page with RAG context (LLM, one call per page)\n"
                "  4. Cache complete wiki (deterministic)\n\n"
                "Opening browser...\n"
                (format "When done, press SPC c m to view in Emacs.\n")))
      (special-mode))
    (display-buffer buf)
    (browse-url (format "%s?repo=%s"
                        bw/deepwiki-frontend-url
                        (url-hexify-string root)))))

(provide 'deepwiki)
;;; deepwiki.el ends here
