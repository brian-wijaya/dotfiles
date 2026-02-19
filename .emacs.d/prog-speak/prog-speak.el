;;; prog-speak.el --- Deterministic English surface-form normalization -*- lexical-binding: t; -*-

;; Author: Implementation Agent
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: text, convenience

;;; Commentary:

;; prog-speak is a deterministic English surface-form normalization transform
;; designed to reduce syntactic overhead during reading.
;;
;; It is NOT a grammar engine.
;; It is NOT reversible without access to original text.
;; It is NOT intended to produce grammatically correct English.
;;
;; Its sole purpose is to compress written English into a stripped-down form
;; resembling utilitarian, second-language technical English.
;;
;; Usage:
;;   M-x prog-speak-toggle          - Toggle overlay-based display
;;   M-x prog-speak-rewrite-buffer  - Destructively rewrite buffer (requires confirmation)
;;   M-x prog-speak-rewrite-region  - Destructively rewrite active region

;;; Code:

(require 'cl-lib)

;; Add this directory to load-path for prog-speak-lemmas
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'prog-speak-lemmas)

;;; Customization

(defgroup prog-speak nil
  "Deterministic English surface-form normalization."
  :group 'text
  :prefix "prog-speak-")

;;; Protected word list

(defconst prog-speak--protected-words
  '("is" "was" "has" "does" "this" "his" "its")
  "Words that must not have trailing s/es removed.")

;;; Progress reporting

(defvar prog-speak--progress-cancel nil
  "When non-nil, current operation should be cancelled.")

(defun prog-speak--progress-reporter-create (message total)
  "Create a progress reporter with MESSAGE for TOTAL units."
  (make-progress-reporter message 0 total))

(defun prog-speak--progress-reporter-update (reporter value)
  "Update REPORTER with VALUE and check for cancellation."
  (progress-reporter-update reporter value)
  (when (input-pending-p)
    (let ((key (read-event nil nil 0)))
      (when (and key (eq key ?\C-g))
        (setq prog-speak--progress-cancel t))))
  (when prog-speak--progress-cancel
    (signal 'quit nil)))

(defun prog-speak--progress-reporter-done (reporter)
  "Mark REPORTER as done."
  (progress-reporter-done reporter))

;;; Core transformation functions

(defun prog-speak--remove-articles (text)
  "Remove articles (a, an, the) from TEXT.
Articles are removed if and only if:
- token is exactly \"a\", \"an\", or \"the\"
- token is lowercase
- token is surrounded by whitespace or string boundary."
  (let ((result text))
    ;; Match articles surrounded by whitespace or boundaries
    ;; Pattern: (^|\\s)(a|an|the)(\\s|$) → \\1\\3
    (setq result (replace-regexp-in-string
                  "\\(^\\|[[:space:]]\\)\\(a\\|an\\|the\\)\\([[:space:]]\\|$\\)"
                  "\\1\\3"
                  result t))
    ;; Handle multiple spaces that may result
    (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " result t)))

(defun prog-speak--de-inflect-verb (word)
  "Return lemma for WORD using dictionary lookup with heuristic fallback.
Uses NIH SPECIALIST Lexicon via prog-speak-lemmas.el for accurate lemmatization."
  (cond
   ;; Protected words - never transform
   ((member word prog-speak--protected-words)
    word)
   ;; Must be lowercase
   ((not (string-match-p "\\`[a-z]+\\'" word))
    word)
   ;; Dictionary lookup (primary) - 20k+ verb mappings from SPECIALIST Lexicon
   ((gethash word prog-speak--lemma-table))
   ;; Heuristic fallback for unknown words
   ((< (length word) 4)
    word)
   ((string-match-p "ies\\'" word)
    (concat (substring word 0 -3) "y"))
   ((string-match-p "[sxz]es\\'" word)
    (substring word 0 -2))
   ((string-match-p "\\(ch\\|sh\\)es\\'" word)
    (substring word 0 -2))
   ((string-match-p "s\\'" word)
    (substring word 0 -1))
   ;; No change
   (t word)))

(defun prog-speak--de-inflect-verbs (text)
  "De-inflect third-person singular verbs in TEXT.
Only applies to lowercase words of 4+ characters that:
- End with s or es
- Are preceded by a lowercase word
- Are not in the protected list."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Match: lowercase word followed by space followed by word ending in s/es
    (while (re-search-forward
            "\\b\\([a-z]+\\)[[:space:]]+\\([a-z]\\{3,\\}s\\)\\b"
            nil t)
      (let* ((_preceding (match-string 1))  ; required for regex but unused
             (word (match-string 2))
             (word-start (match-beginning 2))
             (word-end (match-end 2))
             (de-inflected (prog-speak--de-inflect-verb word)))
        (unless (string= word de-inflected)
          (goto-char word-start)
          (delete-region word-start word-end)
          (insert de-inflected))))
    (buffer-string)))

(defun prog-speak--transform-text (text)
  "Apply all prog-speak transformations to TEXT."
  (prog-speak--de-inflect-verbs
   (prog-speak--remove-articles text)))

;;; Scope detection

(defun prog-speak--in-org-prose-p (pos)
  "Return non-nil if POS is in Org mode prose (not code/blocks/drawers)."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char pos)
      (let ((element (org-element-at-point)))
        (when element
          (let ((type (org-element-type element)))
            (and (memq type '(paragraph headline))
                 ;; Not inside a source block, example block, etc.
                 (not (org-in-src-block-p))
                 ;; Not in a drawer
                 (not (org-at-drawer-p))
                 (not (org-in-drawer-p)))))))))

(defun prog-speak--in-code-comment-p (pos)
  "Return non-nil if POS is inside a comment in a programming mode."
  (and (not (derived-mode-p 'org-mode))
       (derived-mode-p 'prog-mode)
       (save-excursion
         (goto-char pos)
         (let ((ppss (syntax-ppss)))
           (or (nth 4 ppss)    ; in a comment
               (nth 3 ppss))))); in a string (docstring)
  )

(defun prog-speak--in-scope-p (pos)
  "Return non-nil if POS is within transformation scope."
  (or (prog-speak--in-org-prose-p pos)
      (prog-speak--in-code-comment-p pos)
      ;; For non-org, non-prog buffers, apply everywhere (text-mode, etc.)
      (and (not (derived-mode-p 'org-mode))
           (not (derived-mode-p 'prog-mode)))))

;;; Region detection for scoped transformation

(defun prog-speak--get-transformable-regions ()
  "Return a list of (START . END) regions that are within transformation scope."
  (let ((regions '())
        (pos (point-min))
        region-start)
    (save-excursion
      (while (< pos (point-max))
        (goto-char pos)
        (if (prog-speak--in-scope-p pos)
            (progn
              (unless region-start
                (setq region-start pos))
              (setq pos (1+ pos)))
          (when region-start
            (push (cons region-start pos) regions)
            (setq region-start nil))
          (setq pos (1+ pos))))
      (when region-start
        (push (cons region-start (point-max)) regions)))
    (nreverse regions)))

;;; Overlay management

(defvar-local prog-speak--overlays nil
  "List of overlays created by prog-speak-mode.")

(defun prog-speak--clear-overlays ()
  "Remove all prog-speak overlays from current buffer."
  (mapc #'delete-overlay prog-speak--overlays)
  (setq prog-speak--overlays nil))

(defun prog-speak--create-overlay (start end _original transformed)
  "Create an overlay from START to END displaying TRANSFORMED instead of ORIGINAL."
  (let ((ov (make-overlay start end nil t nil)))
    (overlay-put ov 'prog-speak t)
    (overlay-put ov 'display transformed)
    (overlay-put ov 'evaporate t)
    (push ov prog-speak--overlays)
    ov))

(defun prog-speak--apply-overlays ()
  "Apply prog-speak transformation overlays to current buffer."
  (prog-speak--clear-overlays)
  (let* ((total (buffer-size))
         (reporter (when (> total 1000)
                     (prog-speak--progress-reporter-create
                      "Applying prog-speak..." total)))
         (prog-speak--progress-cancel nil)
         (processed 0))
    (condition-case nil
        (progn
          ;; First pass: remove articles
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "\\b\\w+\\b" nil t)
              (let* ((word-start (match-beginning 0))
                     (word-end (match-end 0))
                     (word (match-string 0)))
                (when (and (prog-speak--in-scope-p word-start)
                           (member word '("a" "an" "the"))
                           (string-match-p "\\`[a-z]+\\'" word))
                  ;; Remove article and trailing space if present
                  (let ((end-with-space (if (and (< word-end (point-max))
                                                 (eq (char-after word-end) ?\s))
                                            (1+ word-end)
                                          word-end)))
                    (prog-speak--create-overlay word-start end-with-space word "")))
                (setq processed word-end)
                (when reporter
                  (prog-speak--progress-reporter-update reporter processed)))))
          ;; Second pass: de-inflect verbs
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward
                    "\\b[a-z]+[[:space:]]+\\([a-z]\\{3,\\}s\\)\\b"
                    nil t)
              (let* ((word (match-string 1))
                     (word-start (match-beginning 1))
                     (word-end (match-end 1)))
                (when (prog-speak--in-scope-p word-start)
                  (let ((de-inflected (prog-speak--de-inflect-verb word)))
                    (unless (string= word de-inflected)
                      ;; Check if there's already an overlay here
                      (unless (cl-some (lambda (ov)
                                         (and (overlay-get ov 'prog-speak)
                                              (= (overlay-start ov) word-start)))
                                       (overlays-at word-start))
                        (prog-speak--create-overlay word-start word-end word de-inflected))))))))
          (when reporter
            (prog-speak--progress-reporter-done reporter)))
      (quit
       (prog-speak--clear-overlays)
       (message "prog-speak: Cancelled")
       nil)
      (error
       (prog-speak--clear-overlays)
       (prog-speak-mode -1)
       (message "prog-speak: Error applying overlays. Mode disabled.")
       nil))))

;;; Minor mode definition

(define-minor-mode prog-speak-mode
  "Toggle prog-speak display transformation.
When enabled, articles (a, an, the) and third-person singular verb
inflections are hidden via overlays. Original buffer text is unchanged."
  :lighter " ProgSpeak"
  :group 'prog-speak
  (if prog-speak-mode
      (condition-case err
          (prog-speak--apply-overlays)
        (error
         (prog-speak--clear-overlays)
         (setq prog-speak-mode nil)
         (message "prog-speak: Failed to enable: %s" (error-message-string err))))
    (prog-speak--clear-overlays)))

;;; Interactive commands

;;;###autoload
(defun prog-speak-toggle ()
  "Toggle prog-speak-mode in current buffer."
  (interactive)
  (prog-speak-mode (if prog-speak-mode -1 1)))

;;;###autoload
(defun prog-speak-rewrite-buffer ()
  "Destructively rewrite entire buffer with prog-speak transformations.
This permanently modifies buffer text. Original can only be recovered via undo.
Requires explicit confirmation."
  (interactive)
  (unless (yes-or-no-p "This will permanently modify the buffer. Continue? ")
    (message "Cancelled.")
    (cl-return-from prog-speak-rewrite-buffer nil))
  (let* ((total (buffer-size))
         (reporter (prog-speak--progress-reporter-create
                    "Rewriting buffer..." total))
         (prog-speak--progress-cancel nil)
         (regions (prog-speak--get-transformable-regions))
         (offset 0)
         (processed 0))
    (condition-case err
        (progn
          ;; Process regions in reverse to maintain positions
          (dolist (region (nreverse regions))
            (let* ((start (+ (car region) offset))
                   (end (+ (cdr region) offset))
                   (original (buffer-substring-no-properties start end))
                   (transformed (prog-speak--transform-text original))
                   (delta (- (length transformed) (length original))))
              (when (not (string= original transformed))
                (goto-char start)
                (delete-region start end)
                (insert transformed)
                (setq offset (+ offset delta)))
              (setq processed (+ processed (- (cdr region) (car region))))
              (prog-speak--progress-reporter-update reporter processed)))
          (prog-speak--progress-reporter-done reporter)
          (message "prog-speak: Buffer rewritten."))
      (quit
       (message "prog-speak: Rewrite cancelled. Use undo to recover any partial changes."))
      (error
       (message "prog-speak: Error during rewrite: %s" (error-message-string err))))))

;;;###autoload
(defun prog-speak-rewrite-region (start end)
  "Destructively rewrite active region with prog-speak transformations.
This permanently modifies the text. Original can only be recovered via undo."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((total (- end start))
         (reporter (when (> total 500)
                     (prog-speak--progress-reporter-create
                      "Rewriting region..." total)))
         (prog-speak--progress-cancel nil)
         (original (buffer-substring-no-properties start end)))
    (condition-case err
        (progn
          ;; For region, we transform the whole thing at once
          ;; but still report progress
          (when reporter
            (prog-speak--progress-reporter-update reporter (/ total 2)))
          (let ((transformed (prog-speak--transform-text original)))
            (when reporter
              (prog-speak--progress-reporter-update reporter total))
            (goto-char start)
            (delete-region start end)
            (insert transformed)
            (when reporter
              (prog-speak--progress-reporter-done reporter))
            (message "prog-speak: Region rewritten.")))
      (quit
       (message "prog-speak: Rewrite cancelled."))
      (error
       (message "prog-speak: Error during rewrite: %s" (error-message-string err))))))

(provide 'prog-speak)

;;; prog-speak.el ends here
