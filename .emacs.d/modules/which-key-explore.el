;;; which-key-explore.el --- interactive which-key explorer -*- lexical-binding: t; -*-

;; Interactive browsable which-key menu.  Provides a modal explorer buffer
;; that lists all bindings under a given prefix with rich metadata, drill-down
;; navigation, and posframe info popups.

;;; Code:

(require 'posframe)

;; ---------------------------------------------------------------------------
;; State variables
;; ---------------------------------------------------------------------------

(defvar bw/which-key-explore--active nil
  "Non-nil when which-key explorer is active.")

(defvar bw/which-key-explore--prefix nil
  "Current prefix string being explored (e.g., \"SPC\" or \"SPC b\").")

(defvar bw/which-key-explore--history nil
  "Navigation history stack of prefix strings for back.")

(defvar bw/which-key-explore--forward-history nil
  "Forward navigation stack (for M-right after M-left).")

(defvar bw/which-key-explore--info-timer nil
  "Idle timer for showing info posframe.")

(defvar bw/which-key-explore--buffer-name "*which-key-explore*"
  "Buffer name for the explorer.")

;; ---------------------------------------------------------------------------
;; Prefix string to keymap
;; ---------------------------------------------------------------------------

(defun bw/which-key-explore--prefix-to-keymap (prefix)
  "Convert PREFIX string to its keymap.
E.g., \"SPC\" -> bw/leader-map, \"SPC b\" -> bw/leader-b-map."
  (let ((parts (split-string prefix " ")))
    (cond
     ((equal parts '("SPC")) bw/leader-map)
     ((equal parts '("SPC" "b")) bw/leader-b-map)
     ((equal parts '("SPC" "c")) bw/leader-c-map)
     ((equal parts '("SPC" "f")) bw/leader-f-map)
     ((equal parts '("SPC" "g")) bw/leader-g-map)
     ((equal parts '("SPC" "h")) bw/leader-h-map)
     ((equal parts '("SPC" "h" "r")) bw/leader-hr-map)
     ((equal parts '("SPC" "i")) bw/leader-i-map)
     ((equal parts '("SPC" "n")) bw/leader-n-map)
     ((equal parts '("SPC" "o")) bw/leader-o-map)
     ((equal parts '("SPC" "o" "h")) bw/leader-oh-map)
     ((equal parts '("SPC" "p")) bw/leader-p-map)
     ((equal parts '("SPC" "q")) bw/leader-q-map)
     ((equal parts '("SPC" "s")) bw/leader-s-map)
     ((equal parts '("SPC" "t")) bw/leader-t-map)
     ((equal parts '("SPC" "w")) bw/leader-w-map)
     ;; Fallback: try looking up via the leader map
     (t (let* ((tail (cdr parts))  ; drop "SPC"
               (km bw/leader-map))
          (dolist (k tail)
            (when (keymapp km)
              (setq km (lookup-key km (kbd k)))))
          (and (keymapp km) km))))))

;; ---------------------------------------------------------------------------
;; Which-key label helpers
;; ---------------------------------------------------------------------------

(defun bw/which-key-explore--get-which-key-label (prefix key-str)
  "Get which-key replacement label for KEY-STR under PREFIX.
Returns the label string or nil."
  (when (boundp 'which-key-replacement-alist)
    (let ((full-key (concat prefix " " key-str)))
      (catch 'found
        (dolist (entry which-key-replacement-alist)
          (let ((pattern (car entry))
                (replacement (cdr entry)))
            (when (and (consp pattern) (consp replacement))
              (let ((key-pat (car pattern))
                    (cmd-pat (cdr pattern))
                    (rep-str (cdr replacement)))
                (when (and rep-str
                           (or (and key-pat (stringp key-pat)
                                    (string-match-p key-pat full-key))
                               (null key-pat)))
                  (throw 'found rep-str))))))
        nil))))

(defun bw/which-key-explore--get-which-key-label-for-cmd (cmd)
  "Get which-key label associated with CMD, if any."
  (when (and cmd (boundp 'which-key-replacement-alist))
    (let ((cmd-name (symbol-name cmd)))
      (catch 'found
        (dolist (entry which-key-replacement-alist)
          (let ((pattern (car entry))
                (replacement (cdr entry)))
            (when (and (consp pattern) (consp replacement))
              (let ((cmd-pat (cdr pattern))
                    (rep-str (cdr replacement)))
                (when (and rep-str cmd-pat (stringp cmd-pat)
                           (string-match-p cmd-pat cmd-name))
                  (throw 'found rep-str))))))
        nil))))

;; ---------------------------------------------------------------------------
;; Get bindings for a prefix
;; ---------------------------------------------------------------------------

(defun bw/which-key-explore--get-bindings (prefix)
  "Return list of (KEY COMMAND LABEL IS-PREFIX) for PREFIX."
  (let* ((keymap (bw/which-key-explore--prefix-to-keymap prefix))
         (result '()))
    (when (keymapp keymap)
      (map-keymap
       (lambda (event binding)
         (let* ((key-str (key-description (vector event)))
                ;; Unwrap (LABEL . COMMAND) cons cells used by which-key
                (wk-label (and (consp binding)
                               (not (keymapp binding))
                               (stringp (car binding))
                               (car binding)))
                (real-binding (if wk-label (cdr binding) binding))
                (is-prefix (keymapp real-binding))
                (cmd (cond
                      (is-prefix 'prefix)
                      ((symbolp real-binding) real-binding)
                      ((and (consp real-binding) (symbolp (cdr real-binding)))
                       (cdr real-binding))
                      (t nil)))
                (label (or wk-label
                           (bw/which-key-explore--get-which-key-label prefix key-str))))
           (when (and (not (eq real-binding 'undefined))
                      (not (string-match-p "^<" key-str)))
             (push (list key-str cmd label is-prefix)
                   result))))
       keymap))
    (sort result (lambda (a b) (string< (car a) (car b))))))

;; ---------------------------------------------------------------------------
;; Render
;; ---------------------------------------------------------------------------

(defun bw/which-key-explore--render ()
  "Render the explorer buffer for current prefix."
  (let* ((buf (get-buffer-create bw/which-key-explore--buffer-name))
         (bindings (bw/which-key-explore--get-bindings bw/which-key-explore--prefix))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert (propertize (format " %s\n" bw/which-key-explore--prefix)
                          'face '(:height 1.2 :weight bold :foreground "#51afef")))
      (insert (propertize (make-string 60 ?-) 'face '(:foreground "#5B6268")))
      (insert "\n")
      (dolist (b bindings)
        (let* ((key (nth 0 b))
               (cmd (nth 1 b))
               (label (nth 2 b))
               (is-prefix (nth 3 b))
               (cmd-str (cond
                         ((symbolp cmd) (symbol-name cmd))
                         ((keymapp cmd) "+prefix")
                         (t (format "%s" cmd)))))
          (insert (propertize (format " %-6s" key)
                              'face '(:foreground "#ECBE7B" :weight bold))
                  (propertize (format "%-35s" cmd-str)
                              'face (if is-prefix
                                        '(:foreground "#51afef" :weight bold)
                                      '(:foreground "#bbc2cf")))
                  (if label
                      (propertize (format " -- %s" label)
                                  'face '(:foreground "#5B6268"))
                    "")
                  "\n")))
      (goto-char (point-min))
      (forward-line 2) ; skip header
      (setq buffer-read-only t)
      (bw/which-key-explore-mode 1)
      (when (fboundp 'evil-emacs-state)
        (evil-emacs-state)))
    (display-buffer buf
                    '((display-buffer-in-side-window)
                      (side . bottom)
                      (window-height . 0.4)))
    (let ((win (get-buffer-window buf)))
      (when (window-live-p win)
        (select-window win)))))

;; ---------------------------------------------------------------------------
;; Parsing current line
;; ---------------------------------------------------------------------------

(defun bw/which-key-explore--command-at-point ()
  "Get command symbol from current explorer line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at " *\\S-+ +\\(\\S-+\\)")
      (intern-soft (match-string 1)))))

(defun bw/which-key-explore--binding-at-point ()
  "Get full binding data from current explorer line.
Returns (KEY CMD LABEL IS-PREFIX) or nil."
  (let* ((line-num (- (line-number-at-pos) 3)) ; skip header lines
         (bindings (bw/which-key-explore--get-bindings bw/which-key-explore--prefix)))
    (when (and (>= line-num 0) (< line-num (length bindings)))
      (nth line-num bindings))))

;; ---------------------------------------------------------------------------
;; Source and config helpers
;; ---------------------------------------------------------------------------

(defun bw/which-key-explore--find-source (cmd)
  "Find source file:line for CMD."
  (condition-case nil
      (let ((loc (find-function-noselect cmd)))
        (when (and (car loc) (cdr loc))
          (with-current-buffer (car loc)
            (goto-char (cdr loc))
            (format "%s:%d" (buffer-file-name) (line-number-at-pos)))))
    (error nil)))

(defun bw/which-key-explore--find-config-location (cmd)
  "Find where CMD is bound in config files.  Returns \"file:line\" or nil."
  (let ((symbol-str (regexp-quote (symbol-name cmd)))
        (dirs (list (expand-file-name "modules/" user-emacs-directory)
                    (expand-file-name "profiles/" user-emacs-directory))))
    (catch 'found
      (dolist (dir dirs)
        (when (file-directory-p dir)
          (dolist (file (directory-files dir t "\\.el\\'"))
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (when (re-search-forward (format "'%s\\b" symbol-str) nil t)
                (throw 'found
                       (format "%s:%d" file (line-number-at-pos))))))))
      nil)))

(defun bw/which-key-explore--grep-for-symbol (cmd)
  "Grep for CMD symbol across config files.  Returns list of file:line strings."
  (let ((symbol-str (symbol-name cmd))
        (dirs (list (expand-file-name "modules/" user-emacs-directory)
                    (expand-file-name "profiles/" user-emacs-directory)))
        (results '()))
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.el\\'"))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward (regexp-quote symbol-str) nil t)
              (push (format "%s:%d: %s"
                            (file-name-nondirectory file)
                            (line-number-at-pos)
                            (string-trim (thing-at-point 'line t)))
                    results))))))
    (nreverse results)))

;; ---------------------------------------------------------------------------
;; Info posframe
;; ---------------------------------------------------------------------------

(defun bw/which-key-explore--start-info-timer ()
  "Start idle timer for info posframe."
  (bw/which-key-explore--cancel-info-timer)
  (setq bw/which-key-explore--info-timer
        (run-with-idle-timer 0.3 t #'bw/which-key-explore--show-info)))

(defun bw/which-key-explore--cancel-info-timer ()
  "Cancel info timer."
  (when bw/which-key-explore--info-timer
    (cancel-timer bw/which-key-explore--info-timer)
    (setq bw/which-key-explore--info-timer nil)))

(defun bw/which-key-explore--show-info ()
  "Show info posframe for command on current line."
  (when (and bw/which-key-explore--active
             (eq (current-buffer) (get-buffer bw/which-key-explore--buffer-name)))
    (let* ((cmd (bw/which-key-explore--command-at-point))
           (doc (when (and cmd (fboundp cmd))
                  (car (split-string (or (documentation cmd t) "No documentation") "\n"))))
           (src (when (and cmd (fboundp cmd))
                  (bw/which-key-explore--find-source cmd)))
           (config-loc (when cmd
                         (bw/which-key-explore--find-config-location cmd)))
           (label (when cmd (bw/which-key-explore--get-which-key-label-for-cmd cmd))))
      (when cmd
        (posframe-show "*which-key-explore-info*"
                       :string (concat
                                (propertize (symbol-name cmd) 'face '(:weight bold :foreground "#51afef"))
                                "\n"
                                (when doc (propertize doc 'face '(:foreground "#bbc2cf")))
                                (when doc "\n")
                                (when src (propertize (format "Source: %s" src) 'face '(:foreground "#98be65")))
                                (when src "\n")
                                (when label (propertize (format "Label: %s" label) 'face '(:foreground "#ECBE7B")))
                                (when label "\n")
                                (when config-loc (propertize (format "Config: %s" config-loc) 'face '(:foreground "#c678dd"))))
                       :position (point)
                       :background-color "#282c34"
                       :foreground-color "#bbc2cf"
                       :border-width 1
                       :border-color "#5B6268"
                       :timeout 5)))))

;; ---------------------------------------------------------------------------
;; Navigation
;; ---------------------------------------------------------------------------

(defun bw/which-key-explore--drill ()
  "Drill into prefix key on current line."
  (interactive)
  (let* ((binding (bw/which-key-explore--binding-at-point))
         (key (nth 0 binding))
         (is-prefix (nth 3 binding)))
    (when is-prefix
      (push bw/which-key-explore--prefix bw/which-key-explore--history)
      (setq bw/which-key-explore--forward-history nil)
      (setq bw/which-key-explore--prefix (concat bw/which-key-explore--prefix " " key))
      (bw/which-key-explore--render))))

(defun bw/which-key-explore--go-up ()
  "Go up one prefix level."
  (interactive)
  (let ((parts (split-string bw/which-key-explore--prefix " ")))
    (when (> (length parts) 1)
      (push bw/which-key-explore--prefix bw/which-key-explore--history)
      (setq bw/which-key-explore--forward-history nil)
      (setq bw/which-key-explore--prefix (string-join (butlast parts) " "))
      (bw/which-key-explore--render))))

(defun bw/which-key-explore--history-back ()
  "Go back in navigation history."
  (interactive)
  (when bw/which-key-explore--history
    (push bw/which-key-explore--prefix bw/which-key-explore--forward-history)
    (setq bw/which-key-explore--prefix (pop bw/which-key-explore--history))
    (bw/which-key-explore--render)))

(defun bw/which-key-explore--history-forward ()
  "Go forward in navigation history."
  (interactive)
  (when bw/which-key-explore--forward-history
    (push bw/which-key-explore--prefix bw/which-key-explore--history)
    (setq bw/which-key-explore--prefix (pop bw/which-key-explore--forward-history))
    (bw/which-key-explore--render)))

;; ---------------------------------------------------------------------------
;; Meta-commands
;; ---------------------------------------------------------------------------

(defun bw/which-key-explore--goto-source ()
  "Open source file at definition of command on current line."
  (interactive)
  (let ((cmd (bw/which-key-explore--command-at-point)))
    (when cmd
      (bw/which-key-explore--exit)
      (find-function cmd))))

(defun bw/which-key-explore--goto-config ()
  "Open config file at binding definition for command on current line."
  (interactive)
  (let* ((cmd (bw/which-key-explore--command-at-point))
         (loc (when cmd (bw/which-key-explore--find-config-location cmd))))
    (when loc
      (bw/which-key-explore--exit)
      (let* ((parts (split-string loc ":"))
             (file (car parts))
             (line (string-to-number (cadr parts))))
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun bw/which-key-explore--copy-command-name ()
  "Copy command name to kill-ring."
  (interactive)
  (let ((cmd (bw/which-key-explore--command-at-point)))
    (when cmd
      (kill-new (symbol-name cmd))
      (message "Copied: %s" (symbol-name cmd)))))

(defun bw/which-key-explore--copy-source-location ()
  "Copy file:line of command source to kill-ring."
  (interactive)
  (let* ((cmd (bw/which-key-explore--command-at-point))
         (src (when cmd (bw/which-key-explore--find-source cmd))))
    (when src
      (kill-new src)
      (message "Copied: %s" src))))

(defun bw/which-key-explore--copy-which-key-label ()
  "Copy which-key label to kill-ring."
  (interactive)
  (let* ((cmd (bw/which-key-explore--command-at-point))
         (label (when cmd (bw/which-key-explore--get-which-key-label-for-cmd cmd))))
    (when label
      (kill-new label)
      (message "Copied: %s" label))))

(defun bw/which-key-explore--describe-function ()
  "Run describe-function for command on current line."
  (interactive)
  (let ((cmd (bw/which-key-explore--command-at-point)))
    (when cmd
      (describe-function cmd))))

(defun bw/which-key-explore--show-callers ()
  "Show other files/keymaps that reference command on current line."
  (interactive)
  (let* ((cmd (bw/which-key-explore--command-at-point))
         (results (when cmd (bw/which-key-explore--grep-for-symbol cmd))))
    (if results
        (progn
          (with-current-buffer (get-buffer-create "*which-key-explore-callers*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "References to %s:\n\n" cmd))
              (dolist (r results)
                (insert r "\n"))
              (setq buffer-read-only t))
            (display-buffer (current-buffer))))
      (message "No references found for %s" cmd))))

;; ---------------------------------------------------------------------------
;; Exit
;; ---------------------------------------------------------------------------

(defun bw/which-key-explore--exit ()
  "Exit which-key explorer."
  (interactive)
  (bw/which-key-explore--cancel-info-timer)
  (posframe-delete "*which-key-explore-info*")
  (setq bw/which-key-explore--active nil)
  (let ((win (get-buffer-window bw/which-key-explore--buffer-name)))
    (when win (delete-window win)))
  (when (get-buffer bw/which-key-explore--buffer-name)
    (kill-buffer bw/which-key-explore--buffer-name)))

;; ---------------------------------------------------------------------------
;; Minor mode
;; ---------------------------------------------------------------------------

(define-minor-mode bw/which-key-explore-mode
  "Which-key explorer navigation mode."
  :lighter " WK-Explore"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Navigation
            (define-key map (kbd "j") #'next-line)
            (define-key map (kbd "k") #'previous-line)
            (define-key map (kbd "<down>") #'next-line)
            (define-key map (kbd "<up>") #'previous-line)
            ;; Drill into prefix
            (define-key map (kbd "RET") #'bw/which-key-explore--drill)
            (define-key map (kbd "<right>") #'bw/which-key-explore--drill)
            ;; Go up
            (define-key map (kbd "DEL") #'bw/which-key-explore--go-up)
            (define-key map (kbd "h") #'bw/which-key-explore--go-up)
            ;; History navigation
            (define-key map (kbd "M-<left>") #'bw/which-key-explore--history-back)
            (define-key map (kbd "M-<right>") #'bw/which-key-explore--history-forward)
            ;; Toggle/exit
            (define-key map (kbd "~") #'bw/which-key-explore--exit)
            (define-key map (kbd "q") #'bw/which-key-explore--exit)
            ;; Meta-commands
            (define-key map (kbd "e") #'bw/which-key-explore--goto-source)
            (define-key map (kbd "E") #'bw/which-key-explore--goto-config)
            (define-key map (kbd "y") #'bw/which-key-explore--copy-command-name)
            (define-key map (kbd "Y") #'bw/which-key-explore--copy-source-location)
            (define-key map (kbd "d") #'bw/which-key-explore--describe-function)
            (define-key map (kbd "w") #'bw/which-key-explore--copy-which-key-label)
            (define-key map (kbd "c") #'bw/which-key-explore--show-callers)
            map))

;; ---------------------------------------------------------------------------
;; Entry points
;; ---------------------------------------------------------------------------

(defun bw/which-key-explore-toggle ()
  "Toggle which-key explorer for top-level SPC menu."
  (interactive)
  (if bw/which-key-explore--active
      (bw/which-key-explore--exit)
    (bw/which-key-explore--enter "SPC")))

(defun bw/which-key-explore-prefix (prefix)
  "Open which-key explorer at PREFIX level (e.g., \"SPC b\")."
  (interactive "sPrefix: ")
  (if bw/which-key-explore--active
      (bw/which-key-explore--exit)
    (bw/which-key-explore--enter prefix)))

(defun bw/which-key-explore--enter (prefix)
  "Enter explorer mode for PREFIX."
  (setq bw/which-key-explore--active t
        bw/which-key-explore--prefix prefix
        bw/which-key-explore--history nil
        bw/which-key-explore--forward-history nil)
  (bw/which-key-explore--render)
  (bw/which-key-explore--start-info-timer))

(provide 'which-key-explore)
;;; which-key-explore.el ends here
