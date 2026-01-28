;;; which-key-explore.el --- interactive which-key explorer -*- lexical-binding: t; -*-

;; Interactive browsable which-key menu.  Provides a modal explorer buffer
;; that lists all bindings under a given prefix with rich metadata, drill-down
;; navigation, and posframe info popups.

;;; Code:

(require 'posframe)

;; ---------------------------------------------------------------------------
;; State variables
;; ---------------------------------------------------------------------------

(defvar bw/wke--active nil
  "Non-nil when which-key explorer is active.")

(defvar bw/wke--prefix nil
  "Current prefix string being explored (e.g., \"SPC\" or \"SPC b\").")

(defvar bw/wke--history nil
  "Navigation history stack of prefix strings for back.")

(defvar bw/wke--forward-history nil
  "Forward navigation stack (for M-right after M-left).")

(defvar bw/wke--info-timer nil
  "Idle timer for showing info posframe.")

(defvar bw/wke--buffer-name "*which-key-explore*"
  "Buffer name for the explorer.")

;; ---------------------------------------------------------------------------
;; Prefix string to keymap
;; ---------------------------------------------------------------------------

(defun bw/wke--prefix-to-keymap (prefix)
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

(defun bw/wke--get-which-key-label (prefix key-str)
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

(defun bw/wke--get-which-key-label-for-cmd (cmd)
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

(defun bw/wke--get-bindings (prefix)
  "Return list of (KEY COMMAND LABEL IS-PREFIX) for PREFIX."
  (let* ((keymap (bw/wke--prefix-to-keymap prefix))
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
                           (bw/wke--get-which-key-label prefix key-str))))
           (when (and (not (eq real-binding 'undefined))
                      (not (string-match-p "^<" key-str)))
             (push (list key-str cmd label is-prefix)
                   result))))
       keymap))
    (sort result (lambda (a b) (string< (car a) (car b))))))

;; ---------------------------------------------------------------------------
;; Render
;; ---------------------------------------------------------------------------

(defun bw/wke--render ()
  "Render the explorer buffer for current prefix."
  (let* ((buf (get-buffer-create bw/wke--buffer-name))
         (bindings (bw/wke--get-bindings bw/wke--prefix))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert (propertize (format " %s\n" bw/wke--prefix)
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
      (bw/wke-mode 1)
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

(defun bw/wke--command-at-point ()
  "Get command symbol from current explorer line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at " *\\S-+ +\\(\\S-+\\)")
      (intern-soft (match-string 1)))))

(defun bw/wke--binding-at-point ()
  "Get full binding data from current explorer line.
Returns (KEY CMD LABEL IS-PREFIX) or nil."
  (let* ((line-num (- (line-number-at-pos) 3)) ; skip header lines
         (bindings (bw/wke--get-bindings bw/wke--prefix)))
    (when (and (>= line-num 0) (< line-num (length bindings)))
      (nth line-num bindings))))

;; ---------------------------------------------------------------------------
;; Source and config helpers
;; ---------------------------------------------------------------------------

(defun bw/wke--find-source (cmd)
  "Find source file:line for CMD."
  (condition-case nil
      (let ((loc (find-function-noselect cmd)))
        (when (and (car loc) (cdr loc))
          (with-current-buffer (car loc)
            (goto-char (cdr loc))
            (format "%s:%d" (buffer-file-name) (line-number-at-pos)))))
    (error nil)))

(defun bw/wke--find-config-location (cmd)
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

(defun bw/wke--grep-for-symbol (cmd)
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

(defun bw/wke--start-info-timer ()
  "Start idle timer for info posframe."
  (bw/wke--cancel-info-timer)
  (setq bw/wke--info-timer
        (run-with-idle-timer 0.3 t #'bw/wke--show-info)))

(defun bw/wke--cancel-info-timer ()
  "Cancel info timer."
  (when bw/wke--info-timer
    (cancel-timer bw/wke--info-timer)
    (setq bw/wke--info-timer nil)))

(defun bw/wke--show-info ()
  "Show info posframe for command on current line."
  (when (and bw/wke--active
             (eq (current-buffer) (get-buffer bw/wke--buffer-name)))
    (let* ((cmd (bw/wke--command-at-point))
           (doc (when (and cmd (fboundp cmd))
                  (car (split-string (or (documentation cmd t) "No documentation") "\n"))))
           (src (when (and cmd (fboundp cmd))
                  (bw/wke--find-source cmd)))
           (config-loc (when cmd
                         (bw/wke--find-config-location cmd)))
           (label (when cmd (bw/wke--get-which-key-label-for-cmd cmd))))
      (when cmd
        (posframe-show "*wke-info*"
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

(defun bw/wke--drill ()
  "Drill into prefix key on current line."
  (interactive)
  (let* ((binding (bw/wke--binding-at-point))
         (key (nth 0 binding))
         (is-prefix (nth 3 binding)))
    (when is-prefix
      (push bw/wke--prefix bw/wke--history)
      (setq bw/wke--forward-history nil)
      (setq bw/wke--prefix (concat bw/wke--prefix " " key))
      (bw/wke--render))))

(defun bw/wke--go-up ()
  "Go up one prefix level."
  (interactive)
  (let ((parts (split-string bw/wke--prefix " ")))
    (when (> (length parts) 1)
      (push bw/wke--prefix bw/wke--history)
      (setq bw/wke--forward-history nil)
      (setq bw/wke--prefix (string-join (butlast parts) " "))
      (bw/wke--render))))

(defun bw/wke--history-back ()
  "Go back in navigation history."
  (interactive)
  (when bw/wke--history
    (push bw/wke--prefix bw/wke--forward-history)
    (setq bw/wke--prefix (pop bw/wke--history))
    (bw/wke--render)))

(defun bw/wke--history-forward ()
  "Go forward in navigation history."
  (interactive)
  (when bw/wke--forward-history
    (push bw/wke--prefix bw/wke--history)
    (setq bw/wke--prefix (pop bw/wke--forward-history))
    (bw/wke--render)))

;; ---------------------------------------------------------------------------
;; Meta-commands
;; ---------------------------------------------------------------------------

(defun bw/wke--goto-source ()
  "Open source file at definition of command on current line."
  (interactive)
  (let ((cmd (bw/wke--command-at-point)))
    (when cmd
      (bw/wke--exit)
      (find-function cmd))))

(defun bw/wke--goto-config ()
  "Open config file at binding definition for command on current line."
  (interactive)
  (let* ((cmd (bw/wke--command-at-point))
         (loc (when cmd (bw/wke--find-config-location cmd))))
    (when loc
      (bw/wke--exit)
      (let* ((parts (split-string loc ":"))
             (file (car parts))
             (line (string-to-number (cadr parts))))
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun bw/wke--copy-command-name ()
  "Copy command name to kill-ring."
  (interactive)
  (let ((cmd (bw/wke--command-at-point)))
    (when cmd
      (kill-new (symbol-name cmd))
      (message "Copied: %s" (symbol-name cmd)))))

(defun bw/wke--copy-source-location ()
  "Copy file:line of command source to kill-ring."
  (interactive)
  (let* ((cmd (bw/wke--command-at-point))
         (src (when cmd (bw/wke--find-source cmd))))
    (when src
      (kill-new src)
      (message "Copied: %s" src))))

(defun bw/wke--copy-which-key-label ()
  "Copy which-key label to kill-ring."
  (interactive)
  (let* ((cmd (bw/wke--command-at-point))
         (label (when cmd (bw/wke--get-which-key-label-for-cmd cmd))))
    (when label
      (kill-new label)
      (message "Copied: %s" label))))

(defun bw/wke--describe-function ()
  "Run describe-function for command on current line."
  (interactive)
  (let ((cmd (bw/wke--command-at-point)))
    (when cmd
      (describe-function cmd))))

(defun bw/wke--show-callers ()
  "Show other files/keymaps that reference command on current line."
  (interactive)
  (let* ((cmd (bw/wke--command-at-point))
         (results (when cmd (bw/wke--grep-for-symbol cmd))))
    (if results
        (progn
          (with-current-buffer (get-buffer-create "*wke-callers*")
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

(defun bw/wke--exit ()
  "Exit which-key explorer."
  (interactive)
  (bw/wke--cancel-info-timer)
  (posframe-delete "*wke-info*")
  (setq bw/wke--active nil)
  (let ((win (get-buffer-window bw/wke--buffer-name)))
    (when win (delete-window win)))
  (when (get-buffer bw/wke--buffer-name)
    (kill-buffer bw/wke--buffer-name)))

;; ---------------------------------------------------------------------------
;; Minor mode
;; ---------------------------------------------------------------------------

(define-minor-mode bw/wke-mode
  "Which-key explorer navigation mode."
  :lighter " WKE"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Navigation
            (define-key map (kbd "j") #'next-line)
            (define-key map (kbd "k") #'previous-line)
            (define-key map (kbd "<down>") #'next-line)
            (define-key map (kbd "<up>") #'previous-line)
            ;; Drill into prefix
            (define-key map (kbd "RET") #'bw/wke--drill)
            (define-key map (kbd "<right>") #'bw/wke--drill)
            ;; Go up
            (define-key map (kbd "DEL") #'bw/wke--go-up)
            (define-key map (kbd "h") #'bw/wke--go-up)
            ;; History navigation
            (define-key map (kbd "M-<left>") #'bw/wke--history-back)
            (define-key map (kbd "M-<right>") #'bw/wke--history-forward)
            ;; Toggle/exit
            (define-key map (kbd "~") #'bw/wke--exit)
            (define-key map (kbd "q") #'bw/wke--exit)
            ;; Meta-commands
            (define-key map (kbd "e") #'bw/wke--goto-source)
            (define-key map (kbd "E") #'bw/wke--goto-config)
            (define-key map (kbd "y") #'bw/wke--copy-command-name)
            (define-key map (kbd "Y") #'bw/wke--copy-source-location)
            (define-key map (kbd "d") #'bw/wke--describe-function)
            (define-key map (kbd "w") #'bw/wke--copy-which-key-label)
            (define-key map (kbd "c") #'bw/wke--show-callers)
            map))

;; ---------------------------------------------------------------------------
;; Entry points
;; ---------------------------------------------------------------------------

(defun bw/which-key-explore-toggle ()
  "Toggle which-key explorer for top-level SPC menu."
  (interactive)
  (if bw/wke--active
      (bw/wke--exit)
    (bw/wke--enter "SPC")))

(defun bw/which-key-explore-prefix (prefix)
  "Open which-key explorer at PREFIX level (e.g., \"SPC b\")."
  (interactive "sPrefix: ")
  (if bw/wke--active
      (bw/wke--exit)
    (bw/wke--enter prefix)))

(defun bw/wke--enter (prefix)
  "Enter explorer mode for PREFIX."
  (setq bw/wke--active t
        bw/wke--prefix prefix
        bw/wke--history nil
        bw/wke--forward-history nil)
  (bw/wke--render)
  (bw/wke--start-info-timer))

(provide 'which-key-explore)
;;; which-key-explore.el ends here
