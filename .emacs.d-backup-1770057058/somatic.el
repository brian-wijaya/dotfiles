;;; somatic.el --- Somatic sensor integration for Emacs -*- lexical-binding: t; -*-

;; Author: bw
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: hardware, tools
;; URL: https://github.com/bw/somatic

;;; Commentary:

;; Integration with the Somatic sensory perception daemon via D-Bus.
;; Provides real-time mode-line indicators for typing state, pointer
;; activity, and focus tracking.  Requires somatic-dbus running on the
;; session bus as org.somatic.Sensor.

;;; Code:

(require 'dbus)

(defgroup somatic nil
  "Somatic sensor integration."
  :group 'tools
  :prefix "somatic-")

(defcustom somatic-enable-mode-line t
  "Show somatic state in mode-line."
  :type 'boolean)

(defcustom somatic-enable-clipboard-sync nil
  "Sync clipboard changes to the kill ring."
  :type 'boolean)

(defcustom somatic-enable-focus-hooks nil
  "Run hooks on focus change signals."
  :type 'boolean)

(defcustom somatic-idle-threshold-ms 3000
  "Pointer dwell threshold for idle indicator."
  :type 'integer)

(defvar somatic--typing nil "Current typing state.")
(defvar somatic--typing-iki 0.0 "Current inter-key interval.")
(defvar somatic--pointer-idle nil "Pointer idle state.")
(defvar somatic--pointer-dwell 0 "Pointer dwell in ms.")
(defvar somatic--focus-window 0 "Focused window ID.")
(defvar somatic--last-seq 0 "Last state sequence.")
(defvar somatic--dbus-registrations nil "Active D-Bus signal registrations.")
(defvar somatic--connected nil "Whether D-Bus connection is active.")

;; ============================================================================
;; Mode-line
;; ============================================================================

(defvar somatic--mode-line-string "")

(defun somatic--update-mode-line ()
  "Recompute the mode-line indicator string."
  (setq somatic--mode-line-string
        (if (not somatic--connected)
            " [somatic:off]"
          (concat
           " ["
           ;; Typing indicator
           (if somatic--typing
               (propertize "T" 'face '(:foreground "#9ece6a" :weight bold))
             (propertize "." 'face '(:foreground "#565f89")))
           ;; Pointer indicator
           (if somatic--pointer-idle
               (propertize "P" 'face '(:foreground "#ff9e64"))
             (propertize "." 'face '(:foreground "#565f89")))
           "]")))
  (force-mode-line-update t))

;; ============================================================================
;; D-Bus signal handlers
;; ============================================================================

(defun somatic--on-typing-changed (is-typing iki)
  "Handle TypingStateChanged signal.
IS-TYPING is 0 or 1, IKI is inter-key interval in ms."
  (setq somatic--typing (not (zerop is-typing))
        somatic--typing-iki iki)
  (when somatic-enable-mode-line
    (somatic--update-mode-line)))

(defun somatic--on-focus-changed (window-id _name)
  "Handle FocusChanged signal.
WINDOW-ID is the X11 window ID."
  (setq somatic--focus-window window-id)
  (when somatic-enable-focus-hooks
    (run-hooks 'somatic-focus-change-hook))
  (when somatic-enable-mode-line
    (somatic--update-mode-line)))

(defun somatic--on-clipboard-changed (_hash _kind)
  "Handle ClipboardChanged signal.
Optionally syncs clipboard content to the Emacs kill ring."
  (when somatic-enable-clipboard-sync
    (let ((content (ignore-errors
                     (shell-command-to-string "xclip -selection clipboard -o 2>/dev/null"))))
      (when (and content (not (string-empty-p content)))
        (kill-new content)
        (message "somatic: clipboard synced (%d bytes)" (length content))))))

(defun somatic--on-pointer-idle (dwell-ms)
  "Handle PointerIdle signal.
DWELL-MS is pointer dwell time in milliseconds."
  (setq somatic--pointer-idle t
        somatic--pointer-dwell dwell-ms)
  (when somatic-enable-mode-line
    (somatic--update-mode-line)))

(defun somatic--on-state-changed (seq)
  "Handle StateChanged signal.
SEQ is the fusion sequence number."
  (setq somatic--last-seq seq)
  ;; Reset pointer idle when state changes (movement detected)
  (when somatic--pointer-idle
    (setq somatic--pointer-idle nil)
    (when somatic-enable-mode-line
      (somatic--update-mode-line))))

;; ============================================================================
;; D-Bus method calls
;; ============================================================================

(defun somatic-hud-message (text category)
  "Post a message to the somatic HUD overlay.
TEXT is the message string, CATEGORY is one of: info, build, e2e,
save, warn, error, test, hook, vault."
  (interactive
   (list (read-string "HUD message: ")
         (completing-read "Category: "
                          '("info" "build" "e2e" "save" "warn"
                            "error" "test" "hook" "vault")
                          nil t nil nil "info")))
  (dbus-call-method :session
                    "org.somatic.Sensor"
                    "/org/somatic/Sensor"
                    "org.somatic.Sensor"
                    "PostMessage"
                    text category))

(defun somatic-get-snapshot ()
  "Retrieve current sensor snapshot as an alist."
  (interactive)
  (let ((result (dbus-call-method :session
                                  "org.somatic.Sensor"
                                  "/org/somatic/Sensor"
                                  "org.somatic.Sensor"
                                  "GetSnapshot")))
    (when (called-interactively-p 'interactive)
      (message "somatic snapshot: %S" result))
    result))

(defun somatic-get-dynamics ()
  "Retrieve current pointer dynamics."
  (interactive)
  (let ((result (dbus-call-method :session
                                  "org.somatic.Sensor"
                                  "/org/somatic/Sensor"
                                  "org.somatic.Sensor"
                                  "GetDynamics")))
    (when (called-interactively-p 'interactive)
      (let ((vel (nth 0 result))
            (acc (nth 1 result))
            (dwl (nth 2 result))
            (ent (nth 3 result)))
        (message "pointer: vel=%.1f acc=%.1f dwell=%.0fms entropy=%.3f"
                 vel acc dwl ent)))
    result))

;; ============================================================================
;; Hooks
;; ============================================================================

(defvar somatic-focus-change-hook nil
  "Hook run when the focused X11 window changes.
The variable `somatic--focus-window' holds the new window ID.")

;; ============================================================================
;; Connection management
;; ============================================================================

(defun somatic--register-signals ()
  "Register all D-Bus signal handlers."
  (let ((svc "org.somatic.Sensor")
        (path "/org/somatic/Sensor")
        (iface "org.somatic.Sensor"))
    (push (dbus-register-signal :session svc path iface
                                "TypingStateChanged"
                                #'somatic--on-typing-changed)
          somatic--dbus-registrations)
    (push (dbus-register-signal :session svc path iface
                                "FocusChanged"
                                #'somatic--on-focus-changed)
          somatic--dbus-registrations)
    (push (dbus-register-signal :session svc path iface
                                "ClipboardChanged"
                                #'somatic--on-clipboard-changed)
          somatic--dbus-registrations)
    (push (dbus-register-signal :session svc path iface
                                "PointerIdle"
                                #'somatic--on-pointer-idle)
          somatic--dbus-registrations)
    (push (dbus-register-signal :session svc path iface
                                "StateChanged"
                                #'somatic--on-state-changed)
          somatic--dbus-registrations)))

(defun somatic--unregister-signals ()
  "Unregister all D-Bus signal handlers."
  (dolist (reg somatic--dbus-registrations)
    (dbus-unregister-object reg))
  (setq somatic--dbus-registrations nil))

;; ============================================================================
;; Minor mode
;; ============================================================================

(defvar somatic-mode-map (make-sparse-keymap)
  "Keymap for `somatic-mode'.")

;;;###autoload
(define-minor-mode somatic-mode
  "Global minor mode for somatic sensor integration.
Connects to the somatic-dbus daemon on the session bus and
provides real-time mode-line indicators for typing state and
pointer activity."
  :global t
  :lighter somatic--mode-line-string
  :keymap somatic-mode-map
  (if somatic-mode
      (condition-case err
          (progn
            (somatic--register-signals)
            (setq somatic--connected t)
            (somatic--update-mode-line)
            (message "somatic-mode enabled"))
        (dbus-error
         (setq somatic--connected nil)
         (somatic--update-mode-line)
         (message "somatic-mode: D-Bus connection failed: %s" (error-message-string err))))
    (somatic--unregister-signals)
    (setq somatic--connected nil
          somatic--typing nil
          somatic--pointer-idle nil)
    (somatic--update-mode-line)
    (message "somatic-mode disabled")))

(provide 'somatic)
;;; somatic.el ends here
