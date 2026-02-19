;;; sensor.el --- Sensor integration for Emacs -*- lexical-binding: t; -*-

;; Author: bw
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: hardware, tools
;; URL: https://github.com/bw/sensor

;;; Commentary:

;; Integration with the sensor perception daemon via D-Bus.
;; Provides real-time mode-line indicators for typing state, pointer
;; activity, and focus tracking.  Requires sensor running on the
;; session bus as org.sensor.Sensor.

;;; Code:

(require 'dbus)

(defgroup sensor nil
  "Sensor integration."
  :group 'tools
  :prefix "sensor-")

(defcustom sensor-enable-mode-line t
  "Show sensor state in mode-line."
  :type 'boolean)

(defcustom sensor-enable-clipboard-sync nil
  "Sync clipboard changes to the kill ring."
  :type 'boolean)

(defcustom sensor-enable-focus-hooks nil
  "Run hooks on focus change signals."
  :type 'boolean)

(defcustom sensor-idle-threshold-ms 3000
  "Pointer dwell threshold for idle indicator."
  :type 'integer)

(defvar sensor--typing nil "Current typing state.")
(defvar sensor--typing-iki 0.0 "Current inter-key interval.")
(defvar sensor--pointer-idle nil "Pointer idle state.")
(defvar sensor--pointer-dwell 0 "Pointer dwell in ms.")
(defvar sensor--focus-window 0 "Focused window ID.")
(defvar sensor--last-seq 0 "Last state sequence.")
(defvar sensor--dbus-registrations nil "Active D-Bus signal registrations.")
(defvar sensor--connected nil "Whether D-Bus connection is active.")

;; ============================================================================
;; Mode-line
;; ============================================================================

(defvar sensor--mode-line-string "")

(defun sensor--update-mode-line ()
  "Recompute the mode-line indicator string."
  (setq sensor--mode-line-string
        (if (not sensor--connected)
            " [sensor:off]"
          (concat
           " ["
           ;; Typing indicator
           (if sensor--typing
               (propertize "T" 'face '(:foreground "#9ece6a" :weight bold))
             (propertize "." 'face '(:foreground "#565f89")))
           ;; Pointer indicator
           (if sensor--pointer-idle
               (propertize "P" 'face '(:foreground "#ff9e64"))
             (propertize "." 'face '(:foreground "#565f89")))
           "]")))
  (force-mode-line-update t))

;; ============================================================================
;; D-Bus signal handlers
;; ============================================================================

(defun sensor--on-typing-changed (is-typing iki)
  "Handle TypingStateChanged signal.
IS-TYPING is 0 or 1, IKI is inter-key interval in ms."
  (setq sensor--typing (not (zerop is-typing))
        sensor--typing-iki iki)
  (when sensor-enable-mode-line
    (sensor--update-mode-line)))

(defun sensor--on-focus-changed (window-id _name)
  "Handle FocusChanged signal.
WINDOW-ID is the X11 window ID."
  (setq sensor--focus-window window-id)
  (when sensor-enable-focus-hooks
    (run-hooks 'sensor-focus-change-hook))
  (when sensor-enable-mode-line
    (sensor--update-mode-line)))

(defun sensor--on-clipboard-changed (_hash _kind)
  "Handle ClipboardChanged signal.
Optionally syncs clipboard content to the Emacs kill ring."
  (when sensor-enable-clipboard-sync
    (let ((content (ignore-errors
                     (shell-command-to-string "xclip -selection clipboard -o 2>/dev/null"))))
      (when (and content (not (string-empty-p content)))
        (kill-new content)
        (message "sensor: clipboard synced (%d bytes)" (length content))))))

(defun sensor--on-pointer-idle (dwell-ms)
  "Handle PointerIdle signal.
DWELL-MS is pointer dwell time in milliseconds."
  (setq sensor--pointer-idle t
        sensor--pointer-dwell dwell-ms)
  (when sensor-enable-mode-line
    (sensor--update-mode-line)))

(defun sensor--on-state-changed (seq)
  "Handle StateChanged signal.
SEQ is the fusion sequence number."
  (setq sensor--last-seq seq)
  ;; Reset pointer idle when state changes (movement detected)
  (when sensor--pointer-idle
    (setq sensor--pointer-idle nil)
    (when sensor-enable-mode-line
      (sensor--update-mode-line))))

;; ============================================================================
;; D-Bus method calls
;; ============================================================================

(defun sensor-hud-message (text category)
  "Post a message to the sensor HUD overlay.
TEXT is the message string, CATEGORY is one of: info, build, e2e,
save, warn, error, test, hook, vault."
  (interactive
   (list (read-string "HUD message: ")
         (completing-read "Category: "
                          '("info" "build" "e2e" "save" "warn"
                            "error" "test" "hook" "vault")
                          nil t nil nil "info")))
  (dbus-call-method :session
                    "org.sensor.Sensor"
                    "/org/sensor/Sensor"
                    "org.sensor.Sensor"
                    "PostMessage"
                    text category))

(defun sensor-get-snapshot ()
  "Retrieve current sensor snapshot as an alist."
  (interactive)
  (let ((result (dbus-call-method :session
                                  "org.sensor.Sensor"
                                  "/org/sensor/Sensor"
                                  "org.sensor.Sensor"
                                  "GetSnapshot")))
    (when (called-interactively-p 'interactive)
      (message "sensor snapshot: %S" result))
    result))

(defun sensor-get-dynamics ()
  "Retrieve current pointer dynamics."
  (interactive)
  (let ((result (dbus-call-method :session
                                  "org.sensor.Sensor"
                                  "/org/sensor/Sensor"
                                  "org.sensor.Sensor"
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

(defvar sensor-focus-change-hook nil
  "Hook run when the focused X11 window changes.
The variable `sensor--focus-window' holds the new window ID.")

;; ============================================================================
;; Connection management
;; ============================================================================

(defun sensor--register-signals ()
  "Register all D-Bus signal handlers."
  (let ((svc "org.sensor.Sensor")
        (path "/org/sensor/Sensor")
        (iface "org.sensor.Sensor"))
    (push (dbus-register-signal :session svc path iface
                                "TypingStateChanged"
                                #'sensor--on-typing-changed)
          sensor--dbus-registrations)
    (push (dbus-register-signal :session svc path iface
                                "FocusChanged"
                                #'sensor--on-focus-changed)
          sensor--dbus-registrations)
    (push (dbus-register-signal :session svc path iface
                                "ClipboardChanged"
                                #'sensor--on-clipboard-changed)
          sensor--dbus-registrations)
    (push (dbus-register-signal :session svc path iface
                                "PointerIdle"
                                #'sensor--on-pointer-idle)
          sensor--dbus-registrations)
    (push (dbus-register-signal :session svc path iface
                                "StateChanged"
                                #'sensor--on-state-changed)
          sensor--dbus-registrations)))

(defun sensor--unregister-signals ()
  "Unregister all D-Bus signal handlers."
  (dolist (reg sensor--dbus-registrations)
    (dbus-unregister-object reg))
  (setq sensor--dbus-registrations nil))

;; ============================================================================
;; Minor mode
;; ============================================================================

(defvar sensor-mode-map (make-sparse-keymap)
  "Keymap for `sensor-mode'.")

;;;###autoload
(define-minor-mode sensor-mode
  "Global minor mode for sensor integration.
Connects to the sensor daemon on the session bus and
provides real-time mode-line indicators for typing state and
pointer activity."
  :global t
  :lighter sensor--mode-line-string
  :keymap sensor-mode-map
  (if sensor-mode
      (condition-case err
          (progn
            (sensor--register-signals)
            (setq sensor--connected t)
            (sensor--update-mode-line)
            (message "sensor-mode enabled"))
        (dbus-error
         (setq sensor--connected nil)
         (sensor--update-mode-line)
         (message "sensor-mode: D-Bus connection failed: %s" (error-message-string err))))
    (sensor--unregister-signals)
    (setq sensor--connected nil
          sensor--typing nil
          sensor--pointer-idle nil)
    (sensor--update-mode-line)
    (message "sensor-mode disabled")))

(provide 'sensor)
;;; sensor.el ends here
