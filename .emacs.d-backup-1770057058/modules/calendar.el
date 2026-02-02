;;; calendar.el --- calfw and khal integration -*- lexical-binding: t; -*-
(use-package calfw :demand t)
(use-package calfw-ical :demand t :after calfw)

(defun bw/calendar ()
  "Display calendar output in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*bw-calendar*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (call-process "khal" nil buf nil "calendar")
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

(defun bw/agenda ()
  "Display upcoming events."
  (interactive)
  (let ((buf (get-buffer-create "*bw-agenda*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (call-process "khal" nil buf nil "list" "today" "30d")
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

(defun bw/calendar-sync ()
  "Sync calendars using vdirsyncer."
  (interactive)
  (message "Syncing calendars...")
  (async-shell-command "vdirsyncer sync" "*vdirsyncer*"))
