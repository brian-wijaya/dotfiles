;;; init.el -*- lexical-binding: t; -*-
;;
;; ⛔ WARNING TO AI AGENTS ⛔
;; NEVER use symlinks (stow/chezmoi) for dotfiles. They caused catastrophic
;; data loss. This file is managed via DIRECT COPY to ~/dotfiles/

;; Profile selector
;; Options: 'default-vanilla, 'default-doom, 'bw-vanilla, 'bw-doom
(defvar bw/active-profile 'bw-doom)

(defun switch-profile ()
  "Switch profile by changing bw/active-profile in init.el and reloading."
  (interactive)
  (let* ((profiles '(("DEFAULT-VANILLA (stock Emacs)" . default-vanilla)
                     ("DEFAULT-DOOM (Doom defaults)" . default-doom)
                     ("BW-VANILLA (your C-c custom)" . bw-vanilla)
                     ("BW-DOOM (Doom + your commands)" . bw-doom)))
         (choice (completing-read "Profile: " (mapcar #'car profiles) nil t))
         (sym (cdr (assoc choice profiles))))
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/init.el")
      (goto-char (point-min))
      (when (re-search-forward "^(defvar bw/active-profile '.*)" nil t)
        (replace-match (format "(defvar bw/active-profile '%s)" sym)))
      (write-file "~/.emacs.d/init.el"))
    (message "Switched to %s - restart Emacs for full effect" choice)))

;; Custom file (keep out of init.el)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t t))

;; Global guards (all profiles)
(load (expand-file-name "modules/memory-protection" user-emacs-directory))

;; Somatic sensor integration (requires somatic-dbus running)
(require 'somatic nil t)
(when (featurep 'somatic)
  (somatic-mode 1))

;; Load profile
(let ((profile-file (expand-file-name
                     (format "profiles/%s.el" bw/active-profile)
                     user-emacs-directory)))
  (if (file-exists-p profile-file)
      (load profile-file)
    (error "Profile not found: %s" profile-file)))
