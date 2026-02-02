;;; email.el --- mu4e email config -*- lexical-binding: t; -*-
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(use-package mu4e
  :ensure nil
  :commands (mu4e)
  :config
  (setq mu4e-maildir "~/Mail"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval nil
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-attachment-dir "~/Downloads"
        mu4e-change-filenames-when-moving t
        message-kill-buffer-on-exit t
        mu4e-confirm-quit nil
        mu4e-headers-date-format "%Y-%m-%d"
        mu4e-headers-time-format "%H:%M"
        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:from . 22)
                              (:subject)))

  (setq mu4e-contexts
        (list
         ;; Account 1: wijaya193@hotmail.com (MOST IMPORTANT)
         (make-mu4e-context
          :name "wijaya193"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/hotmail-wijaya193/" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "wijaya193@hotmail.com")
                  (user-full-name . "Brian Huai")
                  (mu4e-drafts-folder . "/hotmail-wijaya193/Drafts")
                  (mu4e-sent-folder . "/hotmail-wijaya193/Sent")
                  (mu4e-trash-folder . "/hotmail-wijaya193/Deleted")
                  (mu4e-refile-folder . "/hotmail-wijaya193/Archive")
                  (smtpmail-smtp-server . "smtp.office365.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type . starttls)))

         ;; Account 2: bw1animation@gmail.com (SECOND MOST IMPORTANT)
         (make-mu4e-context
          :name "bw1animation"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail-bw1/" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "bw1animation@gmail.com")
                  (user-full-name . "Brian Huai")
                  (mu4e-drafts-folder . "/gmail-bw1/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/gmail-bw1/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/gmail-bw1/[Gmail]/Trash")
                  (mu4e-refile-folder . "/gmail-bw1/[Gmail]/All Mail")))

         ;; Account 3: parampathonmon2@gmail.com
         (make-mu4e-context
          :name "parampathonmon2"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail-parampathonmon2/" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "parampathonmon2@gmail.com")
                  (user-full-name . "Brian Huai")
                  (mu4e-drafts-folder . "/gmail-parampathonmon2/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/gmail-parampathonmon2/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/gmail-parampathonmon2/[Gmail]/Trash")
                  (mu4e-refile-folder . "/gmail-parampathonmon2/[Gmail]/All Mail")))

         ;; Account 4: sarampathonmon3@gmail.com
         (make-mu4e-context
          :name "sarampathonmon3"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail-sarampathonmon3/" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "sarampathonmon3@gmail.com")
                  (user-full-name . "Brian Huai")
                  (mu4e-drafts-folder . "/gmail-sarampathonmon3/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/gmail-sarampathonmon3/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/gmail-sarampathonmon3/[Gmail]/Trash")
                  (mu4e-refile-folder . "/gmail-sarampathonmon3/[Gmail]/All Mail")))

         ;; Account 5: museumnumberfour@gmail.com
         (make-mu4e-context
          :name "museumnumberfour"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail-museumnumberfour/" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "museumnumberfour@gmail.com")
                  (user-full-name . "Brian Huai")
                  (mu4e-drafts-folder . "/gmail-museumnumberfour/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/gmail-museumnumberfour/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/gmail-museumnumberfour/[Gmail]/Trash")
                  (mu4e-refile-folder . "/gmail-museumnumberfour/[Gmail]/All Mail")))

         ;; Account 6: withinearshotaudio@gmail.com
         (make-mu4e-context
          :name "withinearshotaudio"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail-withinearshotaudio/" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "withinearshotaudio@gmail.com")
                  (user-full-name . "Brian Huai")
                  (mu4e-drafts-folder . "/gmail-withinearshotaudio/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/gmail-withinearshotaudio/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/gmail-withinearshotaudio/[Gmail]/Trash")
                  (mu4e-refile-folder . "/gmail-withinearshotaudio/[Gmail]/All Mail")))

         ;; Account 7: parameshransani5@gmail.com
         (make-mu4e-context
          :name "parameshransani5"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail-parameshransani5/" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "parameshransani5@gmail.com")
                  (user-full-name . "Brian Huai")
                  (mu4e-drafts-folder . "/gmail-parameshransani5/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/gmail-parameshransani5/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/gmail-parameshransani5/[Gmail]/Trash")
                  (mu4e-refile-folder . "/gmail-parameshransani5/[Gmail]/All Mail")))

         ;; Account 8: srichnanrahn3@gmail.com
         (make-mu4e-context
          :name "srichnanrahn3"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail-srichnanrahn3/" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "srichnanrahn3@gmail.com")
                  (user-full-name . "Brian Huai")
                  (mu4e-drafts-folder . "/gmail-srichnanrahn3/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/gmail-srichnanrahn3/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/gmail-srichnanrahn3/[Gmail]/Trash")
                  (mu4e-refile-folder . "/gmail-srichnanrahn3/[Gmail]/All Mail")))

         ;; Account 9: brianhuaidesign@gmail.com (LEAST IMPORTANT)
         (make-mu4e-context
          :name "brianhuaidesign"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail/" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "brianhuaidesign@gmail.com")
                  (user-full-name . "Brian Huai")
                  (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/gmail/[Gmail]/Trash")
                  (mu4e-refile-folder . "/gmail/[Gmail]/All Mail")))))

  (setq mu4e-context-policy 'pick-first)

  ;; Default SMTP settings (for Gmail accounts)
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-auth-credentials "~/.authinfo"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls))
