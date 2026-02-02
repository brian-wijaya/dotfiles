;;; which-key-posframe.el --- which-key + posframe -*- lexical-binding: t; -*-
(use-package which-key
  :demand t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.03
        which-key-idle-secondary-delay 0.03
        which-key-separator ":"
        which-key-prefix-prefix ""
        which-key-add-column-padding 2
        which-key-show-early-on-C-h t
        echo-keystrokes 0.01))

(use-package posframe
  :demand t)

(use-package which-key-posframe
  :demand t
  :after (which-key posframe)
  :config
  (setq which-key-max-display-columns 5
        which-key-max-description-length 35)

  (defun bw/which-key-posframe--show-buffer (act-popup-dim)
    "Show which-key posframe above modeline, full width with margins."
    (when (posframe-workable-p)
      (let* ((buf (get-buffer " *which-key*"))
             (height (car act-popup-dim))
             (max-height (min height 15)))
        (when buf
          (posframe-show buf
                         :height max-height
                         :min-width (- (frame-width) 8)
                         :poshandler (lambda (info)
                                       (let* ((pw (plist-get info :parent-frame-width))
                                              (ph (plist-get info :parent-frame-height))
                                              (cw (plist-get info :posframe-width))
                                              (ch (plist-get info :posframe-height))
                                              (bottom-margin 130))
                                         (cons (/ (- pw cw) 2)
                                               (- ph ch bottom-margin))))
                         :background-color "#21242b"
                         :foreground-color "#bbc2cf"
                         :border-width 1
                         :border-color "#51afef")))))

  (advice-add 'which-key-posframe--show-buffer
              :override #'bw/which-key-posframe--show-buffer)

  (set-face-attribute 'which-key-posframe nil :background "#21242b")
  (set-face-attribute 'which-key-posframe-border nil :background "#51afef")
  (which-key-posframe-mode 1))
