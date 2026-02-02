;;; window-mgmt.el --- shackle, popper, winner, ace-window -*- lexical-binding: t; -*-
(use-package ace-window
  :demand t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package shackle
  :demand t
  :config
  (setq shackle-rules
        '(("*Help*" :select t :align below :size 0.33)
          ("*Warnings*" :align below :size 0.25)
          ("*compilation*" :select nil :align below :size 0.25)
          ("*Messages*" :select nil :align below :size 0.25)
          ("*Completions*" :align below :size 0.3)
          ("*Backtrace*" :select t :align below :size 0.4)
          ("*Calendar*" :select t :align below :size 0.3)
          ("\\*vterm.*" :regexp t :select t :align below :size 0.4)
          ("*bw-calendar*" :select t :align right :size 0.4)
          ("*bw-agenda*" :select t :align right :size 0.4)
          ("*Org Agenda*" :select t :align right :size 0.5)
          ("\\*Dirvish.*" :regexp t :select t :align left :size 0.4)))
  (shackle-mode 1))

(use-package popper
  :demand t
  :bind (("C-\`" . popper-toggle)
         ("M-\`" . popper-cycle)
         ("C-M-\`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*compilation\\*"
          "\\*Completions\\*"
          "\\*Help\\*"
          "\\*Backtrace\\*"
          "\\*bw-calendar\\*"
          "\\*bw-agenda\\*"
          "\\*vdirsyncer\\*"
          help-mode
          compilation-mode))
  :config
  (popper-mode 1))

(winner-mode 1)
