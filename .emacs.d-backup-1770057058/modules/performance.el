;;; performance.el --- performance tuning -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 256 1024 1024))
(setq read-process-output-max (* 4 1024 1024))
(setq vc-handled-backends '(Git))
(setq frame-inhibit-implied-resize t)

(when (featurep 'native-compile)
  (setq native-comp-async-jobs-number 8
        native-comp-deferred-compilation t
        native-comp-async-report-warnings-errors nil))

(setq scroll-conservatively 0
      scroll-step 1
      scroll-margin 0
      scroll-preserve-screen-position t
      scroll-error-top-bottom t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      next-screen-context-lines 2
      fast-but-imprecise-scrolling nil
      redisplay-skip-fontification-on-input nil)

(setq menu-bar-update-hook nil)
