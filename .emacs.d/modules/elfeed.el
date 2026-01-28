;;; elfeed.el --- RSS feed reader -*- lexical-binding: t; -*-

(use-package elfeed
  :ensure t
  :commands elfeed
  :config
  (setq elfeed-feeds
        '(;; Tech news (high signal)
          ("https://hnrss.org/frontpage?points=100" tech hn)
          ("https://lobste.rs/rss" tech lobsters)

          ;; Languages/tools you use
          ("https://blog.rust-lang.org/feed.xml" rust lang)
          ("https://go.dev/blog/feed.atom" go lang)

          ;; Emacs
          ("https://planet.emacslife.com/atom.xml" emacs)))

  ;; Show entries from last 2 weeks
  (setq elfeed-search-filter "@2-weeks-ago +unread")

  ;; Auto-fetch on open
  (add-hook 'elfeed-search-mode-hook 'elfeed-update)

  ;; Mark all as read with 'R'
  (define-key elfeed-search-mode-map (kbd "R")
    (lambda () (interactive)
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread))))

(provide 'elfeed)
