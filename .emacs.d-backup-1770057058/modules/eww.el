;;; eww.el --- EWW browser config -*- lexical-binding: t; -*-
(setq eww-search-prefix "https://www.google.com/search?q=")
(setq eww-home-url "file:///home/bw/.emacs.d/docs-home.html")

(defun docs-home ()
  "Open documentation index in EWW."
  (interactive)
  (eww-open-file "~/.emacs.d/docs-home.html"))
