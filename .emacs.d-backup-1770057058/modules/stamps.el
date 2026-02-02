;;; stamps.el --- timestamp macros -*- lexical-binding: t; -*-
(defvar bw/timestamp-regexp
  "[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}_[0-9]\\{4\\}\\.[0-9]\\{2\\}\\.\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\)"
  "Regex matching timestamp format YY-MM-DD_HHMM.SS.Day")

(defun bw/timestamp-bounds ()
  "Return (start . end) of timestamp at/near point, or nil."
  (save-excursion
    (let ((orig (point)) (bol (line-beginning-position)) (eol (line-end-position)))
      (cond
       ((looking-at bw/timestamp-regexp)
        (cons (match-beginning 0) (match-end 0)))
       ((and (re-search-backward bw/timestamp-regexp bol t)
             (<= orig (match-end 0)))
        (cons (match-beginning 0) (match-end 0)))
       ((and (goto-char orig)
             (re-search-forward bw/timestamp-regexp eol t)
             (>= orig (match-beginning 0)))
        (cons (match-beginning 0) (match-end 0)))))))

(defun bw/stamp-todo ()
  "Insert TODO: stamp."
  (interactive)
  (insert "TODO: "))

(defun bw/stamp-should-be ()
  "Insert SHOULD BE: stamp."
  (interactive)
  (insert "SHOULD BE: "))

(defun bw/stamp-timestamp ()
  "Insert timestamp."
  (interactive)
  (insert (format-time-string "%y-%m-%d_%H%M.%S.%a")))
