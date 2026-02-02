;;; calibre-widgets.el --- Widgets for editing book metadata.  -*- lexical-binding: t; -*-
;; Copyright (C) 2023  Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file contains widgets for use in editing book metadata.

;;; Code:
(require 'calibre-core)

(define-widget 'calibre-author 'string
  "A author widget.
It reads an Author's name from an editable text field."
  :completions calibre-authors-completion-table
  :format "%{%t%}: %v"
  :tag "Author")

(define-widget 'calibre-publisher 'string
  "A publisher widget.
It reads a Publisher's name from an editable text field."
  :completions calibre-publishers-completion-table
  :tag ""
  :format "%{%t%}: %v")

(define-widget 'calibre-series 'string
  "A series widget.
It reads a series's name from an editable text field."
  :completions calibre-series-completion-table
  :tag "Series"
  :format "%{%t%}: %v")

(define-widget 'calibre-tag 'string
  "A tag widget.
It reads a tag from an editable text field."
  :completions calibre-tags-completion-table
  :tag "Tag"
  :format "%t %v")

(define-widget 'calibre-date 'vector
  "A date widget.
It reads a year, month, and day."
  :value-to-internal (lambda (_widget value)
                       (message "%s" value)
                       (format-time-string "%F" value))
  :value-to-external (lambda (_widget value)
                       (message "to-ext: %s" value)
                       (seq-let (year month day) value
                         (parse-iso8601-time-string (format "%s-%s-%sT00:00:00" year month day))))
  :convert-widget (lambda (widget)
                    (widget-put widget :args '(vector (integer :tag "Year" :format "%t:  %v")
                                                      (integer :tag "Month" :format "%t: %v")
                                                      (integer :tag "Day" :format "%t:   %v")))
                    widget))

(provide 'calibre-widgets)
;;; calibre-widgets.el ends here
