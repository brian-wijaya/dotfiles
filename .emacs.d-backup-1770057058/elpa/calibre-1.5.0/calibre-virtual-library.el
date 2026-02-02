;;; calibre-virtual-library.el --- Virtual library support -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; This file is part of calibre.el.

;; calibre.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; calibre.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with calibre.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file provides virtual library support for calibre.el.  A
;; virtual library is a named set of predefined search filters.

;;; Code:
(require 'calibre-core)

(defcustom calibre-virtual-libraries nil
  "A list of predefined filters, i.e. Virtual Libraries.
Each element is a cons cell (name . filters) where name is a
string, and filters is a list of vectors [op field value]"
  :type (let ((fields '(choice :tag "Field"
                               (const :tag "Title" title)
                               (const :tag "Author" author)
                               (const :tag "Publisher" publisher)
                               (const :tag "Series" series)
                               (const :tag "Tag" tag)
                               (const :tag "Format" format))))
          `(repeat :tag "Virtual Libraries"
                 (cons :tag "Virtual Library"
                       (string :tag "Name")
                       (repeat :tag "Filters"
                               (vector :tag "Filter"
                                       (choice :tag "Operation"
                                               (const :tag "Include" +)
                                               (const  :tag "Exclude" -))
                                       (choice :tag "Type"
                                               (list :tag "Basic"
                                                       :inline t
                                                       ,fields
                                                       (string :tag "Value"))
                                               (repeat :tag "Composite"
                                                       (vector :tag "Filter"
                                                               ,fields
                                                               (string :tag "Value")))))))))
  :group 'calibre
  :package-version '("calibre" . "1.1.0"))

(defun calibre-select-virtual-library (arg &optional virtual-library)
  "Prompt the user to select a virtual library.
If called with a prefix argument clear the active virtual library."
  (interactive "P")
  (if arg
      (calibre-library-clear-filters)
    (setf calibre-library--filters
          (alist-get
           (if virtual-library
               virtual-library
             (completing-read "Virtual Library: "
                              (mapcar #'car calibre-virtual-libraries)
                              nil
                              t))
           calibre-virtual-libraries nil nil #'string=)))
  (calibre-library--refresh t))

(provide 'calibre-virtual-library)
;;; calibre-virtual-library.el ends here
