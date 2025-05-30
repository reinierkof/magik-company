;;; magik-company-prefixes.el --- This file contains the prefix functions to determine whether the user is at a certain prefix or not.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:  <reinier.koffijberg@keronic.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defvar magik-company-cur-prefix nil)
(defvar magik-company-prefix-at-methods nil)
(defvar magik-company-prefix-at-conditions nil)
(defvar magik-company-prefix-at-dynamics nil)
(defvar magik-company-prefix-at-globals nil)
(defvar magik-company-prefix-at-objects nil)
(defvar magik-company-prefix-at-slot nil)

(defun magik-company--prefix ()
  "Prefix and what to load based on those the current point."
  (if (or (magik-company--in-comment)
	  (magik-company--in-string))
      (setq magik-company-prefix-at-methods nil
	    magik-company-prefix-at-conditions nil
	    magik-company-prefix-at-dynamics nil
	    magik-company-prefix-at-globals nil
	    magik-company-prefix-at-slot nil
	    magik-company-prefix-at-objects nil)
    (progn
      (setq magik-company-prefix-at-methods (magik-company--at-method-prefix)
	    magik-company-prefix-at-conditions (magik-company--at-raise-condition-prefix)
	    magik-company-prefix-at-dynamics (magik-company--at-dynamic-prefix)
	    magik-company-prefix-at-globals (magik-company--at-global-prefix)
	    magik-company-prefix-at-slot (magik-company--at-slot-prefix)
	    magik-company-prefix-at-objects (magik-company--at-object-prefix)))
    (magik-company--determine-cur-prefix)))

(defun magik-company--determine-cur-prefix()
  "The prefix from recent characters."
  (let ((start (line-beginning-position))
	(end (point))
	(regex "[^a-zA-Z0-9:_!<^]+")
	result)
    (save-excursion
      (if (re-search-backward regex start t)
	  (setq result (buffer-substring-no-properties (+ 1 (point)) end))
	(setq result (buffer-substring-no-properties start end)))
      )
    (setq magik-company-cur-prefix (downcase result))
    magik-company-cur-prefix
    )
  )

(defun magik-company--in-comment ()
  "Check if the current line start is with #, ignore whitespaces."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*#")))

(defun magik-company--in-string ()
  "Check if the current point is inside double quotes (\") or single quotes (')."
  (let ((syntax (syntax-ppss)))
    (nth 3 syntax)))

(defun magik-company--at-method-prefix ()
  "Detect if point is at . method point."
  (save-excursion
    (re-search-backward "\\(_self\\|_clone\\|\\S-\\)\\.\\(\\sw+\\)\\=" (line-beginning-position) t)))

(defun magik-company--at-raise-condition-prefix ()
  "Detect if the point is at condition.raise(: ."
  (save-excursion
    (if (re-search-backward "condition\\.raise(\\s-*:\\(\\sw+\\)\\=" (line-beginning-position) t)
	t
      nil)))

(defun magik-company--at-slot-prefix ()
  "Detect if the point is at .vari"
  (save-excursion
    (if (re-search-backward "\\(?:^\\|[^[:word:].]\\)\\.\\w*\\="
			    (line-beginning-position) t)
	t
      nil)))

(defun magik-company--at-dynamic-prefix ()
  "Detect if the point is at a !..! dynamic prefix."
  (save-excursion
    (if (and (re-search-backward "\\Sw\\(!\\sw*\\)\\=" (line-beginning-position) t)
	     (not (eq (following-char) ?.))
	     (not (equal ":" (buffer-substring-no-properties (match-beginning 1) (1+ (match-beginning 1))))))
	t
      nil)))

(defun magik-company--at-global-prefix ()
  "Detect if the point is at a possible global."
  (save-excursion
    (if (re-search-backward "^\\s-*\\(\\sw+\\)\\=" (line-beginning-position) t)
	t
      nil)))

(defun magik-company--at-object-prefix ()
  "Detect if the point is at a possible object.
Allows for single words or two words connected with a ':'."
  (save-excursion
    (if (or
	 (re-search-backward "\\b\\(\\sw+\\):\\(\\sw+\\)\\=" (line-beginning-position) t)
	 (re-search-backward "\\b\\(\\sw+\\)\\=" (line-beginning-position) t))
	(not (or (eq (following-char) ?.)
		 (save-excursion
		   (goto-char (match-beginning 0))
		   (re-search-backward "\\." (line-beginning-position) t))))
      nil)))

(provide 'magik-company-prefixes)
;;; magik-company-prefixes.el ends here
