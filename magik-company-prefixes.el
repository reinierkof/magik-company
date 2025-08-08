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
(require 'magik-session)
(defvar magik-company-cur-prefix nil)
(defvar magik-company-prefix-at-methods nil)
(defvar magik-company-prefix-at-conditions nil)
(defvar magik-company-prefix-at-dynamics nil)
(defvar magik-company-prefix-at-globals nil)
(defvar magik-company-prefix-at-objects nil)
(defvar magik-company-prefix-at-slot nil)

(defun magik-company--determine-cur-prefix()
  "The prefix from recent characters."
  (let ((start (line-beginning-position))
	(end (point))
	(regex "[^a-zA-Z0-9:_!<^]+")
	result)
    (save-excursion
      (if (re-search-backward regex start t)
	  (setq result (buffer-substring-no-properties (+ 1 (point)) end))
	(setq result (buffer-substring-no-properties start end))))
    (setq magik-company-cur-prefix (downcase result))
    magik-company-cur-prefix))

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
  "Detect if the point is at .variable ."
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
    (if (re-search-backward "\\b\\(\\sw+\\)\\_>" (line-beginning-position) t)
	(let ((start (match-beginning 0)))
	  (if (and start
		   (> start (point-min))
		   (not (eq (char-before start) ?.)))
	      t
	    nil))
      nil)))

(defun magik-company--at-object-prefix ()
  "Detect if the point is at a possible object.
Allows for single words or two words connected with a ':'."
  (save-excursion
    (if (or
	 (re-search-backward "\\b\\(\\sw+\\):\\(\\sw+\\)\\_>" (line-beginning-position) t)
	 (re-search-backward "\\b\\(\\sw+\\)\\_>" (line-beginning-position) t))
	(let ((start (match-beginning 0)))
	  (if (and start
		   (> start (point-min))
		   (not (eq (char-before start) ?.)))
	      t
	    nil))
      nil)))

(defun magik-company--session-within-typeable-area()
  "Detect if we are in a command."
  (save-excursion
    (let ((cursor-loc (point)))
      (goto-char (point-max))
      (let ((magik-prefix-pos (save-excursion (when (re-search-backward magik-session-prompt nil t)
						(match-end 0)))))
        (if (and (number-or-marker-p magik-prefix-pos)
                 (>= cursor-loc magik-prefix-pos))
            t
        nil)))))

(provide 'magik-company-prefixes)
;;; magik-company-prefixes.el ends here
