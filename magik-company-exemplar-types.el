;;; magik-company-exemplar-types.el --- This file contains all the functions which are related to extracting which type a variable might be.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: ;;; magik-company-exemplar-types.el ---  <reinier.koffijberg@RDS>
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

(require 'magik-company-cb-cache)

(defvar magik-company--typed-assignment-patterns
  '(("integer"    "\\s-*<<[ \t\n]*\\([-+]?[0-9]+\\)\\(\\s-+\\|$\\)")
    ("float"      "\\s-*<<[ \t\n]*\\([-+]?[0-9]*\\.[0-9]+\\)")
    ("char16_vector" "\\s-*<<[ \t\n]*\\(\"[^\"]*\"\\)")
    ("simple_vector" "\\s-*<<[ \t\n]*\\({.*\\)"))
  "List of assignment patterns for Magik variables.
Each entry is a double: (TYPE REGEX).")

(defvar magik-company--class-assignment-patterns
  '("\\s-*<<[ \t\n]*\\(\\S-+\\)\\.new")
  )

(defun magik-company--try-method-exemplar-type (variable)
  "Retrieve the exemplar type based on what VARIABLE is."
  (let (exemplar-type)
    (setq exemplar-type
	  (or (magik-company--self-case variable)
	      (magik-company--check-object-source variable)
	      (magik-company--check-typed-assigned-patterns variable)
	      (magik-company--check-class-assigned-patterns variable)
	      (magik-company--check-typed-params variable)))
    exemplar-type))

(defun magik-company--self-case (variable)
  "Return the exemplar type if VARIABLE is '_self' or '_clone'."
  (when (or (equal variable "_self")
	    (equal variable "_clone"))
    (or (cadr (magik-current-method-name))
	(file-name-sans-extension (buffer-name)))))

(defun magik-company--check-object-source (variable)
  "Return VARIABLE if it exists in `magik-company--objects-source-cache`."
  (when (member variable magik-company--objects-source-cache)
    variable))

(defun magik-company--check-typed-assigned-patterns (variable)
  "Return the matching type for VARIABLE based on typed assignment patterns."
  (cl-loop for (type regex) in magik-company--typed-assignment-patterns
	   for match = (magik-company--check-assignment-and-type variable regex type)
	   when match return match))

(defun magik-company--check-class-assigned-patterns (variable)
  "Return the matching type for VARIABLE based on class assignment patterns."
  (cl-loop for regex in magik-company--class-assignment-patterns
	   for match = (let ((combined-regex (concat (regexp-quote variable) regex)))
			 (save-excursion
			   (when (re-search-backward combined-regex nil t)
			     (match-string 1))))
	   when match return match))

(defun magik-company--check-typed-params (variable)
  "Return the method parameter type for VARIABLE."
  (when-let ((method-param-type (magik-company--method-param-type variable)))
    method-param-type))

(defun magik-company--method-param-type (param-name)
  "Search for the param-name in a method comment block and return the type.
PARAM-NAME ..."
  (save-excursion
    (let (start-loc method-loc)
      (setq start-loc (point))
      (setq method-loc (re-search-backward "\\(_method\\)" nil t))
      ;; If _method is found, proceed to search for the @param and type
      (if method-loc
	  (progn
	    (goto-char start-loc)
	    ;; Search for the @param with the given param-name
	    (if (re-search-backward
		 (format "##\\s-*@param\\s-*{\\([^}]+\\)}\\s-*%s" (regexp-quote param-name))
		 method-loc t)
		(match-string 1)
	      nil))))))

(defun magik-company--check-assignment-and-type (variable regex type)
  "Check if VARIABLE matches a REGEX pattern in the buffer.
If matched, return TYPE-OR-CLASS, otherwise nil."
  (save-excursion
    (if (re-search-backward (concat (regexp-quote variable) regex) nil t)
	(progn
	  (if (stringp type)
	      type
	    (progn
	      (message "Warning: type-or-class is not a valid string, it's: %s" type)
	      nil)))
      nil)))

(defun magik-company--exemplar-near-point ()
  "Get current exemplar-type near cursor position."
  (save-excursion
    (save-match-data
      (let ((pt (1- (magik-company--method-point)))
	    variable
	    exemplar)
	(goto-char pt)
	;; Usefully skip over various syntax types:
	(if (not (zerop (skip-syntax-backward "w_().")))
	    (setq variable (buffer-substring-no-properties (point) pt)))
	(if variable
	    (setq exemplar (magik-company--try-method-exemplar-type variable)))
	exemplar))))

(defun magik-company--method-point ()
  "Detect if point is at . method point."
  (save-excursion
    (if (re-search-backward "\\(_self\\|_clone\\|\\S-\\)\\.\\(\\sw+\\)\\=" (line-beginning-position) t)
	(match-beginning 2))))


;; OLD VERSION WITHOUT SPLITTING INTO FUNCTIONS
;; (defun magik-company--try-method-exemplar-type (variable)
;;   "Retrieve the exemplar type based on what VARIABLE is."
;;   (let (exemplar-type)
;;     (setq exemplar-type
;; 	  (cond

;; 	   ;; Self case
;; 	   ((or (equal variable "_self")
;; 	       (equal variable "_clone")
;; 	       (or (cadr (magik-current-method-name))
;; 		   (file-name-sans-extension (buffer-name)))))

;; 	   ;; Check object source
;; 	   ((member variable magik-company--objects-source-cache)
;; 	    variable)

;; 	   ;; Check typed assigned patterns
;; 	   ((cl-loop for (type regex) in magik-company--typed-assignment-patterns
;; 		     for match = (let ((result (magik-company--check-assignment-and-type variable regex type)))
;; 			     result)
;; 		     when match return match))

;; 	   ;; Check class assigned patterns
;; 	   ((cl-loop for regex in magik-company--class-assignment-patterns
;; 		     for match = (let ((combined-regex (concat (regexp-quote variable) regex)))
;; 				   (save-excursion
;; 				     (when (re-search-backward combined-regex nil t)
;;                                        (let ((result (match-string 1)))
;; 					 result))))
;; 		     when match return match))

;; 	   ;; Check typed params (use the stored result)
;; 	    ((when-let ((method-param-type (magik-company--method-param-type variable)))
;; 	      method-param-type))

;; 	    ;; default case
;; 	    (t
;; 	     nil)))
;;     exemplar-type)


(provide 'magik-company-exemplar-types)
;;; magik-company-exemplar-types.el ends here
