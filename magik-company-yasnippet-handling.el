;;; magik-company-yasnippet-handling.el --- Contains the functions related to completing with yasnippets and recognizing whether a candidate is a yasnippet.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:  <reinier.koffijberg@RDS>
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
(require 'yasnippet)
(defvar magik-company-insert-optional-params nil)
(defvar magik-company-insert-gather-param t)
(defvar magik-company-insert-params t)

(defun magik-company--add-yasnippet-text-property(candidate)
  "If there is a snippet for CANDIDATE add a text property."
  (when (and (not (get-text-property 0 'yasnippet candidate))
	     (magik-company--candidate-is-yasnippet candidate))
    (put-text-property 0 (length candidate) 'yasnippet t candidate)))

(defun magik-company--insert-param-yasnippet (list)
  "Insert a param yasnippet from LIST, each param is a tab and ends after the ')'."
  (when list
    (if (eq (char-before) ?\))
	(progn
	  (delete-char -1)
	  (yas-expand-snippet
	   (concat (mapconcat (lambda (param) (format "${%s}" param)) list ", ") ")$0")))
      (progn
	(yas-expand-snippet
	 (concat (concat "(" (mapconcat (lambda (param) (format "${%s}" param)) list ", ") ")$0")))))))

(defun magik-company--candidate-is-yasnippet (key)
  "Get the snippet called KEY in MODE's tables."
  (interactive)
  (let ((yas-choose-tables-first nil)
	(yas-choose-keys-first nil))
    (cl-find key (yas--all-templates
		  (yas--get-snippet-tables major-mode))
	     :key #'yas--template-key :test #'string=)))

(defun magik-company--insert-candidate-yasnippet(candidate)
  "Insert the yasnippet from CANDIDATE as post completion."
  (let ((a-snippet (magik-company--candidate-is-yasnippet candidate)))
    (delete-region (- (point) (length candidate)) (point))
    (yas-expand-snippet a-snippet)))

(defun magik-company--insert-candidate-args-yasnippet(candidate)
  "Check which type of arguments a CANDIDATE has.
Insert them depending on settings."
  (let ((arguments-to-insert nil))
    (when magik-company-insert-params
      (when (magik-company--candidate-is-method candidate)
	(setq arguments-to-insert (nconc arguments-to-insert
					 (get-text-property 0 'arguments candidate)))
	(when magik-company-insert-optional-params
	  (setq arguments-to-insert (nconc arguments-to-insert
					   (get-text-property 0 'optional candidate))))
	(when (and magik-company-insert-gather-param (get-text-property 0 'gather candidate))
	  (setq arguments-to-insert (nconc arguments-to-insert (list "gather"))))
	(magik-company--insert-param-yasnippet arguments-to-insert)))))

(defun magik-company--candidate-is-method(candidate)
  "Check if CANDIDATE is a magik method."
  (let ((a-kind (get-text-property 0 'kind candidate)))
    (or (eq a-kind 'method)
	(eq a-kind 'assign-method)
	(eq a-kind 'global))))

(provide 'magik-company-yasnippet-handling)
;;; magik-company-yasnippet-handling.el ends here
