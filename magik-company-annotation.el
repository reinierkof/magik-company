;;; magik-company-annotation.el --- Contains all the methods to complete the annotation for magik  -*- lexical-binding: t; -*-

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

(defvar magik-company-show-optional-params-annotation t)
(defvar magik-company-show-gather-param-annotation t)
(defvar magik-company-show-params-annotation t)

(defun magik-company--annotation (candidate)
  "Create an annotation based on parameters of CANDIDATE."
  (let ((a-kind (get-text-property 0 'kind candidate))
	(param-functions (list (magik-company--annotation-required-params candidate)
			       (magik-company--annotation-optional-params candidate)
			       (magik-company--annotation-gather-param candidate)))
	result)
    (setq result (if (and a-kind
			  (eq a-kind 'exemplar))
		     (progn (let ((package (get-text-property 0 'package candidate)))
			      (when package
				(prin1-to-string package))))
		   (progn (when magik-company-show-params-annotation
			    (let ((args (delq nil (apply #'append param-functions))))
			      (when args
				(concat "<" (mapconcat #'identity args ", ") ">")))))))
    (when (get-text-property 0 'iter candidate) (setq result (concat "(I) " result)))
    result))

(defun magik-company--annotation-required-params (candidate)
  "Retrieve the arguments text property from CANDIDATE.
Also ensures it is a list of strings."
  (let ((params (get-text-property 0 'arguments candidate)))
    (when (listp params)  ; Ensure it's a list
      (mapcar #'identity params))))  ; Convert to a proper list of strings

(defun magik-company--annotation-optional-params (candidate)
  "Retrieve the optional arguments text property from CANDIDATE.
Making the arguments italic."
  (when magik-company-show-optional-params-annotation
    (let ((params (get-text-property 0 'optional candidate)))
      (when (and (listp params) params)
	(let ((annotated (mapcar (lambda (a-param)
				   (propertize (format "%s" a-param) 'face '(:slant italic)))
				 params)))
	  (setf (nth 0 annotated)
		(propertize (format "_optional %s" (nth 0 annotated))
			    'face '(:slant italic)))
	  annotated)))))

(defun magik-company--annotation-gather-param (candidate)
  "Retrieve the gather arguments text property from CANDIDATE.
Making the arguments italic."
  (when magik-company-show-gather-param-annotation
    (let ((params (get-text-property 0 'gather candidate)))
      (when (and (listp params) params)
	(let ((annotated (mapcar (lambda (a-param)
				   (propertize (format "%s" a-param) 'face '(:slant italic)))
				 params)))
	  (setf (nth 0 annotated)
		(propertize (format "_gather %s" (nth 0 annotated))
			    'face '(:slant italic)))
	  annotated)))))

(provide 'magik-company-annotation)
;;; magik-company-annotation.el ends here
