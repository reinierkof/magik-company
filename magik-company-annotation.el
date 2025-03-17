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
  (when magik-company-show-params-annotation
    (let ((args nil))
      (setq args (nconc args (magik-company--annotation-required-params candidate)
			(magik-company--annotation-optional-params candidate)
			(magik-company--annotation-gather-param candidate)))
      (when args
	  (concat "<" (string-join args ", ") ">")))))

(defun magik-company--annotation-required-params(candidate)
  "Retrieve the arguments text property from CANDIDATE, leave space for operations."
  (let ((params (get-text-property 0 'arguments candidate)))
      params
      ))

(defun magik-company--annotation-optional-params(candidate)
  "Retrieve the optional arguments text property from CANDIDATE, make italic."
  (when magik-company-show-optional-params-annotation
    (let ((params (get-text-property 0 'optional candidate)))
      (mapcar (lambda (a-param)
		   (propertize a-param 'face '(:slant italic)))
	      params))))

(defun magik-company--annotation-gather-param(candidate)
  "Retrieve the gather arguments text property from CANDIDATE, make italic."
  (when magik-company-show-gather-param-annotation
    (let ((params (get-text-property 0 'gather candidate)))
      (mapcar (lambda (a-param)
		   (propertize a-param 'face '(:slant italic)))
	      params))))

(provide 'magik-company-annotation)
;;; magik-company-annotation.el ends here
