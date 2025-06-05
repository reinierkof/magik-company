;;; magik-company-buffer-cache.el --- Contains the methods to retrieve the parameters & slots from the current buffer  -*- lexical-binding: t; -*-

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
(require 'magik-mode)
(require 'magik-company-treesitter-extras)

(defvar magik-company--params-cache nil)
(defvar magik-company--variables-cache nil)
(defvar magik-company--slots-cache nil)
(defvar magik-company--classname-cache nil)

(defvar magik-company--last-line-number 0)

(defun magik-company--update-buffer-caches ()
  "Update all local buffer caches."
  (when (/= magik-company--last-line-number (line-number-at-pos))
    (setq magik-company--last-line-number (line-number-at-pos))
    (magik-company--load-params-cache)
    (magik-company--load-variables-cache)
    (magik-company--load-slots-cache)
    (magik-company--load-classname-cache)))

(defun magik-company--load-params-cache ()
  "Load method parameters into the cache and add the parameter kind property."
  (setq magik-company--params-cache
	(mapcar (lambda (el) (propertize el 'kind 'parameter))
		(magik-company--method-parameters))))

(defun magik-company--load-variables-cache ()
  "Load local variables into the cache and add the variable kind property."
  (setq magik-company--variables-cache
	(mapcar (lambda (el) (propertize el 'kind 'variable))
		(magik-company--local-variables))))

(defun magik-company--load-slots-cache ()
  "Load exemplar slots into the cache and add the slot kind property."
  (setq magik-company--slots-cache
	(mapcar (lambda (el) (propertize el 'kind 'slot))
		(magik-company--exemplar-slots))))

(defun magik-company--load-classname-cache ()
  "Load the previous class name into the cache and add the exemplar kind property."
  (setq magik-company--classname-cache
	(mapcar (lambda (el) (propertize el 'kind 'exemplar))
		(list (magik-yasnippet-prev-class-name)))))

(defun magik-company--local-variables ()
  "Gather local variables in the current scope."
  (magik-company--ts-variables-in-scope))

(defun magik-company--method-parameters ()
  "Get local buffer parameters from nearest function."
  (let ((method-regex (cdr (assoc "method-with-arguments" magik-regexp)))
	(assignment-regex (cdr (assoc "assignment-method" magik-regexp)))
	params method-loc)
    (save-excursion
      (when (re-search-backward "\\(_method\\)" nil t)
	(setq method-loc (match-beginning 0))))
    (when method-loc
      (save-excursion
	(setq params (or (when (search-backward-regexp method-regex (- method-loc 20) t)
			   (match-string 1))
			 (when (search-backward-regexp assignment-regex (- method-loc 20) t)
			   (match-string 1))))))
    (when params
      (setq params (mapcar #'string-trim (split-string params "," t)))
      (setq params
            (cl-loop for param in params
                     if (or (string-prefix-p "_optional" param)
                            (string-prefix-p "_gather" param))
                     collect (nth 1 (split-string param " "))
                     else
                     collect param)))
      params))

(defun magik-company--exemplar-slots ()
  "Retrieve the slots from a exemplar or mixin."
  (let ((slots '())
        (exemplar-data (magik-company--ts-current-exemplar-node-with-locs)))
    (when (alist-get :node exemplar-data)
      (let ((slotted-loc (alist-get :start exemplar-data))
            (dollar-loc (alist-get :end exemplar-data)))
        (save-excursion
	  (goto-char slotted-loc)
	  (while (re-search-forward "{\\s-*:\\(\\sw+\\)\\s-*,\\s-*\\(_unset\\)\\s-*" dollar-loc t)
	    (push (match-string 1) slots)))))
    slots))

(provide 'magik-company-buffer-cache)
;;; magik-company-buffer-cache.el ends here
