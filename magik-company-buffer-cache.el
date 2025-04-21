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

(defvar magik-company--params-cache nil)
(defvar magik-company--variables-cache nil)
(defvar magik-company--slots-cache nil)
(defvar magik-company--classname-cache nil)

(defvar magik-company--last-line-number 0)

(defun magik-company--update-buffer-caches ()
  "docs."
  (when (/= magik-company--last-line-number (line-number-at-pos))
    (setq magik-company--last-line-number (line-number-at-pos))
    (magik-company--load-params-cache)
    (magik-company--load-variables-cache)
    (magik-company--load-slots-cache)
    (magik-company--load-classname-cache)
    ))

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
  (let ((variables '())
	(scopes (magik-company--scope-locations)))
    (message "%s" scopes)
    (when scopes
      (save-excursion
	(goto-char (nth 1 scopes))
	(while (re-search-forward "\\(\\sw+\\)\\s-*<<" (nth 2 scopes) t)
	  (cl-pushnew (match-string 1) variables))))
    variables))

(defun magik-company--scope-locations()
  "Closest scope start and end locations, can be block proc or method."
  (let ((scope-start-loc
	 (find-position #'re-search-backward #'max "_method" "_block" "_proc"))
	(scope-end-loc
	 (find-position #'re-search-forward #'min "_endmethod" "_endblock" "_endproc")))
    (if (or (= scope-start-loc (point))
	    (= scope-end-loc (point)))
	nil
      (list t scope-start-loc scope-end-loc)
      )))

(defun find-position (search-fn agg-fn &rest patterns)
  "Does a couple re-searches and if found takes the aggregate function of it.
In my case used for min and max"
  (save-excursion
    (let ((positions
	   (delq nil (mapcar (lambda (pat)
			       (save-excursion
				 (when (funcall search-fn pat nil t)
				   (match-beginning 0))))
			     patterns))))
      (if positions
	  (apply agg-fn positions)
	(point)))))

(defun magik-company--method-parameters ()
  ""
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
			   (match-string 1)
			   )))))
    (when params
      (setq params (mapcar #'string-trim (split-string params "," t)))
      (setq params (cl-remove-if (lambda (param)
				   (or (string-suffix-p "_optional" param)
				       (string-suffix-p "_gather" param)))
				 params)))

    params))

(defun magik-company--exemplar-slots ()
  "Retrieve the slots from a exemplar or mixin."
  (let ((slots '())
	slotted-loc dollar-loc)
    (save-excursion
      (setq slotted-loc (or (when (re-search-backward "\\(def_slotted_exemplar\\)" nil t)
			      (match-beginning 0))
			    (when (re-search-backward "\\(def_mixin\\)" nil t)
			      (match-beginning 0))
			    (when (re-search-backward "\\(def_indexed_exemplar\\)" nil t)
			      (match-beginning 0))))
      (when slotted-loc
	(goto-char slotted-loc)
	(setq dollar-loc (when (re-search-forward "\\(\\$\\)" nil t)
			   (match-beginning 0)))
	(when dollar-loc
	  (goto-char slotted-loc)
	  (while (re-search-forward "{\\s-*:\\(\\sw+\\)\\s-*,\\s-*\\(_unset\\)\\s-*}" dollar-loc t)
	    (push (match-string 1) slots))
	  )))
    slots))

(provide 'magik-company-buffer-cache)
;;; magik-company-buffer-cache.el ends here
