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
  ""
  (let ((variables '())
	(more-vars t)
	(var-count 1)
	scope-loc endscope-loc)
    (save-excursion
      (setq scope-loc (or (when (re-search-backward "\\(_method\\)" nil t)
			    (match-beginning 0))
			  (when (re-search-backward "\\(_block\\)" nil t)
			  (match-beginning 0))))
      (when scope-loc
	(setq endscope-loc (or (when (re-search-forward "\\(_endmethod\\)" nil t)
				 (match-beginning 0))
			       (when (re-search-forward "\\(_endblock\\)" nil t)
				 (match-beginning 0))))
	(when endscope-loc
	  (while more-vars
	    (goto-char scope-loc)
	    (if (re-search-forward "\\(\\sw+\\)\\s-*<<" endscope-loc t var-count)
		(progn
		  (setq var-count (+ 1 var-count))
		  (cl-pushnew (match-string 1) variables))
	      (setq more-vars nil))
	    ))))
    variables))

(defun magik-company--method-parameters ()
  ""
  (let ((start-loc (point))
	(method-regex (cdr (assoc "method-with-arguments" magik-regexp)))
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
  ""
  (let ((more-slots t)
	(slots '())
	(slot-count 1)
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
	  (while more-slots
	    (goto-char slotted-loc)
		       (if (re-search-forward "{\\s-*:\\(\\sw+\\)\\s-*,\\s-*\\(_unset\\)\\s-*}" dollar-loc t slot-count)
			   (progn
			     (setq slot-count (+ 1 slot-count))
			     (push (match-string 1) slots))
			 (setq more-slots nil))))))
    slots))

(provide 'magik-company-buffer-cache)
;;; magik-company-buffer-cache.el ends here
