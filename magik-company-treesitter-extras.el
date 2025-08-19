;;; magik-company-treesitter-extras.el --- This file contains all the functions to retrieve treesitter values from your current buffer  -*- lexical-binding: t; -*-

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
(require 'treesit)

(defvar magik-company--ts-scope-keywords '("method" "block" "procedure"))
(defvar magik-company--ts-parameterized-scope-keywords '("method" "procedure"))

(defun magik-company--ts-node-type-in-scope (node type)
  "TYPE in a NODE."
  (let ((results '()))
    (when node
      (let ((stack (treesit-node-children node)))
	(while stack
	  (let ((current (pop stack)))
	    (when (string= (treesit-node-type current) type)
	      (push current results))
	    (when (not (member (treesit-node-type current)
			       magik-company--ts-scope-keywords))
	      (setq stack (append (treesit-node-children current) stack)))))))
    (nreverse results)))

(defun magik-company--ts-enclosing-scope (parameterized?)
  "Return the enclosing node of type ts-*-scope-keywords.
When PARAMETERIZED? then only the parameterized keywords"
  (let ((node (treesit-node-at (point)))
	(keyword-list (if parameterized?
			  magik-company--ts-parameterized-scope-keywords
			magik-company--ts-scope-keywords)))
    (while (and node
		(not (member (treesit-node-type node)
			     keyword-list)))
      (setq node (treesit-node-parent node)))
    node))

(defun magik-company--ts-enclosing-method ()
  "Return the enclosing node of type method."
  (let ((node (treesit-node-at (point))))
    (while (and node
		(not (string= (treesit-node-type node) "method")))
      (setq node (treesit-node-parent node)))
    node))

(defun magik-company--ts-exemplar-of-enclosing-method ()
  "Return the exemplar name of the enclosing method."
  (let ((node (magik-company--ts-enclosing-method)))
    (when node
      (let ((results-node
	     (cdr (assoc 'exemplar
			 (treesit-query-capture node "(method exemplarname: (identifier) @exemplar)")))))
	(when results-node
	  (substring-no-properties (treesit-node-text results-node)))))))

(defun magik-company--ts-parameters-in-scope ()
  "Get local buffer parameter from method/proc in scope."
  (let ((parameterized-scope (magik-company--ts-enclosing-scope t)))
    (when parameterized-scope
      (let ((children (treesit-node-children parameterized-scope))
	    (results '())
	    (found nil))
	(dolist (child children)
	  (if (or (string= (treesit-node-text child) "\n")
		  (string= (treesit-node-text child) ")"))
	      (setq found t)
	    (when (and (not found)
		       (string= (treesit-node-type child) "argument"))
	      (push (substring-no-properties (treesit-node-text child)) results))))
	results))))

(defun magik-company--ts-lhs-variables-in-assignment-node(node variables)
  "Lhs variables of an assignment NODE added in VARIABLES list."
  (let ((stack (magik-company--ts-children-before-assignment node)))
    (while stack
      (let ((current (pop stack)))
	(when (string= (treesit-node-type current) "variable")
	  (push (substring-no-properties (treesit-node-text current)) variables))
	(setq stack (append (treesit-node-children current) stack))))
    variables))

(defun magik-company--ts-children-before-assignment (node)
  "Return a list of NODE's children before << sign."
  (let ((children (treesit-node-children node))
	(result '())
	(found nil))
    (dolist (child children)
      (if (string= (treesit-node-text child) "<<")
	  (setq found t)
	(unless found
	  (push child result))))
    (nreverse result)))

(defun magik-company--ts-import-variables-in-scope (node variables)
  "VARIABLES gained by the import statement in NODE."
  (when node
    (let ((stack (treesit-node-children node)))
      (while stack
	(let ((current (pop stack)))
	  (when (string= (treesit-node-type current) "import")
	    (push (substring-no-properties (treesit-node-text
					    (nth 1 (treesit-node-children current))))
		  variables))
	  (when (not (member (treesit-node-type current)
			     magik-company--ts-scope-keywords))
	    (setq stack (append (treesit-node-children current) stack)))))))
  variables)

(defun magik-company--ts-for-loop-variables-in-scope(node variables)
  "VARIABLES gained in the for loop statement in a NODE."
  (when (and (< (treesit-node-start node) (point))
	     (> (treesit-node-end  node) (point)))
    (let ((children (treesit-node-children node))
	  (found nil))
      (dolist (child children)
	(when (string= (treesit-node-text child) "over")
	  (setq found t))
	(when (and (not found)
		   (string= (treesit-node-type child) "identifier"))
	  (push (substring-no-properties (treesit-node-text child)) variables)))))
  variables)

(defun magik-company--ts-for-local-variables-in-scope(node variables)
  "VARIABLES gained in the for loop statement in a NODE."
  (let ((children (treesit-node-children node))
	(found nil))
    (dolist (child children)
      (when (string= (treesit-node-text child) "<<")
	(setq found t))
      (when (and (not found)
		 (string= (treesit-node-type child) "identifier"))
	(push (substring-no-properties (treesit-node-text child)) variables))))
  variables)

(defun magik-company--ts-exemplar-node-in-buffer-for (exemplar-name)
  "Exemplar node in buffer for EXEMPLAR-NAME."
  (cdr (assoc 'exemplar-node (treesit-query-capture
			      (treesit-buffer-root-node)
			      (format
			       "
((invoke receiver: (variable) @var (symbol) @sym) @exemplar-node
(#match %S @var) (#match %S @sym))"
			       "^def_slotted_exemplar$"
			       (concat "^:" exemplar-name "$"))))))

(defun magik-company--ts-current-exemplar-node-with-locs ()
  "Exemplar node with the locations."
  (let* ((exemplar-node (magik-company--ts-exemplar-node-in-buffer-for
			 (magik-company--ts-exemplar-of-enclosing-method)))
	 (start-loc (and exemplar-node (treesit-node-start exemplar-node)))
	 (end-loc (and exemplar-node (treesit-node-end exemplar-node))))
    `((:node . ,exemplar-node)
      (:start . ,start-loc)
      (:end . ,end-loc))))

(defun magik-company--ts-variables-in-scope ()
  "Return a list of all variable nodes within the enclosing fragment scope."
  (interactive)
  (let ((variables '())
	(scope (magik-company--ts-enclosing-scope nil)))
    (when scope
      (setq variables (magik-company--ts-import-variables-in-scope scope variables))
      (dolist (a-node (magik-company--ts-node-type-in-scope scope "assignment"))
	(setq variables (magik-company--ts-lhs-variables-in-assignment-node a-node variables)))
      (dolist (a-node (magik-company--ts-node-type-in-scope scope "iterator"))
	(setq variables (magik-company--ts-for-loop-variables-in-scope a-node variables)))
      (dolist (a-node (magik-company--ts-node-type-in-scope scope "local"))
	(setq variables (magik-company--ts-for-local-variables-in-scope a-node variables))))
    (delete-dups variables)))

(provide 'magik-company-treesitter-extras)
;;; magik-company-treesitter-extras.el ends here
