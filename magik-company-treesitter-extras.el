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

(defvar magik-company--ts-scope-keywords '("method" "block"))


(defun magik-company--ts-assignments-in-scope (node)
  "Assignments in a NODE."
  (let ((results '()))
    (let ((stack (list node)))
      (while stack
	(let ((current (pop stack)))
	  (when (string= (treesit-node-type current) "assignment")
	    (push current results))
	  (setq stack (append (treesit-node-children current) stack)))))
    (nreverse results))
  )

(defun magik-company--ts-enclosing-scope ()
  "Return the enclosing node of type ts-scope-keywords."
  (let ((node (treesit-node-at (point))))
    (while (and node
		(not (member (treesit-node-type node)
			     magik-company--ts-scope-keywords)))
      (setq node (treesit-node-parent node)))
    node))

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

(defun magik-company--ts-variables-in-scope ()
  "Return a list of all variable nodes within the enclosing fragment scope."
  (interactive)
  (let ((variables '())
	(scope (magik-company--ts-enclosing-scope)))
    (when scope
      (dolist (a-node (magik-company--ts-assignments-in-scope scope))
	(setq variables (magik-company--ts-lhs-variables-in-assignment-node a-node variables))
	))
    (delete-dups variables)))

(provide 'magik-company-treesitter-extras)
;;; magik-company-treesitter-extras.el ends here
