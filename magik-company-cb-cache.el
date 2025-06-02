;;; magik-company-cb-cache.el --- Contains all the functionality to communicate with the class browser and the results are stored here.  -*- lexical-binding: t; -*-

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
(require 'magik-company-cb)

(defvar magik-company--objects-source-cache-loaded nil
  "Tracks whether the object cache is loaded, for optional reset.")
(defvar magik-company--globals-source-cache-loaded nil
  "Tracks whether the globals cache is loaded, for optional reset.")
(defvar magik-company--conditions-source-cache-loaded nil
  "Tracks whether the conditions cache is loaded, for optional reset.")

(defvar magik-company--objects-source-cache nil)
(defvar magik-company--globals-source-cache nil)
(defvar magik-company--conditions-source-cache nil)
(defvar magik-company--class-method-source-cache nil)

(defun magik-company-reload-cache (&rest _args)
  "Reset the caches such that they will refill upon triggering the prefix."
  (interactive)
  (setq magik-company--objects-source-cache-loaded nil
	magik-company--globals-source-cache-loaded nil
	magik-company--conditions-source-cache-loaded nil
	magik-company--objects-source-cache nil
	magik-company--globals-source-cache nil
	magik-company--conditions-source-cache nil
	magik-company--class-method-source-cache nil))

(defun magik-company--load-source-caches ()
  "Check if the source caches are loaded and do init if needed."
  (when (not magik-company--objects-source-cache-loaded)
    (magik-company--objects-source-init))

  (when (not magik-company--globals-source-cache-loaded)
    (magik-company--globals-source-init))

  (when (not magik-company--conditions-source-cache-loaded)
    (magik-company--conditions-source-init)))

(defun magik-company--method-candidates (prefix)
  "List of methods on a class.
Uses a cache variable `magik-company--class-method-source-cache'.
All the methods beginning with the first character,
 are returned and stored in the cache.
Thus subsequent characters refining the match are handled by auto-complete.
the list of all possible matches, without recourse to the class browser.
PREFIX the current prefix"
  (let ((exemplar (magik-company--exemplar-near-point))
	(short-prefix prefix))
    (if exemplar
	(progn
	  (setq short-prefix (concat exemplar "." (if (> (length short-prefix) 0) (substring short-prefix 0 1))))
	  (if (not (and magik-company--class-method-source-cache
			(equal (concat " " short-prefix) (car magik-company--class-method-source-cache))))
	      (progn
		(when (magik-company--cb-start-process)
		  (setq magik-company--class-method-source-cache (magik-company--cb-method-candidates short-prefix))))))
      (setq magik-company--class-method-source-cache nil))
    magik-company--class-method-source-cache))

(defun magik-company--objects-source-init (&optional reset)
  "Initialisation function for obtaining all Magik Objects for use in company-mode.
If RESET is true, the cache is regenerated."
  (when (magik-company--cb-start-process)
    (when (or (not magik-company--objects-source-cache-loaded) reset)
      (let ((prefix "sw:object"))
	(setq magik-company--objects-source-cache (magik-company--cb-class-candidates prefix))
	(setq magik-company--objects-source-cache-loaded t)))))

(defun magik-company--globals-source-init (&optional reset)
  "Initialisation function for obtaining all Magik Globals for use in company-mode.
If RESET is true, the cache is regenerated."
  (when (magik-company--cb-start-process)
    (when (or (not magik-company--globals-source-cache-loaded) reset)
      (let ((prefix "<global>."))
	(setq magik-company--globals-source-cache (magik-company--cb-method-candidates prefix))
	(setq magik-company--globals-source-cache-loaded t)))))

(defun magik-company--conditions-source-init (&optional reset)
  "Initialisation function for obtaining all Magik Conditions.
Magik conditions are for use in company-mode.
If RESET is true, the cache is regenerated."
  (when (magik-company--cb-start-process)
    (when (or (not magik-company--conditions-source-cache-loaded) reset)
      (let ((prefix "<condition>."))
	(setq magik-company--conditions-source-cache (magik-company--cb-method-candidates prefix))
	;; adds an : in front so the prefix works with the results.
	(setq magik-company--conditions-source-cache
	      (mapcar (lambda (item) (concat ":" item)) (magik-company--cb-method-candidates prefix)))
	(setq magik-company--conditions-source-cache-loaded t)))))

(provide 'magik-company-cb-cache)
;;; magik-company-cb-cache.el ends here
