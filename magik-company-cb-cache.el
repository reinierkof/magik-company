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
(require 'magik-cb-ac)

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

(defun magik-company-reload-cache ()
  "Reset the caches such that they will refill upon triggering the prefix."
  (interactive)
  (setq magik-company--objects-source-cache-loaded nil
        magik-company--globals-source-cache-loaded nil
        magik-company--conditions-source-cache-loaded nil
        )
  )

(defun magik-company--load-source-caches ()
  "Check if the source caches are loaded and do init if needed."
    (when (not magik-company--objects-source-cache-loaded)
    (magik-company--objects-source-init))

  (when (not magik-company--globals-source-cache-loaded)
    (magik-company--globals-source-init))

  (when (not magik-company--conditions-source-cache-loaded)
    (magik-company--conditions-source-init))
  )

(defun magik-company--objects-source-init (&optional reset)
  "Initialisation function for obtaining all Magik Objects for use in auto-complete-mode.
If RESET is true, the cache is regenerated."
  (when (magik-cb-ac-start-process)
    (when (or (not magik-company--objects-source-cache-loaded) reset)
      (let ((prefix "sw:object"))
        (setq magik-company--objects-source-cache (magik-cb-ac-class-candidates prefix))
        (setq magik-company--objects-source-cache-loaded t)))))

(defun magik-company--globals-source-init (&optional reset)
  "Initialisation function for obtaining all Magik Conditions for use in auto-complete-mode.
If RESET is true, the cache is regenerated."
   (when (magik-cb-ac-start-process)
    (when (or (not magik-company--globals-source-cache-loaded) reset)
      (let ((prefix "<global>."))
        (setq magik-company--globals-source-cache (magik-cb-ac-method-candidates prefix))
        (setq magik-company--globals-source-cache-loaded t)))))

(defun magik-company--conditions-source-init (&optional reset)
  "Initialisation function for obtaining all Magik Conditions for use in auto-complete-mode.
If RESET is true, the cache is regenerated."
   (when (magik-cb-ac-start-process)
    (when (or (not magik-company--conditions-source-cache-loaded) reset)
      (let ((prefix "<condition>."))
        (setq magik-company--conditions-source-cache (magik-cb-ac-method-candidates prefix))
        ;; adds an : infront so the prefix works with the results.
        (setq magik-company--conditions-source-cache
              (mapcar (lambda (item) (concat ":" item)) (magik-cb-ac-method-candidates prefix)))
        (setq magik-company--conditions-source-cache-loaded t)))))

(provide 'magik-company-cb-cache)
;;; magik-company-cb-cache.el ends here
