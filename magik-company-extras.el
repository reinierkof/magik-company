;;; magik-company-extras.el --- This file adds hooks and session names for the classbrowser to start to the original magik-mode.  -*- lexical-binding: t; -*-

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
(require 'company)

(add-hook 'magik-session-start-process-post-hook #'magik-company--kill-cb-ac-buffer)

(defconst magik-company--cb-buffer "*cb-company*"
  "The autocomplete class browser buffer associated with the GIS process.")

(defvar magik-company--cb-process nil
  "Variable that references to the actual process.")

(defun magik-company--kill-cb-ac-buffer ()
  "Try to kill the magik-company buffer when starting a session."
  (when (get-buffer magik-company--cb-buffer)
    (let ((magik-local-cb-ac-process (get-buffer-process (get-buffer magik-company--cb-buffer))))
      (when magik-local-cb-ac-process
	(delete-process magik-local-cb-ac-process)
	(setq magik-company--cb-process nil)))))

(defun magik-company--add-icon-to-alist (kind icon)
  "Append a KIND to ICON in `company-vscode-icons-mapping' if its new."
  (unless (assoc kind company-vscode-icons-mapping)
    (setq company-vscode-icons-mapping
	  (append company-vscode-icons-mapping
		  `((,kind . ,icon))))))

(dolist (entry '((dynamic . "symbol-event.svg")
		 (exemplar . "symbol-class.svg")
		 (slot  . "symbol-array.svg")
		 (method  . "symbol-method.svg")
		 (assign-method  . "symbol-method.svg")
		 (condition  . "symbol-property.svg")
		 (parameter  . "symbol-parameter.svg")
		 (global  . "symbol-misc.svg")))
  (magik-company--add-icon-to-alist (car entry) (cdr entry)))

(provide 'magik-company-extras)
;;; magik-company-extras.el ends here
