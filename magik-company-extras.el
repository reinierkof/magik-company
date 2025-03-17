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

(require 'magik-company-cb)

(with-eval-after-load 'magik-mode
  (message "TESTING HOOK ADDED")
  (add-hook 'magik-session-start-process-post-hook 'magik-company--kill-cb-ac-buffer))


(defconst magik-session-cb-ac-buffer "*cb-company*"
  "The autocomplete class browser buffer associated with the GIS process.")

(defvar magik-company--cb-process nil
  "Variable that references to the actual process.")

(defun magik-company--kill-cb-ac-buffer ()
  "Try to kill the magik-company buffer when starting a session."
    (when (get-buffer magik-session-cb-ac-buffer)
      (let ((magik-local-cb-ac-process (get-buffer-process (get-buffer magik-session-cb-ac-buffer))))
	(when magik-local-cb-ac-process
          (delete-process magik-local-cb-ac-process)
          (setq magik-company--cb-process nil)))))

(provide 'magik-company-extras)
;;; magik-company-extras.el ends here
