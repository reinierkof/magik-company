;;; magik-company.el --- Magik backend for company-mode -*- lexical-binding: t; -*-

;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (magik-mode "0.4.1") (company "1.0.2") (yasnippet "0.14.0"))
;; URL: https://github.com/reinierkof/magik-company
;; Keywords: convenience

;; Copyright (C) 2024 Reinier Koffijberg

;; Author: Reinier Koffijberg <reinierkof@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a project for better auto-completion in Magik.

;;; Code:

(require 'company)
(require 'magik-company-annotation)
(require 'magik-company-cb)
(require 'magik-company-buffer-cache)
(require 'magik-company-prefixes)
(require 'magik-company-cb-cache)
(require 'magik-company-exemplar-types)
(require 'magik-company-yasnippet-handling)

(defvar magik-company--objects-candidates nil)
(defvar magik-company--globals-candidates nil)
(defvar magik-company--conditions-candidates nil)
(defvar magik-company--class-method-candidates nil)
(defvar magik-company--params-candidates nil)
(defvar magik-company--variables-candidates nil)
(defvar magik-company--slots-candidates nil)
(defvar magik-company--exemplar-candidate nil)
(defvar magik-company--initialised? nil)
(defvar magik-company--major-mode nil)
(defvar magik-company--session-mode "magik-session-mode")
(defvar magik-company--buffer-mode "magik-ts-mode")

(defgroup magik-company nil
  "Company back-end for Magik code completion."
  :group 'company
  :group 'magik)

;;;###autoload
(define-minor-mode magik-company-mode
  "Minor mode to enable the Magik company backend."
  :lighter nil
  (if magik-company-mode
      (magik-company--enable)
    (magik-company--disable)))

(defun magik-company--enable ()
  "Set up buffer for `magik-company-mode`."
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'magik-company)
  (setq magik-company--major-mode major-mode)
  (unless magik-company--initialised?
    (advice-add #'magik-transmit-region :after #'magik-company-reload-cache)
    (advice-add #'magik-session-kill-process :after #'magik-company--exit-cb-buffers)
    (add-hook 'magik-session-start-process-post-hook #'magik-company--kill-cb-ac-buffer)
    (setq magik-company--initialised? t)))

(defun magik-company--disable ()
  "Tear down `magik-company-mode` in this buffer."
  (setq company-backends (remove 'magik-company company-backends)))

;;;###autoload
(defun magik-company (command &optional arg &rest _ignored)
  "Company backend for `magik-mode'.
COMMAND, ARG, IGNORED"
  (interactive (list 'interactive))
  (cl-case command
    (prefix (magik-company--prefix))
    (candidates (magik-company--candidates))
    (annotation (magik-company--annotation arg))
    (kind (magik-company--kind arg))
    (post-completion (magik-company--post-completion arg))))

(defun magik-company--prefix ()
  "Prefix and what to load based on those the current point."
  (if (or (magik-company--in-comment)
	  (magik-company--in-string)
          (and (string= magik-company--major-mode magik-company--session-mode)
               (not (magik-company--session-within-typeable-area))))
      (setq magik-company-prefix-at-methods nil
	    magik-company-prefix-at-conditions nil
	    magik-company-prefix-at-dynamics nil
	    magik-company-prefix-at-globals nil
	    magik-company-prefix-at-slot nil
	    magik-company-prefix-at-objects nil)
    (progn
      (setq magik-company-prefix-at-methods (magik-company--at-method-prefix)
	    magik-company-prefix-at-conditions (magik-company--at-raise-condition-prefix)
	    magik-company-prefix-at-dynamics (magik-company--at-dynamic-prefix)
	    magik-company-prefix-at-globals (magik-company--at-global-prefix)
	    magik-company-prefix-at-slot (magik-company--at-slot-prefix)
	    magik-company-prefix-at-objects (magik-company--at-object-prefix)))
    (magik-company--determine-cur-prefix)))

(defun magik-company--candidates ()
  "Generate a list of completion candidates."
  (magik-company--load-source-caches)

  (let ((magik-candidates '()))
    (when (string= magik-company--major-mode magik-company--buffer-mode)
      (magik-company--buffer-local-candidates magik-candidates))

    (when (or magik-company-prefix-at-globals
	      magik-company-prefix-at-dynamics)
      (setq magik-company--globals-candidates
	    (magik-company--filter-candidates magik-company--globals-source-cache magik-candidates))
      (setq magik-candidates (append magik-candidates magik-company--globals-candidates)))

    (when magik-company-prefix-at-objects
      (setq magik-company--objects-candidates (magik-company--filter-candidates magik-company--objects-source-cache magik-candidates))
      (setq magik-candidates (append magik-candidates magik-company--objects-candidates)))

    (when magik-company-prefix-at-methods
      (setq magik-company--class-method-candidates (magik-company--filter-candidates (magik-company--method-candidates magik-company-cur-prefix) magik-candidates))
      (setq magik-candidates (append magik-candidates magik-company--class-method-candidates)))

    (when magik-company-prefix-at-conditions
      (setq magik-company--conditions-candidates (magik-company--filter-candidates magik-company--conditions-source-cache magik-candidates))
      (setq magik-candidates (append magik-candidates magik-company--conditions-candidates)))

    (dolist (candidate magik-candidates)
      (magik-company--add-yasnippet-text-property candidate))

    ;; should not contain duplicates, because the filter takes it out.
    ;; in case we need it we can use this one.
    ;; (setq magik-candidates (delete-dups magik-candidates))
    magik-candidates))

(defun magik-company--buffer-local-candidates(magik-candidates)
  "Add to the list of completion MAGIK-CANDIDATES with buffer-local functions."
  (magik-company--update-buffer-caches)
  (when magik-company-prefix-at-slot
    (setq magik-company--slots-candidates (magik-company--filter-candidates magik-company--slots-cache magik-candidates))
    (setq magik-candidates (append magik-candidates magik-company--slots-candidates)))

  (when magik-company-prefix-at-objects
    (setq magik-company--params-candidates (magik-company--filter-candidates magik-company--params-cache magik-candidates))
    (setq magik-candidates (append magik-candidates magik-company--params-candidates))

    (setq magik-company--variables-candidates (magik-company--filter-candidates magik-company--variables-cache magik-candidates))
    (setq magik-candidates (append magik-candidates magik-company--variables-candidates))

    (setq magik-company--exemplar-candidate (magik-company--filter-candidates magik-company--classname-cache magik-candidates))
    (setq magik-candidates (append magik-candidates magik-company--exemplar-candidate))))

(defun magik-company--filter-candidates (new-candidates existing-candidates)
  "Filter NEW-CANDIDATES.
Include only candidates starting with current prefix.
and are not already present in EXISTING-CANDIDATES."
  (if (listp new-candidates)
      (progn
	(cl-remove-if-not (lambda (candidate)
			    (and (string-prefix-p magik-company-cur-prefix candidate)
				 (not (member candidate existing-candidates))))
			  new-candidates))
    '()))

(defun magik-company--post-completion (candidate)
  "Insert parameters in snippet for CANDIDATE."
  (if (get-text-property 0 'yasnippet candidate)
      (magik-company--insert-candidate-yasnippet candidate)
    (magik-company--insert-candidate-args-yasnippet candidate)))

(defun magik-company--kind (candidate)
  "Retrieve the kind for CANDIDATE."
  (if (get-text-property 0 'yasnippet candidate)
      'snippet
    (get-text-property 0 'kind candidate)))

(provide 'magik-company)
;;; magik-company.el ends here
