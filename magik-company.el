;;; rewrite-magik-company.el --- Magik backend for company-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Reinier Koffijberg

;; Author: Reinier Koffijberg <some@gmail.com>

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

;; See the README for more details.

;;; Code:

(require 'company)
(require 'magik-cb-ac)
(require 'magik-company-annotation)
(require 'magik-company-buffer-cache)
(require 'magik-company-prefixes)
(require 'magik-company-cb-cache)
(require 'magik-company-exemplar-types)
(require 'magik-company-yasnippet-handling)

(defgroup company-magik nil
  "Company back-end for Magik code completion."
  :group 'company
  :group 'magik)

;;;###autoload
(defun company-magik (command &optional arg &rest ignored)
  "Company backend for magik-mode.
COMMAND, ARG, IGNORED"
  (interactive (list 'interactive))
  (cl-case command
    (prefix (magik-company--prefix))
    (candidates (magik-company--candidates))
    (annotation (magik-company--annotation arg)
    (kind nil)
    (post-completion (magik-company--post-completion arg))
    )
  )

(defun magik-company--candidates ()
  "Generate a list of completion candidates."
  (magik-company--load-source-caches)

  (let ((magik-candidates nil))
    (when (or magik-company-prefix-at-globals
	      magik-company-prefix-at-dynamics)
      (setq magik-candidates (nconc magik-candidates magik-company--globals-source-cache)))

    (when magik-company-prefix-at-objects
      (setq magik-candidates (nconc magik-candidates magik-company--objects-source-cache)))

    (when magik-company-prefix-at-methods
      (message "is prefix: %s, the same as magik-prefix: %s" prefix magik-company-cur-prefix)
      (setq magik-candidates (nconc magik-candidates
				     (magik-company--method-candidates magik-company-cur-prefix))))

    (when magik-company-prefix-at-conditions
      (setq magik-candidates (nconc magik-candidates magik-company--conditions-source-cache)))

    (dolist (candidate candidates)
      (when (magik-company--add-yasnippet-text-property candidate)
	))

    ;; should not contain duplicates, because the filter takes it out.
    ;; incase we need it we can use this one.
    ;; (setq magik-candidates (delete-dups magik-candidates))
    magik-candidates))



(defun magik-company--post-completion (candidate)
  "Insert parameters in snippet for CANDIDATE."
  (if (get-text-property 0 'yasnippet candidate)
      (magik-company--insert-candidate-yasnippet candidate)
    (magik-company--insert-candidate-args-yasnippet candidate))
  )


;; (defun magik-company--kind (candidate)
;;   (cond
;;    ((member candidate magik-company--objects-candidates)  'variable)
;;    ((member candidate magik-company--globals-candidates) (if (string-prefix-p "!" candidate) 'event 'field))
;;    ((member candidate magik-company--conditions-candidates)  'property)
;;    ((member candidate magik-company--class-method-candidates) 'method)
;;    (t nil)))
(provide 'magik-company)
;;; magik-company.el ends here
