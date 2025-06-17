;;; magik-company-cb.el --- ;;; magik-company-cb.el --- Magik Classbrowser company Support -*- lexical-binding: t; -*-

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

(require 'magik-cb)
(require 'magik-session)
(require 'magik-company-extras)

(defvar magik-company--cb-max-methods 1000)
(defvar magik-company--session-running nil)
(defvar magik-company--cb-candidadates nil)

(declare-function magik-company-reload-cache "magik-company")

(defun magik-company--exit-cb-buffers ()
  "Ensure cb buffers are deleted and status is reset."
  (setq magik-company--session-running nil)
  (magik-company-reload-cache)
  (magik-company--force-kill-cb-company-buffers))

(defun magik-company--cb-filter (p s)
  "Process data coming back from the CB auto-complete buffer.
P ...
S ..."
  (with-current-buffer (process-buffer p)
    (unwind-protect
	(let ((buffer-read-only nil)
	      (coding-system-for-read magik-cb-coding-system)
	      fn)
	  (setq magik-cb-filter-str (concat magik-cb-filter-str s))
	  (save-match-data
	    (setq fn (cond ((string-match "\C-e" magik-cb-filter-str)
			    'magik-company--cb-candidate-methods)
			   ((string-match "\C-c" magik-cb-filter-str)
			    'magik-company--cb-candidate-classes)
			   (t
			    nil))))
	  (setq magik-cb-filter-str ""
		magik-company--cb-candidadates (if fn
						   (progn
						     (insert-file-contents (magik-cb-temp-file-name p) nil nil nil t)
						     (funcall fn)))))
      (setq magik-cb-filter-str ""
	    magik-company--cb-candidadates (if (eq magik-company--cb-candidadates 'unset) nil magik-company--cb-candidadates)))))

(defun magik-company--cb-start-process ()
  "Start a Class Browser process for auto-complete-mode.
Returns t if the process was started or running, nil if there's an error."
  (if (process-live-p magik-company--cb-process)
      t
    (let ((gis-buffer-name (magik-company--cb-get-gis-buffer))
	  smallworld-gis)
      (when (and gis-buffer-name
		 (or magik-company--session-running
		     (magik-company--magik-session-started? gis-buffer-name)))
	(setq smallworld-gis (buffer-local-value 'magik-smallworld-gis (get-buffer gis-buffer-name)))
	(setq magik-company--cb-process
	      (magik-cb-get-process-create
	       magik-company--cb-buffer 'magik-company--cb-filter smallworld-gis gis-buffer-name nil)))
      (if (process-live-p magik-company--cb-process)
	  (progn
	    (magik-company-reload-cache)
	    t)
	nil))))

(defun magik-company--force-kill-cb-company-buffers ()
  "Kill all buffers whose names start with *cb and end with company*.
killing any associated processes without prompting."
  (dolist (buf (buffer-list))
    (let ((name (buffer-name buf)))
      (when (and name
		 (string-prefix-p "*cb" name)
		 (string-suffix-p "company*" name))
	(when-let* ((proc (get-buffer-process buf)))
	  (delete-process proc))
	(kill-buffer buf)))))

(defun magik-company--magik-session-started?(gis-buffer-name)
  "Check in the buffer from GIS-BUFFER-NAME whether session is initialized."
  (let ((session-started?
	 (with-current-buffer gis-buffer-name
	   (goto-char (point-max))
	   (let ((magik-prefix-pos (save-excursion (when (re-search-backward "Magik>" nil t)
						     (match-beginning 0))))
		 (magik-end-process-pos (save-excursion (when (re-search-backward "Process magik-session-process" nil t)
							  (match-beginning 0)))))
	     (if (and (number-or-marker-p magik-prefix-pos)
		      (or (not (number-or-marker-p magik-end-process-pos))
			  (> magik-prefix-pos magik-end-process-pos)))
		 t
	       nil)))))
    (if session-started?
	(magik-company--method-finder-started? gis-buffer-name)
      nil
      )))

(defun magik-company--method-finder-started?(gis-buffer-name)
  "Poke to the magik-process in GIS-BUFFER-NAME to see if the method-finder is started."
  (let ((smallworld-gis (get-buffer-process (get-buffer gis-buffer-name))))
    (if smallworld-gis
	(progn
	  ;; send some magik code to start the method finder.
	  (process-send-string smallworld-gis "_if method_finder _isnt _unset\n_then\n  method_finder.lazy_start?\n_endif\n$\n")
	  (with-current-buffer gis-buffer-name
	    (goto-char (point-max))
	    (let ((method-finder-pos (save-excursion (when (re-search-backward "method_finder: loaded" nil t)
						       (match-beginning 0))))
		  (magik-end-process-pos (save-excursion (when (re-search-backward "Process magik-session-process" nil t)
							   (match-beginning 0)))))
	      (if (and (number-or-marker-p method-finder-pos)
		       (or (not (number-or-marker-p magik-end-process-pos))
			   (> method-finder-pos magik-end-process-pos)))
		  (progn (setq magik-company--session-running t)
			 t)
		nil))))
      nil)))

(defun magik-company--cb-get-gis-buffer ()
  "Find the gis buffer in current buffers if it is active.
Stores the buffer name in `magik-company--cb-gis-buffer-name`
 or returns nil if no GIS buffer is found."
  (let ((gis-buffer-name nil))
    (cl-loop for buffer in (buffer-list)
	     do (if (and (stringp (buffer-name buffer))
			 (string-prefix-p "*gis" (buffer-name buffer))
			 (get-buffer-process buffer))
		    (setq gis-buffer-name (buffer-name buffer))))
    gis-buffer-name))

(defun magik-company--cb-candidate-methods ()
  "Return candidate methods matching `ac-prefix' from Method finder output."
  ;;TODO combine method definition with its signature.
  (let ((method (car nil))
	(ac-limit magik-company--cb-max-methods))
    (setq method
	  (if (zerop (length method))
	      "\\sw"
	    (regexp-quote method)))
    (let ((i 0)
	  (regexp (concat "^\\(" method "\\S-*\\)" magik-cb-in-keyword "\\(\\S-+\\)\\s-+\\(.*\\)\n\\(.*\n\\)\n\\(\\( +##.*\n\\)*\\)"))
	  candidate
	  classify
	  class
	  args
	  documentation
	  candidates)
      (goto-char (point-min))
      (save-match-data
	(while (and (or (null ac-limit) (< i ac-limit))
		    (re-search-forward regexp nil t))
	  (setq candidate (match-string-no-properties 1)
		class     (match-string-no-properties 2)
		classify  (match-string-no-properties 3)
		args      (magik-company--cb-method-args (match-beginning 4))
		documentation (match-string-no-properties 5))
	  (magik-company--cb-add-method-properties candidate class args classify documentation)
	  (if (member candidate candidates)
	      nil ; already present
	    (setq candidates (append (list candidate) candidates)
		  i (1+ i)))))
      (nreverse candidates))))


(defun magik-company--cb-method-args (pt)
  "Return method arguments from Class Browser at point PT."
  (save-excursion
    (goto-char pt)
    (save-match-data
      (let ((case-fold-search nil)
	    optional
	    args
	    gather
	    opt
	    name)
	(if (looking-at "$")
	    nil ; No arguments
	  (forward-char 1) ; space
	  (while (not (looking-at "$"))
	    (setq pt (point))
	    (cond ((looking-at "\\(OPT \\)?GATH \\(.*\\)")
		   (setq gather (list (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
		   (goto-char (match-end 0)))
		  ((looking-at "OPT ")
		   (setq opt t)
		   (goto-char (match-end 0)))
		  ((> (skip-syntax-forward "w_") 0) ; found argument (may contain _ which may be classed as symbols.)
		   (setq name (list (buffer-substring-no-properties pt (point))))
		   (if opt
		       (setq optional (append optional name))
		     (setq args (append args name)))
		   (if (eq (following-char) ? )
		       (forward-char 1)))
		  (t ;catch all error
		   (message "Found unrecognised character at %d in %s" (point) (current-buffer))
		   (goto-char (end-of-line))))))
	(list args optional gather)))))

(defun magik-company--cb-candidate-classes ()
  "Return candidate classes from Method finder output."
  (let ((i 0)
	(regexp (concat "\\(\\S-+:\\)\\(\\S-+\\)")) ; capture class name and its package
	candidate
	candidates
	package)
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward regexp nil t)
	(setq candidate (match-string-no-properties 2))
	(setq package (match-string-no-properties 1))
	(setq package (substring package 0 (1- (length package))))
	(put-text-property 0 (length candidate)  'kind 'exemplar candidate)
	(put-text-property 0 (length candidate)  'package package candidate)
	(save-match-data
	  ;; Use cl-pushnew to prevent duplicates
	  (cl-pushnew candidate candidates :test 'equal)
	  (setq i (1+ i))))
      candidates)))

(defun magik-company--cb-add-method-properties (candidate class args classify documentation)
  "Return method documentation string.
CLASS ...
CANDIDATE ...
ARGS ...
CLASSIFY ...
DOCUMENTATION ..."
  (let* ((required (elt args 0))
	 (optional (elt args 1))
	 (gather (elt args 2))
	 (candidate-length (length candidate))
	 (method-signature (magik-method-name-type candidate))
	 (signature (cdr method-signature))
	 (signature-p (> (length signature) 0))
	 assignment)

    (cond ((zerop (length classify))
	   ;; only check for iter
	   ;; classify also contains (iter)
	   nil)
	  ((string-match "iter" classify)
	   (put-text-property 0 candidate-length 'iter t candidate))
	  ((equal (substring classify 0 1) "A")
	   (setq classify (concat "Advanced" (substring classify 1))))
	  ((equal (substring classify 0 1) "B")
	   (setq classify (concat "Basic" (substring classify 1))))
	  (t
	   ;;do nothing
	   nil))

    ;; Handle << assignment like signatures - take first required argument
    (if (and signature-p (equal (substring signature -1) "<"))
	(setq assignment (car required)
	      required (cdr required)))

    (when documentation
      (while (string-match "^ +## " documentation)
	(setq documentation (replace-match "" nil nil documentation)))
      (put-text-property 0 candidate-length 'documentation documentation candidate))

    (when required
      (put-text-property 0 candidate-length 'arguments required candidate))

    (when gather
      (put-text-property 0 candidate-length 'gather gather candidate))

    (when optional
      (put-text-property 0 candidate-length 'optional optional candidate))

    ;;Switch case
    (cond
     ((equal class "<condition>")
      (put-text-property 0 candidate-length 'kind 'condition candidate))

     ((equal class "<global>")
      ;; Global class is either a dynamic or a global proc
      (if (string-prefix-p "!" candidate)
	  (put-text-property 0 candidate-length 'kind 'dynamic candidate)
	(put-text-property 0 candidate-length 'kind 'global candidate)))

     ;; No signs after the method
     ((not signature-p)
      (put-text-property 0 candidate-length 'kind 'method candidate))

     ;; handles normal methods
     ;; Still need to find a way to have start signature in the candidate.
     ((equal (substring signature 0 1) "(")
      (put-text-property 0 candidate-length 'kind 'method candidate)
      (put-text-property 0 candidate-length 'start-signature "(" candidate)
      (put-text-property 0 candidate-length 'end-signature (substring signature 1) candidate))
     (assignment
      (put-text-property 0 candidate-length 'kind 'assignment-method candidate)
      (put-text-property 0 candidate-length 'assign-signature signature candidate)))))

(defun magik-company--cb-method-candidates (prefix)
  "Return list of methods for a class matching PREFIX for auto-complete mode.
PREFIX is of the form \"CLASS\".\"METHOD_NAME_PREFIX\""
  (let ((magik-company--cb-candidadates 'unset) ; use 'unset symbol since nil is also a valid return value.
	(ac-limit 1000)
	class method character)
    (save-match-data
      (cond ((null magik-company--cb-process)
	     (setq magik-company--cb-candidadates nil))
	    ((not (string-match "\\(\\S-+\\)\\.\\(.*\\)" prefix))
	     (setq magik-company--cb-candidadates nil))
	    (t
	     (setq class (match-string-no-properties 1 prefix)
		   method (match-string-no-properties 2 prefix)
		   character (if (equal method "") method (substring method 0 1)))
	     (process-send-string magik-company--cb-process
				  (concat "method_name ^" character "\n"
					  "unadd class \nadd class " class "\n"
					  "method_cut_off " (number-to-string ac-limit) "\n"
					  "override_flags\nshow_classes\nshow_args\nshow_comments\nprint_curr_methods\nshow_topics\n"))
	     (while (and (eq magik-company--cb-candidadates 'unset)
			 (magik-cb-is-running nil magik-company--cb-process))
	       (sleep-for 0.1))
	     (setq magik-company--cb-candidadates (append (list (concat " " class "." character)) magik-company--cb-candidadates)))))
    magik-company--cb-candidadates))

(defun magik-company--cb-class-candidates (prefix)
  "Return list of classes matching PREFIX for auto-complete mode."
  (let ((magik-company--cb-candidadates 'unset)) ; use 'unset symbol since nil is also a valid return value.
    (cond ((null magik-company--cb-process)
	   (setq magik-company--cb-candidadates nil))
	  (t
	   (process-send-string magik-company--cb-process
				(concat "dont_override_flags\npr_family " prefix "\n"))
	   (while (and (eq magik-company--cb-candidadates 'unset)
		       (magik-cb-is-running nil magik-company--cb-process))
	     (sleep-for 0.1))))
    magik-company--cb-candidadates))

(provide 'magik-company-cb)
;;; magik-company-cb.el ends here
