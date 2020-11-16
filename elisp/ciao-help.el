;;; ciao-help.el --- Help for the Ciao environment
;; Copyright (C) 1986-2012 Free Software Foundation, Inc. and
;; M. Hermenegildo and others (herme@fi.upm.es, UPM-CLIP, Spain).

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;------------------------------------------------------------
;; Help (locating manuals etc.)
;;------------------------------------------------------------

;;; Code:

(require 'ciao-config) ; ciao-get-config
(require 'info)

(defvar ciao-info-dir (ciao-get-config :builddoc-dir)
  "Where the actual Ciao (LPdoc) info directory is.")

;; As all Ciao manuals have the same index nodes, there should be no problem
;; if this list is fixed. In case this changes, it would be interesting
;; using Info-index-nodes to obtain dynamically these nodes, but as
;; this function takes too much time at Emacs start, the use of a fixed list
;; is preferable at the moment
(defvar ciao-manual-index-list
  (list "Library/Module Index" "Predicate Index" "Property Index"
	"Regular Type Index" "Declaration Index" "Concept Index"
	"Author Index" "Global Index")
  "List of all indexes from the Ciao manual.")

;; TODO: This may not be necessary if stored in the bundle data
;; structure
(defun ciao-help-info-entry-exists-p (inf)
  "Detects if the (Ciao) info manual INF actually exists."
  (file-exists-p
   (concat ciao-info-dir "/" (car inf))))


(defun ciao-get-generated-manual-bases ()
  "Return the list of Ciao generated manual bases."
  (when (file-directory-p ciao-info-dir)
      (let ((manuals (directory-files ciao-info-dir nil "[a-zA-Z_]+[.]info$")))
	(mapcar (lambda (manual) (file-name-base manual))
		manuals))))

;;;###autoload
(defun ciao-update-info-dir ()
  "Update the info directory list to include Ciao manuals."
  (progn
    ;; (message "Updating Ciao info")
    (add-hook 'Info-mode-hook		; After Info-mode has started
	      (lambda ()
		(setq Info-additional-directory-list Info-default-directory-list)
		))
    (if (null Info-directory-list)    ; but it is not initialized there
	(progn
	  (setq Info-default-directory-list ; Will be initialized from here
		(cons ciao-info-dir Info-default-directory-list))
	  (load-library "info")             ; Info creates Info-directory-list
	  )
      (setq Info-directory-list (cons ciao-info-dir Info-directory-list)))
    ))

;;;###autoload(ciao-update-info-dir)
  
(defun ciao-goto-manuals ()
  "Go to the part of the info directory containing the Ciao manuals."
  (interactive)
  (ciao-locate-manual-in-info-dir "Ciao system"))

(defun ciao-locate-manual-in-info-dir (entry)
  "Locate a manual ENTRY in the info dir."
  (info)
  (Info-directory)
  (if (search-forward entry nil t)
      (recenter 0)
    (error (concat "Could not find " entry " manual in info dir"))))

;;;###autoload
(defun ciao-describe-mode ()
  "Show a short description of the Ciao Emacs mode, including all key bindings."
  (interactive)
  (describe-mode))


;; Provide ourselves:

(provide 'ciao-help)

;;; ciao-help.el ends here
