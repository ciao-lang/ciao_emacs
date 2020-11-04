;;; ciao-info-look.el --- info-look support for ciao-mode

;; Copyright (C) 2019-2020  Miguel Angel Sanchez Ordaz

;; Author: Miguel Angel Sanchez Ordaz <ma.sanchez.ordaz@imdea.org>
;; Keywords: convenience languages tools
;; Package-Version: 0.1
;; Package-Requires: ((emacs "24.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; info-look support for ciao-mode.

;;; Code:

(require 'info)
(require 'info-look)
(require 'seq) ; seq-do, seq-filter
(require 'ciao-config) ; ciao-get-config
(require 'ciao-help)

(defun ciao-doc-spec-entry (manual index)
  "Build the structure for a doc-spec entry, from an INDEX in a Ciao MANUAL.
Returns the stucture of a doc-spec entry."
  (list
   (concat "(" (concat manual (concat ")" index)))
   (lambda (item)
     (when (string-match "^[A-Za-z0-9_@:=<>/\\\.%\$]+" item)
       (replace-regexp-in-string " <[0-9]+>" ""  item)))
   "^ -- \\(PREDICATE\\|PROPERTY\\|REGTYPE\\|DECLARATION\\|MODE\\): "
   ":"))

(defun ciao-doc-spec ()
  "Builds all the structure for Ciao doc-spec.
Return the structure for ciao doc-spec."
  (let ((doc-spec nil)
	(manuals (ciao-get-generated-manual-bases)))
    (if (eq manuals nil)
	(message "WARNING: info-look could not find Ciao manuals.")
      (let ((templist nil))
	(seq-do
	 (lambda (manual)
	   (let ((indexes ciao-manual-index-list))
	     (seq-do
	      (lambda (index)
		(if (string-match "\\(Library/Module\\|Predicate\\|Property\\|Regular Type\\|Declaration\\) Index" index)
		    (push (ciao-doc-spec-entry manual index) doc-spec)
		  (push (ciao-doc-spec-entry manual index) templist)))
		indexes)))
	 manuals)
	(setq doc-spec (append doc-spec templist))))
    doc-spec))

(info-lookup-maybe-add-help
  :mode 'ciao-mode
  :topic 'symbol
  :regexp "^[A-Za-z0-9_@:=<>/\\\.%\$]+"
  :doc-spec (ciao-doc-spec)
  )

(defvar ciao-completions nil
  "List of words corresponding to the index entries of the Ciao manuals.")

(defun ciao-get-completions ()
  "Return all index entries from the Ciao manuals.
Index entries are obtained as a list of symbols, where every symbol is in the
form of a list by `info-lookup->symbol'.  Predicates without arity are added."
  (when (not ciao-completions)
      (let ((info-look-completions (info-lookup->completions 'symbol 'ciao-mode))
	    (completions nil))
	(seq-do
	 (lambda (symbol)
	   (when (string-match "/[0-9]+" (car symbol))
	     (push (replace-regexp-in-string "/[0-9]+" "" (car symbol)) completions))
	   (push (car symbol) completions))
	 info-look-completions)
	(setq ciao-completions completions)))
  ciao-completions)

(defun info-lookup-guess-ciao-symbol ()
  "Get the Ciao symbol at point."
  (let (begin end)
    (save-excursion
      (skip-syntax-backward "w")
      (setq begin (point))
      (skip-syntax-forward "w")
      (setq end (point)))
    (when (not (eq begin end))
      (buffer-substring-no-properties begin end))))

;;;###autoload
(defun ciao-info-lookup-symbol (symbol)
  "Display the definition of a Ciao SYMBOL, as found in the relevant manual."
    (interactive
     (list
      (let ((default (info-lookup-guess-ciao-symbol)))
	  (completing-read
	    (if (not default) "Look up symbol: "
	      (format "Look up symbol (default %s): " default))
	    (ciao-get-completions)
	    nil
	    nil
	    default
	    'info-lookup-history
	    default))))
    (unless (assoc symbol (info-lookup->completions 'symbol 'ciao-mode))
      (setq symbol (caar (seq-filter
			  (lambda (elt) (string-match (concat "^" (concat symbol "/[0-9]")) (car elt)))
			  (reverse (info-lookup->completions 'symbol 'ciao-mode))))))
    (info-lookup 'symbol symbol 'ciao-mode))

(defun ciao-info-complete ()
  "Completions at point for `ciao-mode' using `info-look'."
  (interactive)
  (let (begin end)
    (save-excursion
      (skip-syntax-backward "w")
      (setq begin (point))
      (skip-syntax-forward "w")
      (setq end (point)))
    (completion-in-region begin end (ciao-get-completions))))

; Modified code from info.el
(defun info-selected-manuals-matches (string manuals)
  "Collect STRING matches from a list of MANUALS.
Return a list of matches where each element is in the format
\((FILENAME INDEXTEXT NODENAME LINENUMBER))."
  (unless (string= string "")
    (let ((pattern (format "\n\\* +\\([^\n]*\\(%s\\)[^\n]*\\):[ \t]+\\([^\n]+\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?" (regexp-quote string)))
	  (ohist Info-history)
	  (ohist-list Info-history-list)
	  (current-node Info-current-node)
	  (current-file Info-current-file)
	   matches node nodes)
      (let ((Info-fontify-maximum-menu-size nil))
	(Info-directory)
	(when (null current-node)
	  (setq current-file Info-current-file)
	  (setq current-node Info-current-node))
	(dolist (manual manuals)
	  (message "Searching %s" manual)
	  (condition-case err
	      (if (setq nodes (Info-index-nodes (Info-find-file manual)))
                  (save-excursion
                    (Info-find-node manual (car nodes))
                    (while
                        (progn
                          (goto-char (point-min))
                          (while (re-search-forward pattern nil t)
			    (let ((entry (match-string-no-properties 1))
				  (nodename (match-string-no-properties 3))
				  (line (match-string-no-properties 4)))
			      (add-text-properties
			       (- (match-beginning 2) (match-beginning 1))
			       (- (match-end 2) (match-beginning 1))
			       '(face info-index-match) entry)
			      (setq matches (cons (list manual entry nodename line)
						  matches))))
                          (setq nodes (cdr nodes) node (car nodes)))
                      (Info-goto-node node))))
	    (error
	     (message "%s" (if (eq (car-safe err) 'error)
			       (nth 1 err) err))
	     (sit-for 1 t)))))
      (Info-find-node current-file current-node)
      (setq Info-history ohist
	    Info-history-list ohist-list)
      (message "Searching indices...done")
      (or (nreverse matches) t))))

;;;###autoload
(defun ciao-info-apropos (string)
  "Grovel indices from Ciao manuals for STRING.
Build a menu of the possible matches."
  (interactive
   (list
     (let ((default (info-lookup-guess-ciao-symbol)))
	  (completing-read
	    (if (not default) "Look up symbol: "
	      (format "Look up symbol (default %s): " default))
	    (ciao-get-completions)
	    nil
	    nil
	    default
	    'info-lookup-history
	    default))))
  (if (equal string "")
      (Info-find-node Info-apropos-file "Top")
    (let* (nodename)
      (setq nodename (format "Index for ‘%s’" string))
	(push (list nodename string (info-selected-manuals-matches string (ciao-get-generated-manual-bases)))
	      Info-apropos-nodes)
	(Info-find-node Info-apropos-file nodename))))

(provide 'ciao-info-look)
;;; ciao-info-look.el ends here
