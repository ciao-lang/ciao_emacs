;;; ciao-builder.el --- Interface to Ciao Builder
;; Copyright (C) 2012-2021 Jose F. Morales <jfmcjf@gmail.com>

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

(require 'ciao-config) ; ciao-get-config
(require 'ciao-common) ; ciao-root-dir,
                       ; ciao-path-dirs

;;===========================================================================
;;
;; NOTE: Keep it simple (everything should be reproducible from the
;;   command line or toplevel)
;;
;; TODO:
;;   - Incomplete (it needs more commands)
;;   - Entries in emacs menus and some documentation is missing.
;;   - Processes (like toplevels) need manual restart on update.
;;   - Remove dependencies from shell scripts and POSIX tools (as much
;;     as possible)
;;
;;===========================================================================

(defun ciao-get-builder-proc-buffer ()
  (get-buffer-create "*Ciao-Process-builder*"))

;; TODO: define 'ciao-builder-build-all'?
;; TODO: define 'ciao-builder-update'? (should it download anything?)

(defun ciao-builder-cmdstr (cmd)
  (concat "INSIDE_EMACS=t " (ciao-get-config :ciaosh-bin) " " cmd))

(defun ciao-builder-command (cmd)
  "Execute the `cmd' ciao builder command"
  (async-shell-command
   (ciao-builder-cmdstr cmd)
   (ciao-get-builder-proc-buffer)))

;; TODO: Missing sub- and special targets (e.g., core.ciaobase, core.engine, etc.)
(defun ask-bundle (msg)
  (let
      ((bundle-list
	(split-string
	 (ciao-trim-end
	  (shell-command-to-string (ciao-builder-cmdstr "list")))
	 "\n")))
    (completing-read
     msg
     bundle-list
     nil t "")))

(defun ciao-trim-end (str)
  (replace-regexp-in-string (rx (* (any " \t\n")) eos) "" str))

;;;###autoload
(defun ciao-info () 
  "Show info about the specified Ciao bundle"
  (interactive)
  (let 
      ((target (ask-bundle "Show info about the specified Ciao bundle: ")))
    (ciao-builder-command (concat "info " target))))

;;;###autoload
(defun ciao-build () 
  "(Re)Build the specified Ciao bundle"
  (interactive)
  (let 
      ((target (ask-bundle "(Re)Build the specified Ciao bundle: ")))
    (ciao-builder-command (concat "build " target))))

;;;###autoload
(defun ciao-build-bin () 
  "(Re)Build the specified Ciao bundle [bin]"
  (interactive)
  (let 
      ((target (ask-bundle "(Re)Build the specified Ciao bundle [bin]: ")))
    (ciao-builder-command (concat "build --bin " target))))

;;;###autoload
(defun ciao-build-docs () 
  "(Re)Build the specified Ciao bundle (only docs)"
  (interactive)
  (let 
      ((target (ask-bundle "(Re)Build the specified Ciao bundle [docs]: ")))
    (ciao-builder-command (concat "build --docs " target))))

;;;###autoload
(defun ciao-install () 
  "Install the specified Ciao bundle"
  (interactive)
  (let 
      ((target (ask-bundle "Install the specified Ciao bundle: ")))
    (ciao-builder-command (concat "install " target))))

;;;###autoload
(defun ciao-uninstall () 
  "Uninstall the specified Ciao bundle"
  (interactive)
  (let 
      ((target (ask-bundle "Uninstall the specified Ciao bundle: ")))
    (ciao-builder-command (concat "uninstall " target))))

;;;###autoload
(defun ciao-clean () 
  "Clean the specified Ciao bundle"
  (interactive)
  (let 
      ((target (ask-bundle "Clean the specified Ciao bundle: ")))
    (ciao-builder-command (concat "clean " target))))

;;;###autoload
(defun ciao-dist () 
  "Prepare data for serving the specified Ciao bundle"
  (interactive)
  (let 
      ((target (ask-bundle "Prepare data for serving the specified Ciao bundle: ")))
    (ciao-builder-command (concat "custom_run " target " dist"))))

;; TODO: provide a minor mode for this; add "info"
;;;###autoload
(defun ciao-list-bundles () 
  "List the available bundles."
  (interactive) 
  (ciao-builder-command "list"))

;; ---------------------------------------------------------------------------
;; Extend relative bundle path

;; TODO: invalidate cache if CIAOPATH changes

;; Create a hash table for caching source paths
(defvar ciao--bundle-src-cache (make-hash-table :test 'equal))

(defun ciao-bundle-src (bundle)
  "Obtain the source directory of a bundle"
  (let ((src (gethash bundle ciao--bundle-src-cache)))
    (if src
        src
      (setq src (ciao-bundle-src-nocache bundle))
      (if src
          (progn
            (puthash bundle src ciao--bundle-src-cache)
            src)
        nil))))

(defun ciao-bundle-src-nocache (bundle)
  "Obtain the source directory of a bundle (calling the builder)"
  (message "extracting")
  (let ((str (shell-command-to-string 
              (ciao-builder-cmdstr (concat "info " bundle)))))
    (if (string-match "^  src: \\(.*\\)" str)
        (match-string 1 str)
      nil)))

(defun ciao-bundle-extend-path (filename)
  "Same as bundle_paths:bundle_extend_path/2 predicate"
  (if (string-match "^\\([a-zA-Z0-9_-]+\\)\\(/.*\\)" filename)
      (let* ((bundle (match-string 1 filename))
             (rest (match-string 2 filename))
             (src (ciao-bundle-src bundle)))
        (if src
            (concat src rest)
          filename))
    filename))

;; ---------------------------------------------------------------------------
;; Grep on bundles source
;; TODO: add tags-search

(defun ciao--all-workspaces ()
  "All workspaces (given CIAOPATH and CIAOROOT)"
  (append (mapcar 'directory-file-name
		  (parse-colon-path ciao-path-dirs))
	  (list ciao-root-dir)))

;;;###autoload
(defun ciao-grep-root ()
  "Run grep on Ciao source files at CIAOROOT"
  (interactive)
  (let ((re (read-from-minibuffer "Search Ciao code at CIAOROOT (Regexp): ")))
    (ciao--grep-common re (list ciao-root-dir))))

;;;###autoload
(defun ciao-grep-all ()
  "Run grep on Ciao source files at all workspaces"
  (interactive)
  (let ((dirs (ciao--all-workspaces))
	(re (read-from-minibuffer "Search Ciao code at all workspaces (Regexp): ")))
    (ciao--grep-common re dirs)))

;;;###autoload
(defun ciao-grep ()
  "Run grep on Ciao source files (at the default directory)"
  (interactive)
  (let ((re (read-from-minibuffer "Search Ciao code at the default directory (Regexp): ")))
      (ciao--grep-common re (list (expand-file-name default-directory)))))

(defun ciao--grep-common (regexp dirs)
  "Run grep with REGEXP on Ciao source files at directory DIR"
  (let* ((grep-cmd (concat ciao-root-dir "/core/cmds/grep-source.bash"))
	 (args (append (list grep-cmd "-e" regexp) dirs))
	 (cmdstr (mapconcat 'shell-quote-argument args " ")))
      (grep cmdstr)))

;;;###autoload
(defun ciao-grep-versions-all ()
  "Run grep for bundle version numbers at all workspaces"
  (interactive)
  (let ((dirs (ciao--all-workspaces)))
    (ciao--grep-versions-common dirs)))

(defun ciao--grep-versions-common (dirs)
  "Run grep for bundle version numbers at directory DIR"
  (let* ((grep-cmd (concat ciao-root-dir "/core/cmds/grep-versions.bash"))
	 (args (append (list grep-cmd) dirs))
	 (cmdstr (mapconcat 'shell-quote-argument args " ")))
      (grep cmdstr)))

;; TODO: define 'ciao-builder-build-all'?
;; TODO: define 'ciao-builder-update'? (should it download anything?)


;; Provide ourselves:

(provide 'ciao-builder)

;;; ciao-builder.el ends here

