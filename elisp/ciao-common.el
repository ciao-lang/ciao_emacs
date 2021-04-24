;;; ciao-common.el --- Common definitions for the Ciao mode
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

(require 'ciao-config) ; ciao-get-config

;; ---------------------------------------------------------------------------
;; Basic Ciao mode variables
;; ---------------------------------------------------------------------------

(defgroup ciao nil
  "The Ciao programming environment." 
  :tag "Ciao"
  :group 'emacs)

(defgroup ciao nil
  "The Ciao programming environment." 
  :tag "Ciao"
  :group 'languages)

;;---------------------------------------------------------------------------

(defgroup ciaoide nil
  "Ciao IDE behaviour customization."
  :tag "Ciao IDE"
  :group 'ciao)

;;---------------------------------------------------------------------------

(defgroup ciaocore nil
  "The Ciao core (compiler, toplevel, debugger, and core
libraries)."
  :tag "Ciao core"
  :group 'ciao)

(defgroup ciaopp nil
  "The Ciao preprocesor."
  :tag "CiaoPP"
  :group 'ciao)

(defgroup lpdoc nil
  "The LPdoc documentation generator."
  :tag "LPdoc"
  :group 'ciao)

;;---------------------------------------------------------------------------

;; 'ignore' is because custom passes id of symbol
(defun ciao-do-set-ciao-root (ignore dir) 
  (if (string= dir "") 
      (progn
	(setenv "CIAOROOT" nil)
	(setq ciao-root-dir ""))
    (setenv "CIAOROOT" dir)
    (setq ciao-root-dir dir)))

;; 'ignore' is because custom passes id of symbol
(defun ciao-do-set-ciao-path (ignore dir) 
  (if (string= dir "") 
      (progn
	(setenv "CIAOPATH" nil)
	(setq ciao-path-dirs ""))
    (setenv "CIAOPATH" dir)
    (setq ciao-path-dirs dir)))

(defun ciao-initialize-ciao-root (ignorea ignoreb) 
  (ciao-do-set-ciao-root nil (or (getenv "CIAOROOT") (ciao-get-config :root-dir))))

(defcustom ciao-root-dir ""
  "Path to the Ciao system path (reads/sets the CIAOROOT
environment variable ). Typically left empty, since Ciao
executables know its installation path."
  :group 'ciaocore
  :type 'string
  :initialize 'ciao-initialize-ciao-root
  :set 'ciao-do-set-ciao-root
  )

(defun ciao-set-ciao-root () 
  "Change the location of the Ciao system (changes the
   environment variable @tt{CIAOROOT})."
  (interactive)
  (ciao-do-set-ciao-root nil
   (read-file-name "Change Ciao root path (CIAOROOT)? " 
		   (getenv "CIAOROOT") (getenv "CIAOROOT"))))

(defun ciao-initialize-ciao-path (ignorea ignoreb) 
  (ciao-do-set-ciao-path nil (or (getenv "CIAOPATH") "")))

(defcustom ciao-path-dirs ""
  "Colon-separated paths to collections of Ciao
bundles (reads/sets the CIAOPATH environment variable ). Left
empty to use the default value."
  :group 'ciaocore
  :type 'string
  :initialize 'ciao-initialize-ciao-path
  :set 'ciao-do-set-ciao-path
  )

(defun ciao-set-ciao-path () 
  "Change the paths for Ciao bundles (changes the environment
variable @tt{CIAOPATH})."
  (interactive)
  (ciao-do-set-ciao-path nil
   (read-file-name "Change paths for Ciao bundles (CIAOPATH)? " 
		   (getenv "CIAOPATH") (getenv "CIAOPATH"))))

(defcustom ciao-system (or (getenv "CIAO") (ciao-get-config :ciaosh-bin))
  "Name of Ciao executable which runs the classical top level."
  :group 'ciaocore
  :type 'string)

(defun ciao-set-ciao-system () 
  "Change the Ciao executable used to run the top level. It is set by
default to @tt{ciao} or, to the environment variable @tt{CIAO} if it
is defined. @cindex{toplevel command, setting}" 
  (interactive)
  (setq ciao-system
	(read-file-name "Change Ciao top-level executable? " 
			ciao-system ciao-system)))

(defun ciao-system-args-interactive ()
    (if (string= system-type "windows-nt") "-i" ""))

(defcustom ciao-system-args 
  (or (getenv "CIAOARGS") 
      (ciao-system-args-interactive))
  "Arguments passed to Ciao toplevel executable."
  :group 'ciaocore
  :type 'string)

(defun ciao-set-ciao-system-args () 
  "Change the arguments passed to the Ciao executable. They are
set by default to none or, to the environment variable @tt{CIAOARGS} if it
is defined. @cindex{toplevel command args, setting}"
  (interactive)
  (setq ciao-system-args
	(read-string "Change args passed to Ciao executable? " 
		     ciao-system-args nil)))

;; ---------------------------------------------------------------------------
;; TODO: Redistribute

(defcustom ciao-toplevel-buffer-name "*Ciao*"
  "Basic name of the buffer running the Ciao toplevel inferior process."
  :group 'ciaocore
  :type 'string)

(defcustom ciao-lpdoc-buffer-name "*LPdoc*"
  "Basic name of the buffer running the auto-documenter inferior process."
  :group 'lpdoc
  :type 'string) 

(defcustom ciao-ciaopp-buffer-name "*CiaoPP*"
  "Basic name of the buffer running the CiaoPP inferior process."
  :group 'ciaopp
  :type 'string) 

;; ---------------------------------------------------------------------------
;; LPdoc variables
;; ---------------------------------------------------------------------------

(defcustom ciao-lpdoc-system (or (getenv "LPDOC") (ciao-get-config :lpdoc-bin))
  "Name of LPdoc auto-documenter executable."
  :group 'lpdoc
  :type 'string)

;;;###autoload
(defun ciao-set-lpdoc-system () 
  "Change the executable used to run the LPdoc auto-documenter. It is
set by default to @tt{lpdoc} or to the environment  
variable @tt{LPDOC} if it is defined. @cindex{lpdoc command, setting}
@cindex{auto-documenter command, setting}"
  (interactive)
  (setq ciao-lpdoc-system
	(read-file-name "Change Ciao LPdoc auto-documenter executable? "
   		        ciao-lpdoc-system ciao-lpdoc-system))) 

(defun ciao-lpdoc-system-args-interactive ()
  ;;(if (string= system-type "windows-nt") "-T -i" "-T"))
  "-T")

(defcustom ciao-lpdoc-system-args 
  (or (getenv "LPDOCARGS") 
      (ciao-lpdoc-system-args-interactive))
  "Arguments passed to LPdoc executable."
  :group 'lpdoc
  :type 'string)

;;;###autoload
(defun ciao-set-lpdoc-system-args () 
  "Change the arguments passed to the LPdoc auto-documenter. They are
set by default to none or to the environment variable @tt{LPDOCARGS} if it
is defined. @cindex{lpdoc command args, setting}
@cindex{auto-documenter command args, setting}" 
  (interactive)
  (setq ciao-lpdoc-system-args
	(read-string "Change args passed to LPdoc auto documenter executable? " 
		     ciao-lpdoc-system-args nil)))

;; ---------------------------------------------------------------------------
;; CiaoPP variables
;; ---------------------------------------------------------------------------

;; (defcustom ciao-ciaopp-system (or (getenv "CIAOPP") "ciaopp-1.0")
(defcustom ciao-ciaopp-system (or (getenv "CIAOPP") (ciao-get-config :ciaopp-bin))
  "Name of CiaoPP executable."
  :group 'ciaopp
  :type 'string)

(defun ciao-set-ciaopp-system () 
  "Change the executable used to run the CiaoPP
toplevel. It is set by default to @tt{ciaopp} or, to the environment 
variable @tt{CIAOPP} if it is defined. @cindex{preprocessor command, setting}"
  (interactive)
  (setq ciao-ciaopp-system
	(read-file-name "Change CiaoPP executable? "
   		        ciao-ciaopp-system ciao-ciaopp-system))) 

(defun ciao-ciaopp-system-args-interactive ()
    (if (string= system-type "windows-nt") "-T -i" "-T"))

(defcustom ciao-ciaopp-system-args 
  (or (getenv "CIAOPPARGS") 
      (ciao-ciaopp-system-args-interactive))
  "Arguments passed to CiaoPP executable."
  :group 'ciaopp
  :type 'string)

(defun ciao-set-ciaopp-system-args () 
  "Change the arguments passed to the CiaoPP executable. They are
set by default to none or to the environment variable @tt{CIAOPPARGS} if it
is defined. @cindex{preprocessor command args, setting}"
  (interactive)
  (setq ciao-ciaopp-system-args
	(read-string "Change args passed to CiaoPP executable? "
		     ciao-ciaopp-system-args nil))) 

(defcustom ciao-ciaopp-gmenu-buffer-name "*CiaoPP Interface*"
  "Name of the buffer running the CiaoPP graphical
menu interface."
  :group 'ciaopp
  :type 'string) 


;; Provide ourselves:

(provide 'ciao-common)

;;; ciao-common.el ends here


