;;; ciao-faces.el --- Faces for Ciao Mode

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

;;----------------------------------------------------------------------------
;; Font-lock support - (customizable) face definitions
;;----------------------------------------------------------------------------


;; DONE:
;; - Added list of standard faces, normally set by themes:
;;   should use them more (inherit by default, but let set)
;; - Set ciao code highlighting to use theme default (inherit)
;; TODO:
;; - Eliminate 'highlighting' from all variables names (really, leftover from hilit)

;; Reminder of tty colors:
;; black, red, green, yellow, blue, magenta, cyan, white
;; (tty-color-translate color) approximates the color

;; ;; Reminder of standard faces (these are normally set up by themes)
;; ;; A) standard faces (subset)
;; default
;; bold
;; italic
;; bold-italic
;; underline
;; fixed-pitch
;; fixed-pitch-serif
;; variable-pitch
;; shadow
;; 
;; Here's an incomplete list of faces used to highlight parts of the
;; text temporarily for specific purposes.
;; 
;; highlight
;;    This face is used for text highlighting in various contexts, such
;;    as when the mouse cursor is moved over a hyperlink.
;; isearch
;;    This face is used to highlight the current Isearch match (see Incremental Search).
;; query-replace
;;    This face is used to highlight the current Query Replace match (see Replace).
;; lazy-highlight
;;    This face is used to highlight lazy matches for Isearch and Query
;;    Replace (matches other than the current one).
;; region
;;    This face is used for displaying an active region (see Mark). 
;; secondary-selection
;;    This face is used for displaying a secondary X selection (see Secondary Selection).
;; trailing-whitespace
;;    The face for highlighting excess spaces and tabs at the end of a
;;    line when show-trailing-whitespace is non-nil (see Useless Whitespace).
;; escape-glyph
;;    The face for displaying control characters and escape sequences (see Text Display).
;; homoglyph
;;    The face for displaying lookalike characters, i.e., characters that
;;    look like but are not the characters being represented (see Text Display).
;; nobreak-space
;;    The face for displaying no-break space characters (see Text Display).
;; nobreak-hyphen
;;     The face for displaying no-break hyphen characters (see Text Display).
;; 
;; The following faces control the appearance of parts of the Emacs frame:
;; 
;; tty-menu-enabled-face
;;    This face is used to display enabled menu items on text-mode terminals.
;; tty-menu-disabled-face
;;    This face is used to display disabled menu items on text-mode terminals.
;; tty-menu-selected-face
;;    This face is used to display on text-mode terminals the menu item that
;;    would be selected if you click a mouse or press <RET>.
;; 
;; ;; B) font lock standard faces
;; font-lock-warning-face
;;      for a construct that is peculiar, or that greatly changes the
;;      meaning of other text, like ‘;;;###autoload’ in Emacs Lisp and ‘#error’ in C.
;; font-lock-function-name-face
;;      for the name of a function being defined or declared.
;; font-lock-variable-name-face
;;      for the name of a variable being defined or declared.
;; font-lock-keyword-face
;;       for a keyword with special syntactic significance, like ‘for’ and ‘if’ in C.
;; font-lock-comment-face
;;      for comments.
;; font-lock-comment-delimiter-face
;;      for comments delimiters, like ‘/*’ and ‘*/’ in C. On most terminals,
;;      this inherits from font-lock-comment-face.
;; font-lock-type-face
;;      for the names of user-defined data types.
;; font-lock-constant-face
;;      for the names of constants, like ‘NULL’ in C.
;; font-lock-builtin-face
;;      for the names of built-in functions.
;; font-lock-preprocessor-face
;;      for preprocessor commands. This inherits, by default, from font-lock-builtin-face.
;; font-lock-string-face
;;      for string constants.
;; font-lock-doc-face
;;      for documentation strings in the code. This inherits, by default, from font-lock-string-face.
;; font-lock-negation-char-face
;;      for easily-overlooked negation characters.


;; General faces group
(defgroup ciao-highlighting-faces nil
  "Ciao environment faces for syntax highlighting, debugger, etc."
  :tag "Ciao Faces"
  :group 'ciao)

;; Debugger
(defgroup ciao-highlighting-faces-debugger nil
  "Ciao faces for debugger."
  :tag "Ciao Debugger Faces" :group 'ciao-highlighting-faces)

;; This super-kludge of adding the unnecessary defvar is needed to 
(defvar ciao-face-debug-call 'ciao-face-debug-call)
(defface ciao-face-debug-call ;; ciao-face-blueish-block
  '(;;(((type tty) (class color)) (:background "blue" :foreground "white"))
    (((class color) (background dark)) (:background "blue3"))
    (((class color) (background light)) (:background "slate blue" :foreground "white"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:background "gray")))
  "Face to use when at call port in source debugger."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-exit 'ciao-face-debug-exit)
(defface ciao-face-debug-exit ;; ciao-face-greenish-block
  '(;;(((type tty) (class color)) (:background "green"))
    (((class color) (background light)) (:background "green"))
    (((class color) (background dark)) (:background "darkolivegreen"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t)))
  "Face to use when at exit port in source debugger."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-fail 'ciao-face-debug-fail)
(defface ciao-face-debug-fail ;; ciao-face-reddish-block
  '(;;(((type tty) (class color)) (:background "red" :foreground "black"))
    (((class color) (background light)) (:background "Firebrick" :foreground "White"))
    (((class color) (background dark)) (:background "Firebrick" :foreground "White"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t)))
  "Face to use when at fail port in source debugger."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-redo 'ciao-face-debug-redo)
(defface ciao-face-debug-redo ;; ciao-face-orangy-block
  '(;;(((type tty) (class color)) (:background "magenta" :foreground "black"))
    (((class color) (background light)) (:background "orange"))
    (((class color) (background dark)) (:background "orange" :foreground "black"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t)))
  "Face to use when at redo port in source debugger."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-expansion 'ciao-face-debug-expansion)
(defface ciao-face-debug-expansion ;; ciao-face-yellowish-block
  '(;;(((type tty) (class color)) (:background "yellow" :foreground "black"))
    (((class color) (background light)) (:background "yellow" :foreground "black"))
    (((class color) (background dark)) (:background "yellow" :foreground "black"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t)))
  "Face to use in source debugger when source literal not located."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-breakpoint 'ciao-face-debug-breakpoint)
(defface ciao-face-debug-breakpoint ;; ciao-face-warning
  '(;;(((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :weight bold))
    (((class color) (background dark)) (:foreground "Red" :weight bold))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t :weight bold)))
  "Face to use with breakpoints in source debugger."
  :group 'ciao-highlighting-faces-debugger)

;; Misc language stuff
(defgroup ciao-highlighting-faces-misc nil
  "Ciao faces for miscellanous language features."
  :tag "Ciao Misc Faces" :group 'ciao-highlighting-faces)

;; TODO: why? ;; resolve an emacs / xemacs incompatibility
(defvar ciao-face-script-header 'ciao-face-script-header)
(defface ciao-face-script-header ;; ciao-face-forestgreen
  '(;;(((type tty) (class color)) (:foreground "green" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "lime green"))
    (t (:inverse-video t)))
  "Face to use for script headers."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-quoted-atom 'ciao-face-quoted-atom)
(defface ciao-face-quoted-atom ;; ciao-face-quoted-atom
  '(;;(((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
;    (((class color) (background light)) (:foreground "brown"))
    (((class color) (background light)) (:foreground "gray40"))
    (((class color) (background dark)) (:foreground "gray90"))
    (t (:italic t)))
  "Face to use for quoted atoms."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-funexp-atom 'ciao-face-funexp-atom)
(defface ciao-face-funexp-atom ;; ciao-face-funexp-atom
  '(;;(((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "OliveDrab"))
    (((class color) (background dark)) (:foreground "OliveDrab2"))
    (t (:italic t)))
  "Face to use for atoms in functional notation."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-variable 'ciao-face-variable)
(defface ciao-face-variable ;; ciao-face-variable
  '(;;(((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "sienna"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (t (:italic t)))
  "Face to use for variables."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-string 'ciao-face-string)
(defface ciao-face-string ;; ciao-face-string
  '(;;(((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face to use for strings."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-comment 'ciao-face-comment)
(defface ciao-face-comment ;; ciao-face-comment
  '(;;(((type tty) (class color)) (:foreground "red"))
    (((class grayscale) (background light)) (:foreground "DimGray" :weight bold :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :weight bold :italic t))
    (((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "SlateGray4")) ;; chocolate1
    (t (:weight bold :italic t)))
  "Face to use for code comments using fixed pitch (double %)."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-clauseheadname 'ciao-face-clauseheadname)
(defface ciao-face-clauseheadname ;; ciao-face-blue
  '(;;(((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:inverse-video t :weight bold)))
  "Face to use for clause head functors."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-concurrency-op 'ciao-face-concurrency-op)
(defface ciao-face-concurrency-op ;; ciao-face-coral-bold
  '(;;(((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Coral" :weight bold))
    (((class color) (background dark)) (:foreground "Coral" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for concurrency operators."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-cut 'ciao-face-cut)
(defface ciao-face-cut ;; ciao-face-royalblue
  '(;;(((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "RoyalBlue"))
    (((class color) (background dark)) (:foreground "CornflowerBlue"))
    (t (:inverse-video t)))
  "Face to use for cuts."
  :group 'ciao-highlighting-faces-misc)

;; LPdoc
(defgroup ciao-highlighting-faces-lpdoc nil
  "Ciao faces for documenter-specific assertions (comments, text
strings, commnds, etc.)."
  :tag "Ciao LPdoc Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-lpdoc-bug-comment 'ciao-face-lpdoc-bug-comment)
(defface ciao-face-lpdoc-bug-comment ;; ciao-face-warning
  '(;;(((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :weight bold))
    (((class color) (background dark)) (:foreground "Red" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for LPdoc bug comments."
  :group 'ciao-highlighting-faces-lpdoc)

; (defvar ciao-face-lpdoc-version-comment 'ciao-face-lpdoc-version-comment)
; (defface ciao-face-lpdoc-version-comment ;; ciao-face-comment
;   '(;;(((type tty) (class color)) (:foreground "red"))
;     (((class grayscale) (background light))
;      (:foreground "DimGray" :weight bold :italic t))
;     (((class grayscale) (background dark))
;      (:foreground "LightGray" :weight bold :italic t))
;     (((class color) (background light)) (:foreground "Firebrick"))
;     (((class color) (background dark)) (:foreground "chocolate1"))
;     (t (:weight bold :italic t)))
;   "Face to use for LPdoc version comments."
;   :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-comment 'ciao-face-lpdoc-comment)
(defface ciao-face-lpdoc-comment ;; ciao-face-navyblue
  '(;;(((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "NavyBlue"))
    (((class color) (background dark)) (:foreground "SlateGray3")) ;; RoyalBlue
    (t (:inverse-video t)))
  "Face to use for LPdoc textual comments."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-verbatim 'ciao-face-lpdoc-verbatim)
(defface ciao-face-lpdoc-verbatim ;; ciao-face-navyblue-bold
  '(;;(((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "NavyBlue" :weight bold))
    (((class color) (background dark)) (:foreground "SlateGray1" :weight bold)) ;; RoyalBlue
    (t (:inverse-video t :weight bold)))
  "Face to use for LPdoc verbatim text."
  :group 'ciao-highlighting-faces-lpdoc)

; (defvar ciao-face-lpdoc-include 'ciao-face-lpdoc-include)
; (defface ciao-face-lpdoc-include ;; ciao-face-navyblue-bold
;   '(;;(((type tty) (class color)) (:foreground "blue" :weight bold))
;     (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
;     (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
;     (((class color) (background light)) (:foreground "NavyBlue" :weight bold))
;     (((class color) (background dark)) (:foreground "SlateGray1" :weight bold)) ;; RoyalBlue
;     (t (:inverse-video t :weight bold)))
;   "Face to use for LPdoc include commands."
;   :group 'ciao-highlighting-faces-lpdoc)

; (defvar ciao-face-lpdoc-crossref 'ciao-face-lpdoc-crossref)
; (defface ciao-face-lpdoc-crossref ;; ciao-face-golden
;   '(;;(((type tty) (class color)) (:foreground "blue" :weight light))
;     (((class grayscale) (background light))
;      (:foreground "Gray90" :weight bold :italic t))
;     (((class grayscale) (background dark))
;      (:foreground "DimGray" :weight bold :italic t))
;     (((class color) (background light)) (:foreground "DarkGoldenrod"))
;     (((class color) (background dark)) (:foreground "LightGoldenrod"))
;     (t (:weight bold :italic t)))
;   "Face to use for LPdoc cross-references."
;   :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-command 'ciao-face-lpdoc-command)
(defface ciao-face-lpdoc-command ;; ciao-face-royalblue
  '(;;(((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "RoyalBlue"))
    (((class color) (background dark)) (:foreground "SlateGray1")) ;; CornflowerBlue
    (t (:inverse-video t)))
  "Face to use LPdoc commands inserted in documentation text."
  :group 'ciao-highlighting-faces-lpdoc)

;; Directives
(defgroup ciao-highlighting-faces-directive nil
  "Ciao faces for various directives (:- ...)."
  :tag "Ciao Directives Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-builtin-directive 'ciao-face-builtin-directive)
(defface ciao-face-builtin-directive ;; ciao-face-blue-bold
  '(;;(((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Blue" :weight bold))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for the standard directives."
  :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-predicate-directive 'ciao-face-predicate-directive)
(defface ciao-face-predicate-directive ;; ciao-face-royalblue
  '(;;(((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "RoyalBlue"))
    (((class color) (background dark)) (:foreground "DodgerBlue1")) ;; CornflowerBlue
    (t (:inverse-video t)))
  "Face to use for the predicate-related directives."
  :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-module-directive 'ciao-face-module-directive)
(defface ciao-face-module-directive ;; ciao-face-navyblue-bold
  '(;;(((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Purple3" :weight bold))
    (((class color) (background dark)) (:foreground "mediumpurple2" :weight bold)) ;; RoyalBlue
    (t (:inverse-video t :weight bold))
     )
     "Face to use for the module-related directives."
     :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-condcode-directive 'ciao-face-condcode-directive)
(defface ciao-face-condcode-directive ;; ciao-face-navyblue-bold
  '(;;(((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "NavyBlue" :weight bold))
    (((class color) (background dark)) (:foreground "mediumpurple2" :weight bold)) ;; RoyalBlue
    (t (:inverse-video t :weight bold))
     )
     "Face to use for the conditional code directives."
     :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-library-directive 'ciao-face-library-directive)
(defface ciao-face-library-directive ;; ciao-face-navyblue-bold
  '(;;(((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "NavyBlue" :weight bold))
    (((class color) (background dark)) (:foreground "DodgerBlue1" :weight bold)) ;; RoyalBlue
    (t (:inverse-video t :weight bold)))
  "Face to use for directives defined in the library."
  :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-user-directive 'ciao-face-user-directive)
(defface ciao-face-user-directive ;; ciao-face-navyblue
  '(;;(((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "NavyBlue"))
    (((class color) (background dark)) (:foreground "DodgerBlue1")) ;; RoyalBlue
    (t (:inverse-video t)))
  "Face to use for directives defined by the user (see
   ciao-user-directives custom variable to add new ones)."
  :group 'ciao-highlighting-faces-directive)

;; Assertions
(defgroup ciao-highlighting-faces-assertions nil
  "Ciao faces for Ciao assertions."
  :tag "Ciao Assertions Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-checked-assrt 'ciao-face-checked-assrt)
(defface ciao-face-checked-assrt ;; ciao-face-darkgreen-bold
  '(;;(((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "DarkGreen" :weight bold))
    (((class color) (background dark)) (:foreground "LightGreen" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for checked assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-true-assrt 'ciao-face-true-assrt)
(defface ciao-face-true-assrt ;; ciao-face-forestgreen-bold
  '(;;(((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "ForestGreen" :weight bold))
    (((class color) (background dark)) (:foreground "lime green" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for true assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-false-assrt 'ciao-face-false-assrt)
(defface ciao-face-false-assrt ;; ciao-face-warning
  '(;;(((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :weight bold))
    (((class color) (background dark)) (:foreground "Red" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for false assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-trust-assrt 'ciao-face-trust-assrt)
(defface ciao-face-trust-assrt ;; ciao-face-coral-bold
  '(;;(((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Coral" :weight bold))
    (((class color) (background dark)) (:foreground "Coral" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for trust assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-entry-assrt 'ciao-face-entry-assrt)
(defface ciao-face-entry-assrt ;; ciao-face-brown-bold
  '(;;(((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "brown" :weight bold))
    (((class color) (background dark)) (:foreground "salmon1" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for entry assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-check-assrt 'ciao-face-check-assrt)
(defface ciao-face-check-assrt ;; ciao-face-navyblue-bold
  '(;;(((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "NavyBlue" :weight bold))
    (((class color) (background dark)) (:foreground "DodgerBlue1" :weight bold)) ;; RoyalBlue
    (t (:inverse-video t :weight bold)))
  "Face to use for check assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-prop-assrt 'ciao-face-prop-assrt)
(defface ciao-face-prop-assrt ;; ciao-face-blue-bold
  '(;;(((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Blue" :weight bold))
    (((class color) (background dark)) (:foreground "DodgerBlue1" :weight bold)) ;; LightSkyBlue
    (t (:inverse-video t :weight bold)))
  "Face to use for property definitions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-test-assrt 'ciao-face-test-assrt)
(defface ciao-face-test-assrt ;; ciao-face-blue-bold
  '(;;(((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "GreenYellow" :weight bold))
    (((class color) (background dark)) (:foreground "GreenYellow" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for test assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-texec-assrt 'ciao-face-texec-assrt)
(defface ciao-face-texec-assrt ;; ciao-face-blue-bold
  '(;;(((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "GreenYellow" :weight bold))
    (((class color) (background dark)) (:foreground "GreenYellow" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for texec assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-type-assrt 'ciao-face-type-assrt)
(defface ciao-face-type-assrt ;; ciao-face-mediumblue-bold
  '(;;(((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "MediumBlue" :weight bold))
    (((class color) (background dark)) (:foreground "DodgerBlue1" :weight bold)) ;; SkyBlue
    (t (:inverse-video t :weight bold)))
  "Face to use for type definitions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-modedef-assrt 'ciao-face-modedef-assrt)
(defface ciao-face-modedef-assrt ;; ciao-face-forestgreen-bold
  '(;;(((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "ForestGreen" :weight bold))
    (((class color) (background dark)) (:foreground "lime green" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for modedef definitions."
  :group 'ciao-highlighting-faces-assertions)

;; Top levels (Ciao, CiaoPP, LPdoc)
(defgroup ciao-highlighting-faces-toplevels nil
  "Ciao faces for the Ciao, CiaoPP, LPdoc and shell top levels."
  :tag "Ciao Top Levels Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-prompt 'ciao-face-prompt)
(defface ciao-face-prompt ;; ciao-face-coral-bold
  '(;;(((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Coral" :weight bold))
    (((class color) (background dark)) (:foreground "Coral" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for prompts in top-level and shells."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-answer-var 'ciao-face-answer-var)
(defface ciao-face-answer-var ;; ciao-face-purple
  '(;;(((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:weight bold)))
  "Face to use for answer variables in top level."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-answer-val 'ciao-face-answer-val)
(defface ciao-face-answer-val ;; ciao-face-blue-bold
  '(;;(((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Blue" :weight bold))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for answer values in top level."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-yes-answer 'ciao-face-yes-answer)
(defface ciao-face-yes-answer ;; ciao-face-forestgreen-bold
  '(;;(((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "ForestGreen" :weight bold))
    (((class color) (background dark)) (:foreground "lime green" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for yes answer in top level."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-no-answer 'ciao-face-no-answer)
(defface ciao-face-no-answer ;; ciao-face-golden-bold
  '(;;(((type tty) (class color)) (:foreground "red" :weight light))
    (((class grayscale) (background light)) (:foreground "Gray90" :weight bold :italic t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold :italic t))
    (((class color) (background light)) (:foreground "DarkGoldenrod" :weight bold))
    (((class color) (background dark)) (:foreground "LightGoldenrod" :weight bold))
    (t (:weight bold :italic t)))
  "Face to use for no answer in top level."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-ciaopp-option 'ciao-face-ciaopp-option)
(defface ciao-face-ciaopp-option ;; ciao-face-forestgreen-bold
  '(;;(((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "ForestGreen" :weight bold))
    (((class color) (background dark)) (:foreground "lime green" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for CiaoPP option menus."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-startup-message 'ciao-face-startup-message)
(defface ciao-face-startup-message ;; ciao-face-forestgreen-bold
  '(;;(((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "DodgerBlue4" :weight bold :height 1.1)) ; :family "helv" 
    (((class color) (background dark)) (:foreground "LightBlue2" :height 1.1)) ; :family "helv" 
    (t (:inverse-video t :weight bold)))
  "Face to use for system splash message."
  :group 'ciao-highlighting-faces-toplevels)

;; Messages
(defgroup ciao-highlighting-faces-messages nil
  "Ciao faces for various messages (errors, warnings, notes, etc.)."
  :tag "Ciao Messages Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-debug-mess 'ciao-face-debug-mess)
(defface ciao-face-debug-mess ;; ciao-face-forestgreen-bold
  '(;;(((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "ForestGreen" :weight bold)) ; :family "helv"
    (((class color) (background dark)) (:foreground "lime green" :weight bold)) ; :family "helv"
    (t (:inverse-video t :weight bold)))
  "Face to use for debug messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-error-mess 'ciao-face-error-mess)
(defface ciao-face-error-mess ;; ciao-face-warning
  '(;;(((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :weight bold)) ; :family "helv"
    (((class color) (background dark)) (:foreground "Red" :weight bold)) ; :family "helv"
    (t (:inverse-video t :weight bold)))
  "Face to use for error messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-warning-mess 'ciao-face-warning-mess)
(defface ciao-face-warning-mess ;; ciao-face-brown-bold
  '(;;(((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "brown" :weight bold)) ; :family "helv"
    (((class color) (background dark)) (:foreground "salmon1" :weight bold)) ; :family "helv"
    (t (:inverse-video t :weight bold)))
  "Face to use for warning messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-note-mess 'ciao-face-note-mess)
(defface ciao-face-note-mess ;; ciao-face-brown
  '(;;(((type tty) (class color)) (:foreground "cyan" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "brown")) ; :family "helv"
    (((class color) (background dark)) (:foreground "salmon1")) ; :family "helv"
    (t (:inverse-video t)))
  "Face to use for note messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-passed-mess 'ciao-face-passed-mess)
(defface ciao-face-passed-mess ;; ciao-face-forestgreen-bold
  '(;;(((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "ForestGreen" :weight bold))
    (((class color) (background dark)) (:foreground "lime green" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for passed teset messages." ;; copied from ciao-face-yes-answer
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-failed-mess 'ciao-face-failed-mess)
(defface ciao-face-failed-mess ;; ciao-face-warning
  '(;;(((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :weight bold)) ; :family "helv"
    (((class color) (background dark)) (:foreground "Red" :weight bold)) ; :family "helv"
    (t (:inverse-video t :weight bold)))
  "Face to use for failed test messages." ;; copied from ciao-face-error-mess
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-aborted-mess 'ciao-face-aborted-mess)
(defface ciao-face-aborted-mess ;; ciao-face-warning
  '(;;(((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :weight bold)) ; :family "helv"
    (((class color) (background dark)) (:foreground "Red" :weight bold)) ; :family "helv"
    (t (:inverse-video t :weight bold)))
  "Face to use for aborted test messages." ;; copied from ciao-face-error-mess
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-other-mess 'ciao-face-other-mess)
(defface ciao-face-other-mess ;; ciao-face-brown
  '(;;(((type tty) (class color)) (:foreground "cyan" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "brown")) ; :family "helv"
    (((class color) (background dark)) (:foreground "salmon1")) ; :family "helv"
    (t (:inverse-video t)))
  "Face to use for other messages."
  :group 'ciao-highlighting-faces-messages)

;; (defvar ciao-face-highlight-code 'highlight) ; Alt: 'region
(defvar ciao-face-highlight-code 'ciao-face-highlight-code)
(defface ciao-face-highlight-code
  '(;;(((type tty) (class color)) (:background "yellow" :foreground "black"))
;;     (((class color) (background light)) (:background "yellow" :foreground "black"))
;;     (((class color) (background dark)) (:background "SlateGray2" :foreground "black"))
;;     (((class color) (background light)) (:inherit 'region))
;;     (((class color) (background dark)) (:inherit 'region)))
      (((class color)) (:inherit region)))
    ; (t (:inverse-video t)))
  "Face to use for highlighting code areas (e.g., when locating 
   the code area that an error message refers to)."
  :group 'ciao-highlighting-faces-messages)

;; Definitions of title faces adapted from font-latex.el --JFMC

(defconst ciao-face-sectioning-max 5
  "Highest number for ciao-face-sectioning-N-face")
(defvar ciao-face-sectioning-5-face 'ciao-face-sectioning-5-face)
(defface ciao-face-sectioning-5-face
  '(;;(((type tty pc) (class color) (background light)) (:foreground "blue4" :weight bold))
    ;;(((type tty pc) (class color) (background dark)) (:foreground "yellow" :weight bold))
    (((class color) (background light)) (:weight bold :inherit variable-pitch :foreground "NavyBlue"))
    (((class color) (background dark)) (:weight bold :inherit variable-pitch :foreground "LightGoldenrod1")) ;; CornflowerBlue
    (t (:weight bold :inherit variable-pitch)))
  "Face for sectioning commands at level 5."
  :group 'ciao-highlighting-faces-lpdoc)

(defcustom ciao-face-fontify-sectioning 1.1
  "Whether to fontify sectioning macros with varying height or a color face.

If it is a number, use varying height faces.  The number is used
for scaling starting from `ciao-face-sectioning-5-face'.  Typically
values from 1.05 to 1.3 give best results, depending on your font
setup.  If it is the symbol `color', use `font-lock-type-face'.

Caveats: Customizing the scaling factor applies to all sectioning
faces unless those faces have been saved by customize.  Setting
this variable directly does not take effect unless you call
`ciao-face-update-sectioning-faces' or restart Emacs.

Switching from `color' to a number or vice versa does not take
effect unless you call \\[font-lock-fontify-buffer] or restart
Emacs."
  :type '(choice (number :tag "Scale factor")
                 (const color))
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (unless (eq value 'color)
	   (ciao-face-update-sectioning-faces ciao-face-sectioning-max value)))
  :group 'ciao-faces)

(defun ciao-face-update-sectioning-faces (&optional max height-scale)
  "Update sectioning commands faces."
  (unless height-scale
    (setq height-scale (if (numberp ciao-face-fontify-sectioning)
			   ciao-face-fontify-sectioning
			 1.1)))
  (unless max
    (setq max ciao-face-sectioning-max))
  (dotimes (num max)
    (let* ((num (- max (1+ num)))
	   (face-name (intern (format "ciao-face-sectioning-%s-face" num))))
      (unless (get face-name 'saved-face) ; Do not touch customized faces.
        (set-face-attribute face-name nil :height  height-scale)))))

(defun ciao-face-make-sectioning-faces (max &optional height-scale)
  "Build the faces used to fontify sectioning commands."
  (unless max (setq max ciao-face-sectioning-max))
  (unless height-scale
    (setq height-scale (if (numberp ciao-face-fontify-sectioning)
			   ciao-face-fontify-sectioning
			 1.1)))
  (dotimes (num max)
    (let* ((num (- max (1+ num)))
	   (face-name (intern (format "ciao-face-sectioning-%s-face" num)))
	   (f-inherit (intern (format "ciao-face-sectioning-%s-face" (1+ num)))))
      (eval
       `(defface ,face-name
          '((t (:height ,height-scale :inherit ,f-inherit)))
	  (format "Face for sectioning commands at level %s.

Probably you don't want to customize this face directly.  Better
change the base face `ciao-face-sectioning-5-face' or customize the
variable `ciao-face-fontify-sectioning'." num)
	  :group 'ciao-faces)))))

(ciao-face-make-sectioning-faces ciao-face-sectioning-max)


;; Provide ourselves:

(provide 'ciao-faces)

;;; ciao-faces.el ends here

