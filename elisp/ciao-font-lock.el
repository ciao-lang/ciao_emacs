;;; ciao-font-lock.el --- Font locking module for Ciao Mode

;; Copyright (C) 1986-2012 Free Software Foundation, Inc. and
;; M. Hermenegildo and others (herme@fi.upm.es, UPM-CLIP, Spain).
;;
;; Authors: 2019 Jose F. Morales <jfmcjf@gmail.com>
;;               (modernized font-lock support)  

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

(require 'font-lock)

(require 'ciao-faces)
(require 'ciao-syntax) ; ciao-any-prompt-pattern

;;----------------------------------------------------------------------------

(defun ciao-emacs-can-do-font-lock-p ()
  "Fontifying is supported (possible in windowing system,
modern emacses, and also in ascii mode with emacs>= 21.1)."
  (or window-system (fboundp 'tool-bar-mode)))

;;----------------------------------------------------------------------------

;; TODO: this should not be needed nowadays

;; Just a bridge (for documentation and setting local binding)
;; but better than font-lock-fontify-buffer
(defun ciao-fontify-buffer ()
  "Undate (recompute) syntax-based highlighting (coloring)."
  (interactive)
  (save-excursion
    (font-lock-fontify-region (point-min) (point-max))))

;;----------------------------------------------------------------------------
;; Regular expressions and matching
;;----------------------------------------------------------------------------
;; TODO: move to ciao-syntax

(defvar ciao-predicate-directives
  '( "data" "dynamic" "multifile" "impl_defined" "meta_predicate"
     "discontiguous" "persistent"
     ;; moved as predicate directives (JFMC)
     "export" "redefining" 
     ;; indexer package (JFMC)
     "index"
     ;; new OO-module system (JFMC)
     "public" "constructor" "constant" "static" "attr" "mut" "fluid" "virtual"
     ;; O'Ciao
     "inheritable"
     )
  "Names of directives describing properties of predicates.")

(defvar ciao-module-directives
  '( "module" "package" "bundle"
     ;; new OO-module system (JFMC)
     "class" "interface" "mixin" 
     )
  "Names of module (and similar) directives.")

(defvar ciao-use-module-directives
  '( "use_module" "ensure_loaded"
     "use_package" "include" "use_foreign_library" "use_foreign_source"
     "reexport" "import"
     "initialization" "on_abort"
     ;; new OO-module system (JFMC)
     "use_class"
     "extends"
     ;; ociao
     "implements" "inherit_class"
     )
  "Names of use_module (and similar) directives.")

(defvar ciao-builtin-directives
  '( "new_declaration" "op" 
     "load_compilation_module" "add_sentence_trans" "add_term_trans"
     "add_clause_trans" "add_goal_trans"
     "set_prolog_flag" "push_prolog_flag" "pop_prolog_flag" 
     ;; Resources and Granularity
     "compound_resource" "platform_constants" "platform"
     "load_resource_module" "resource" "head_cost" "literal_cost"
     "trust_default" "granularity_resources" "default_cost"
     ;; Unit-Testing
     "load_test_module"
     "load_test_package"
     ;; Foreign-interface
     "extra_compiler_opts" "extra_linker_opts"
     )
  "Names of other directives.")

(defvar ciao-condcode-directives
  '( ;; Conditional code (package(condcomp))
     "if" "else" "elif" "endif" "compilation_fact"
     )
  "Names of directives to define conditional code.")

(defvar ciao-library-directives
  '(
    ;; functions
    "fun_eval" "fun_return" "lazy" "funct" 
    ;; backwards compatibility:
    "function"
    ;; argnames
    "argnames" 
    ;; make
    "make" 
    )
  "Names of additional directives defined in the libraries.")
 
(defcustom ciao-user-directives '( "mydirective" )
  "List of identifiers of any directives defined by users which you
would like highlighted (colored). Be careful, since wrong entries may
affect other syntax highlighting."
  :group 'ciaolang
  :type 'list)
;; Also, 'ciao-user-directives' now customizable; see above in file.

(defun ciao-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for Ciao mode."
  ;; MR added to support font-lock
  (if (ciao-emacs-can-do-font-lock-p)
      (setq-local font-lock-defaults
                  '(ciao-mode-font-lock-keywords
                    nil nil ((?_ . "w")) nil
                    (font-lock-syntactic-face-function
                     . ciao-syntactic-face-function)))))

(defun ciao-mode-font-lock-keywords ()
  `(
    ;; scripts shebang
    ("^#!.*$" . ciao-face-script-header)
    ;; ----------------------------------------
    ;; mark TODO comments ; TODO: only if inside a comment?
    ("% \\(TODO\\):" 1 'ciao-face-lpdoc-bug-comment t)
    ;; ----------------------------------------
    ;; :- doc
    (,(ciao-direct-regexp "doc") 0 'ciao-face-lpdoc-comment keep)
    ;; bug doc command in :- doc
    (,(ciao-doc-regexp "\\(bug\\)[ \t\n]*,") 1 'ciao-face-lpdoc-bug-comment keep)
    ;; any doc command in :- doc
    (,(ciao-doc-regexp "\\([a-zA-Z0-9_]*\\)[ \t\n]*,") 1 'ciao-face-lpdoc-command keep)
    ;; override face of lpdoc title, section, subsection, subsubsection comments
    (,(ciao-section-text-regexp "title") 1 'ciao-face-sectioning-2-face t)
    (,(ciao-section-text-regexp "section") 1 'ciao-face-sectioning-3-face t)
    (,(ciao-section-text-regexp "subsection") 1 'ciao-face-sectioning-4-face t)
    (,(ciao-section-text-regexp "subsubsection") 1 'ciao-face-sectioning-5-face t)
    ;; \section, #, ##, etc. inside documentation comments
    ;; We use (ciao-font-lock-simple-match-in-doc) to match text
    ;; already marked as LPdoc documentation comments.
    ;; The face is overridden by using the t parameter (override)
    ;; title and section faces (do before commands are detected)
    ((lambda (limit)
       (ciao-font-lock-simple-match-in-doc
        limit
        (ciao-doc-section-command-text-regexp "\\(@\\|\\\\\\)title\\>")))
     3 'ciao-face-sectioning-2-face t)
    ((lambda (limit)
       (ciao-font-lock-simple-match-in-doc
        limit
        (ciao-doc-section-command-text-regexp "#[^#]")))
     2 'ciao-face-sectioning-3-face t)
    ((lambda (limit)
       (ciao-font-lock-simple-match-in-doc
        limit
        (ciao-doc-section-command-text-regexp "##[^#]")))
     2 'ciao-face-sectioning-4-face t)
    ((lambda (limit)
       (ciao-font-lock-simple-match-in-doc
        limit
        (ciao-doc-section-command-text-regexp "###[^#]")))
     2 'ciao-face-sectioning-5-face t)
    ;; Code blocks
    ;; TODO: not handled correctly, use ciao-syntactic-face-function
    ;; TODO: ```...``` blocks inside doccomments should ignore first %, etc.
;    ((lambda (limit) 
;       (ciao-font-lock-match-in-doc limit "\\(@\\|\\\\\\)begin{verbatim}" "\\(@\\|\\\\\\)end{verbatim}"))
;     0 ciao-face-lpdoc-verbatim t)
;    ((lambda (limit) 
;       (ciao-font-lock-match-in-doc limit "```" "```"))
;     0 ciao-face-lpdoc-verbatim t)
    ;;
    ;; `...` in markdown ;; TODO: improve
    ((lambda (limit) 
       (ciao-font-lock-simple-match-in-doc
        limit
        "`[^`\n]+`"))
     0 ciao-face-lpdoc-verbatim t)
    ;; \bug command 
    ((lambda (limit)
       (ciao-font-lock-simple-match-in-doc
        limit
        "\\(@\\|\\\\\\)bug\\>"))
     0 ciao-face-lpdoc-bug-comment t)
    ;; \<<Any>> command
    ((lambda (limit)
       (ciao-font-lock-simple-match-in-doc
        limit
        "\\(@\\|\\\\\\)\\([}{@\\\\]\\|[A-Za-z_]+\\>\\|[?!]\\)"))
     0 ciao-face-lpdoc-command t)
    ;; ----------------------------------------
    ;; fsyntax (funexp) atoms (~foobar)
    ("\\(~[a-z][A-Za-z0-9_\\.^/]*[A-Za-z0-9_^/]\\|~[a-z]\\)" 0 ciao-face-funexp-atom)
    ;; Characters 0'...
    ("0'\\(\\\\.\\|.\\)" 0 ciao-face-string)
    ;; ----------------------------------------
    ;; Directives
    (,(ciao-direct-regexp (concat "\\(public\\)?[ \t\n]*" (regexp-opt ciao-module-directives t)))
     0 ciao-face-module-directive keep)
    (,(ciao-direct-regexp (regexp-opt ciao-predicate-directives t))
     0 ciao-face-predicate-directive keep)
    (,(ciao-direct-regexp (regexp-opt ciao-builtin-directives t))
     0 ciao-face-builtin-directive keep)
    (,(ciao-direct-regexp (regexp-opt ciao-use-module-directives t))
     0 ciao-face-module-directive keep)
    (,(ciao-direct-regexp (regexp-opt ciao-condcode-directives t))
     0 ciao-face-condcode-directive keep)
    (,(ciao-direct-regexp (regexp-opt ciao-library-directives t))
     0 ciao-face-library-directive keep)
    (,(ciao-direct-regexp (regexp-opt ciao-user-directives t))
     0 ciao-face-user-directive keep)
    ;; ----------------------------------------
    ;; Assertion state
    (,(ciao-assrt-kind-regexp "checked") 1 ciao-face-checked-assrt keep)
    (,(ciao-assrt-kind-regexp "true") 1 ciao-face-true-assrt keep)
    (,(ciao-assrt-kind-regexp "false") 1 ciao-face-false-assrt keep)
    (,(ciao-assrt-kind-regexp "trust") 1 ciao-face-trust-assrt keep)
    (,(ciao-assrt-kind-regexp "check") 1 ciao-face-check-assrt keep)
    ;; Program point assertions
    (,(ciao-assrt-pp-kind-regexp "true") 1 ciao-face-true-assrt keep)
    (,(ciao-assrt-pp-kind-regexp "false") 1 ciao-face-false-assrt keep)
    (,(ciao-assrt-pp-kind-regexp "trust") 1 ciao-face-trust-assrt keep)
    (,(ciao-assrt-pp-kind-regexp "check") 1 ciao-face-check-assrt keep)
    (,(ciao-assrt-pp-kind-regexp "checked") 1 ciao-face-checked-assrt keep)
    ;; Assertions
    (,(ciao-direct-regexp
       (concat "[a-zA-Z0-9 \t]*\\<" ; accept any status prefix
               (regexp-opt '("decl" "pred" "comp" "calls" "success" "test" "texec") t)))
     0 ciao-face-check-assrt keep)
    (,(ciao-direct-regexp "prop") 0 ciao-face-prop-assrt keep)
    (,(ciao-direct-regexp "test") 0 ciao-face-test-assrt keep)
    (,(ciao-direct-regexp "texec") 0 ciao-face-texec-assrt keep)
    (,(ciao-direct-regexp "regtype") 0 ciao-face-type-assrt keep)
    (,(ciao-direct-regexp "mtype") 0 ciao-face-type-assrt keep) ;; experimental --JF
    (,(ciao-direct-regexp "mprop") 0 ciao-face-type-assrt keep) ;; experimental --JF
    (,(ciao-direct-regexp "entry") 0 ciao-face-entry-assrt keep)
    (,(ciao-direct-regexp "modedef") 0 ciao-face-modedef-assrt keep)
    ;; ----------------------------------------
    ;; Variables
    ("\\<\\([_A-Z][a-zA-Z0-9_]*\\)" 1 ciao-face-variable)
    ;; Concurrency ops
    ("\\([ \t]&&\\|[ \t]&>\\|[ \t]<&\\|[ \t]&\\|[ \t]@[ \t]\\)" . ciao-face-concurrency-op) 
    ;; Cut
    ("!" . ciao-face-cut)
    ;; Unrecognized directive (mark as warning)
    ("^[ \t]*:-" . ciao-face-lpdoc-bug-comment)
    ;; Necks and control
    ("\\(:-\\|-->\\|=>\\|->\\)" . ciao-face-prompt)
    ("[^=]:=" . ciao-face-prompt)
    ;; TODO: Other major connectors? they can be distracting --JF
;    ("\\(|\\|?\\|->\\|~\\)" . ciao-face-quoted-atom) ;; operators
;    ("[^'a-zA-Z0-9,()_ \t\n]" . ciao-face-quoted-atom) ;; all symbols
    ;; Clause heads ;; TODO: tricky, better way?
    (ciao-font-lock-match-clauseheadname 0 ciao-face-clauseheadname keep)
    ;; This is for debugging... if patata appears in green it means
    ;; all previous rules are understood by emacs
    ;; Emacs
    ;; ("patata" 0 patata_var t)
    )
  )

;; (defvar patata_var 'ciao-face-true-assrt)

;; Any ":- Expr" directive
(defun ciao-direct-regexp (expr)
  (format "^[ \t]*:-[ \t\n]*%s\\>" expr))

;; ":- doc(Expr" directive
(defun ciao-doc-regexp (expr)
  (concat (ciao-direct-regexp "doc") "([ \t\n]*" expr))

;; Text in ":- doc(_,_)" for title, section, subsection, etc.
(defun ciao-section-text-regexp (expr)
  (ciao-doc-regexp (concat expr "[ \t\n]*,[ \t\n]*\"\\([^\\\"\n]+\\)\"")))

;; Title, section, subsection, etc. commands inside docs
(defun ciao-doc-section-command-text-regexp (expr)
  (concat "^\\(%!\\|%\\|/*!|\\)[ \t]+"
          expr
          "[ \t\n]*\\(.*\\)$"))

(defun ciao-assrt-kind-regexp (expr) ; for assertions
  (ciao-direct-regexp (format "\\(%s\\)" expr)))

(defun ciao-assrt-pp-kind-regexp (expr) ; for program-point
  (format "^[ \t\n]*\\(%s\\)(" expr))

;; font-lock-comment-face
;; Match only if the text has been colored before as ciao-face-lpdoc-comment --JF
(defun ciao-font-lock-simple-match-in-doc (limit beginexp)
  (if (not (search-forward-regexp beginexp limit t))
      nil
    (eq (get-text-property (car (match-data)) 'face) 'ciao-face-lpdoc-comment)))

;;; jfmc: old definition
;; (defun ciao-font-lock-match-clauseheadname (limit)
;;  (search-forward-regexp "^[a-z][a-zA-Z0-9_]*" limit t))
;; Alternatives:
;;    ("^\\([a-z'][^(\\.: ]*\\)\\([ \t\n]*\\.\\|[ \t\n]*:-\\|(\\)" 1 ciao-face-clauseheadname t)
;;    ("^[a-z][a-zA-Z0-9_]*" 0 ciao-face-clauseheadname)
;;    ("^\\('\\(\\\\'\\|[^']\\)*'\\)\\([ \t\n]*\\.\\|[ \t\n]*:-\\|(\\)" 0 ciao-face-clauseheadname)
;;    ("^[a-z][a-zA-Z0-9_]*" 0 ciao-face-clauseheadname)

;; Matches a clauseheadname
;; jfmc: version that works even if clauses are inner indented
(defun ciao-font-lock-match-clauseheadname (limit)
  (let ((found nil))
    (while (and (not found)
		(search-forward-regexp "[a-z][a-zA-Z0-9_]*" limit t))
      (let ((begin (match-beginning 0)))
	(if (save-match-data (save-excursion
			       (progn
				 (goto-char begin)
				 (ciao-scan-backward-clausehead))))
	    ;; Found!
	    (setq found t))))
    found))

(defun ciao-scan-backward-clausehead ()
  "Scan characters backwards to determine if we are at a clause head
   position"
  ;; Skip blanks just before clausehead (returns nil if there are no
  ;; blanks and the clausehead is not at column 0)
  (if (= (current-column) 0)
      t ;; trivial case for column 0
    (if (and (> (current-column) 0)
             (= (skip-chars-backward " \t") 0))
        nil ;; no blank, this cannot be a clause head
      ;; Skip comments and empty lines
      (while (and (> (point) 1)
                  (progn (forward-char -1)
                         (looking-at "\n")))
        ;; Find a possible comment and skip blanks
        ;; TODO: incorrectly treats quoted % as comments (jfmc)
        (beginning-of-line)
        (re-search-forward "[^%\n]*")
        (skip-chars-backward " \t"))
      ;; Detect if we are at a position that allows new clauses
      (cond ((= (point) 1) t) ;; beginning of buffer
            ((looking-at "\\.") t) ;; clause end
            ((looking-at "{") (ciao-scan-forward-curly-block)) ;; curly brace
	  (t nil))))) ;; no clause

(defun ciao-scan-forward-curly-block ()
  "Detect if the curly block contains sentences rather than terms"
  ;; TODO: imprecise, it should parse the tokens (i.e. {foo({bar}).} is not detected
  ;; TODO: {foo.} is not detected
  (re-search-forward "[^\\.}]*")
  (looking-at "\\.")
  )

;; ;; Matches corresponding closing delimiter
;; (defun ciao-font-lock-match-until-matching-sexp (limit beginexp)
;;   (let ((begin 0) (end 0))
;;     (if (not (search-forward-regexp beginexp limit t))
;; 	nil
;;       (setq begin (car (match-data)))
;;       (goto-char (- (car (cdr (match-data))) 1))
;;       (forward-list)
;;       (setq end (cons (point) nil))
;;       (set-match-data (cons begin end))
;;       t
;;       )))

; ---------------------------------------------------------------------------
; Determine the final face for syntactic elements 
(defun ciao-syntactic-face-function (state)
  "`font-lock-syntactic-face-function' for Ciao."
  (cond
   ;; Quoted atoms
   ((save-excursion
      (goto-char (nth 8 state))
      (looking-at-p "'"))
    'ciao-face-quoted-atom)
   ;; Strings as lpdoc comments (# "...")
   ((save-excursion
      (goto-char (nth 8 state))
      (and (looking-at-p "\"")
           (progn
             (skip-chars-backward " \t\n")
             (and (> (point) 1)
                  (progn
                    (backward-char)
                    (looking-at-p "#"))))))
    'ciao-face-lpdoc-comment)
   ;; Strings as lpdoc comments (:- doc(, "..."))
   ((save-excursion
      (goto-char (nth 8 state))
      (and (looking-at-p "\"")
           (progn
             (skip-chars-backward " \t\n")
             (beginning-of-line)
             (looking-at-p (ciao-doc-regexp "")))))
    'ciao-face-lpdoc-comment)
   ;; Strings followed by lpdoc comment strings (trick to colour escaped "foo""bar")
   ((save-excursion
      (goto-char (nth 8 state))
      (and (> (point) 1)
           (progn
             (backward-char)
             (looking-at-p "\""))
           (eq (get-text-property (point) 'face)
               'ciao-face-lpdoc-comment)))
    'ciao-face-lpdoc-comment)
   ;; Strings
   ((save-excursion
      (goto-char (nth 8 state))
      (looking-at-p "\""))
    'ciao-face-string)
   ;; doccomments (first line)
   ((save-excursion
      (goto-char (nth 8 state))
      (looking-at-p "%!"))
    'ciao-face-lpdoc-comment)
   ;; doccomments (preceding is a doccomment too)
   ((save-excursion
      (goto-char (nth 8 state))
      (forward-line -1)
      (beginning-of-line)
      (and (looking-at-p "%")
           (eq (get-text-property (point) 'face)
               'ciao-face-lpdoc-comment)))
    'ciao-face-lpdoc-comment)
   ;; Special case for 0'%
   ((save-excursion
      (goto-char (nth 8 state))
      (and (> (point) 2)
           (progn
             (forward-char -2)
             (looking-at-p "0'%"))))
    nil)
   (t 'ciao-face-comment))) ;; TODO: use font-lock-comment-face?

;; TODO: support for ``` code blocks (use put-text-property to amend %)?

; ---------------------------------------------------------------------------

(defun ciao-inferior-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for inferior Ciao process."
  ;; MR added to support font-lock
  (if (ciao-emacs-can-do-font-lock-p)
      (setq-local font-lock-defaults
                  '(ciao-inferior-font-lock-keywords
                    t ;; 't' means font-lock-keywords-only
                    nil ((?_ . "w")) nil
                    (font-lock-syntactic-face-function
                     . ciao-syntactic-face-function)))))

(defun ciao-inferior-font-lock-keywords ()
      `(
        ;("\\<\\([_A-Z][a-zA-Z0-9_]*\\)" 1 ciao-face-variable) ; TODO: fixme, only in goals!
	("^\\([A-Z][a-zA-Z0-9_]*\\) = \\(.*\\)\\(,\\| \\?.*\\)$"
         (1 ciao-face-answer-var)               ;; Answer variable
         (2 ciao-face-answer-val)               ;; Answer value
         (3 ciao-face-prompt)                   ;; Prompt after answer
         )
	("^\\([ \t]+[0-9]+[ \t]+[0-9]+\\)\\(Call:\\).*$"
         (1 ciao-face-debug-redo)               ;; 
         (2 ciao-face-debug-call)               ;; 
         )
	(,(ciao-any-prompt-pattern)
	 ;; "^\\(\\(\\|[0-9]+ \\|ciaopp \\|| \\)\\?-\\)"
         . ciao-face-prompt)                    ;; Prompts
	("^yes$" . ciao-face-yes-answer)        ;; Answer
	("^no$" . ciao-face-no-answer)          ;; Answer
	("^aborted$" . ciao-face-error-mess)     ;; Answer
;;	("^Select[^:]*:" . ciao-face-ciaopp-option) ;; Preproc prompt
	("\\([A-Z][a-zA-Z \\-]*:\\) *\\(\\[[a-zA-Z0-9, _\t\n]*\\]\\)[ \t\n]*\\(([^)]*)\\)[ \t\n]*\\(\\?\\)"
         (1 ciao-face-ciaopp-option)            ;; Option message
         (2 ciao-face-answer-val)               ;; Option values
         (3 ciao-face-answer-var)               ;; Default
         (4 ciao-face-prompt)                   ;; Prompt
         )
	("^{?ERROR.*$" . ciao-face-error-mess)  ;; Error messages
	("^{SYNTAX ERROR.*$" . ciao-face-error-mess)  ;; Error messages
	("^\\*\\* here \\*\\*[ \t]*$" . ciao-face-error-mess)  ;; Error mes
	("^{?WARNING.*$" . ciao-face-warning-mess)  ;; Error messages
	("^{DEBUG.*$" . ciao-face-debug-mess)       ;; Error messages
	("^{?Note:.*$" . ciao-face-note-mess)       ;; Error messages
	("^{NOTE.*$" . ciao-face-note-mess)         ;; Error messages
	("^{?PASSED:.*$" . ciao-face-passed-mess)   ;; Test results messages
	("^{?FAILED:.*$" . ciao-face-failed-mess)   ;; Test results messages
	("^{?ABORTED:.*$" . ciao-face-aborted-mess) ;; Test results messages
        ("^\\({.*\\|}\\)" . ciao-face-other-mess)   ;; Error messages
;;        ("^\\*\\*\\* ---------.*\n^\\*\\*\\* .*\n\\*\\*\\* ---------.*$" 
        ("^\\*\\*\\* \\(---------\\|=========\\).*$" 
	 . ciao-face-highlight-code)              ;; LPdoc (1.9) messages
        ("^\\*\\*\\* .*$" . ciao-face-debug-call) ;; LPdoc (1.9) messages
	; The different startup messages
;	("^Ciao\\>.*$" . ciao-face-startup-message);; Startup
	("^Ciao [0-9]+\\.[0-9].*:.*$" . ciao-face-startup-message);; Startup
        ; Recognizes a date at the end of the line (ciaopp still does it)
	("^(C) .* \\w\\w\\w \\w\\w\\w [1-3]?[0-9]\
 [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [A-Z][A-Z][A-Z] [1-2][0-9][0-9][0-9]$"
        . ciao-face-startup-message)              ;; Startup, second line
;	("\\(^\\?- *[^{ ]\\|^| \\?- *\\).*\\.[ \t]*\n"
;	 . ciao-face-prompt) ;; Query doesn't work(?)
))


;; Provide ourselves:

(provide 'ciao-font-lock)

;;; ciao-font-lock.el ends here
