;;; ciao-syntax.el --- Syntax definitions for Ciao language

;; Copyright (C) 1986-2012 Free Software Foundation, Inc. and
;; M. Hermenegildo and others (herme@fi.upm.es, UPM-CLIP, Spain).
;;
;; Authors: 2019 Jose F. Morales <jfmcjf@gmail.com>
;;               (rewrite indentation code)

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
(require 'ciao-common) ; ciaoide (group)

(defgroup ciaolang nil
  "The Ciao language and syntax."
  :tag "Ciao Language"
  :group 'ciao)

;;------------------------------------------------------------
;; Syntax and movement
;;------------------------------------------------------------

(defvar ciao-mode-syntax-table nil)
(if ciao-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table) ; word constituent
;;    (modify-syntax-entry ?\\ "." table) ; punctuation
;;  1 means CHAR is the start of a two-char comment start sequence.
;;  2 means CHAR is the second character of such a sequence.
;;  3 means CHAR is the start of a two-char comment end sequence.
;;  4 means CHAR is the second character of such a sequence.
    (modify-syntax-entry ?+ "." table) ; punctuation
    (modify-syntax-entry ?- "." table) ; punctuation
    (modify-syntax-entry ?= "." table) ; punctuation
    (modify-syntax-entry ?% "<" table) ; comment starter
    (modify-syntax-entry ?\n ">" table) ; comment ender
    (modify-syntax-entry ?\^m ">" table) ; comment ender
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?< "." table) ; punctuation
    (modify-syntax-entry ?> "." table) ; punctuation
    (modify-syntax-entry ?\' "\"" table) ; escape
    (setq ciao-mode-syntax-table table)))

;; (borrowed from prolog.el) --JF
(defconst ciao-syntax-propertize-function
  (when (fboundp 'syntax-propertize-rules)
    (syntax-propertize-rules
     ("\\<0\\('\"\\|''?\\)" ; 0'', 0'", or just 0'
      (1 (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 0))))
           (string-to-syntax "_"))))
     ("\\<[1-9][0-9]*\\('\\)[0-9a-zA-Z]" (1 "_"))
     ("\\\\[x0-7][[:xdigit:]]*\\(\\\\\\)" (1 "_"))
     )))

(defvar ciao-mode-abbrev-table nil)
(define-abbrev-table 'ciao-mode-abbrev-table ())

(defun ciao-syntax-mode-variables ()
  (setq case-fold-search nil)
  (set-syntax-table ciao-mode-syntax-table)
  ;;
  (setq local-abbrev-table ciao-mode-abbrev-table)
  (setq-local paragraph-start (concat "^%%\\|^$\\|" page-delimiter)) ;'%%..'
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local indent-line-function 'ciao-indent-line)
  (setq-local comment-start "%")
  (setq-local comment-end "")
  (setq-local comment-add 1)
  (setq-local comment-start-skip "\\(?:/\\*+ *\\|%+ *\\)")
;  (setq-local comment-start "%")
;  (setq-local comment-start-skip "%+ *")
  (setq-local comment-column 48)
;; Obsolete since before 19.5
;;   (setq-local comment-indent-hook 'ciao-comment-indent)
  (setq-local comment-indent-function 'ciao-comment-indent)
  (setq-local syntax-propertize-function ciao-syntax-propertize-function)
  )

;;------------------------------------------------------------
;; Indentation
;;------------------------------------------------------------

(defcustom ciao-indent-width 4
  "Indentation level."
  :group 'ciaolang
  :type 'integer)

(defun ciao-indent-line (&optional whole-exp)
  "Indent current line as Ciao code.
With argument, indent any additional lines of the same clause
rigidly along with this one."
  (interactive "p")
  (if (ciao-do-not-indent)
      nil ; avoid indentation in this case
    (let ((indent (ciao-indent-level))
          (pos (- (point-max) (point))) beg)
      (beginning-of-line)
      (setq beg (point))
      (skip-chars-forward " \t")
      ;;
      (if (zerop (- indent (current-column)))
          nil
        (delete-region beg (point))
        (indent-to indent))
      ;;
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))))

(defun ciao-do-not-indent ()
  (save-excursion
    (beginning-of-line)
    (let ((ppss (syntax-ppss)))
      (or (nth 3 ppss) (nth 4 ppss))))) ;; at string or comment

(defun ciao-indent-level ()
  "Compute Ciao indentation level."
  (save-excursion
    (let ((closing nil)
          (lparen nil)
          (basecol 0)
          (innercol 0))
      ;; 0) Move to first nonblank in the line
      (beginning-of-line)
      (skip-chars-forward " \t")
      ;; 1) Detect if the line begins with a closing symbol
      (cond
       ((looking-at "[\]})]") (setq closing 'paren)) ; closing parenthesis
       ((looking-at "[;|]") (setq closing 'bar))) ; closing bar
      ;; 2) Search backward the indentation context
      (setq lparen (ciao-scan-token-backwards closing))
      ;; 3) Compute basecol and innercol
      (if (eq lparen 'base)
          (current-column)
        ;; lparen paren or bar
        (if (and
             (or (eq lparen 'paren) (eq lparen 'bar))
             (save-excursion
               ;; and last non-blank is not an open parenthesis
               (ciao-move-end-of-line) ;; Go to last 
               (forward-char -1) ;; previous
               (or
                (nth 3 (syntax-ppss)) ;; a string
                (not (or (looking-at "[\[{(]")
                         (looking-at "[;|]"))))))
            ;; argument indentation mode
            (progn
              (setq basecol (current-column)) ;; basecol in paren or bar
              (forward-char 1)
              (skip-chars-forward " \t")
              (setq innercol (current-column))) ;; inner in next nonblank
          ;; block indentation mode
          ;;
          ;; compute basecol: search backwards the first nonblank
          ;;   balanced char in the line (it may skip newlines)
          (if (> (current-column) 0)
              (progn
                (forward-char -1)
                (ciao-scan-token-backwards 'blockbase)
                (beginning-of-line) ;; TODO: simplify?
                (skip-chars-forward " \t")))
          (setq basecol (current-column))
          ;; compute innercol
          (if (and (eq lparen 'neck) (looking-at ":-")) ;; ':-' directive
              ;; TODO: same condition as early stop for prev='period?
              (progn
                (forward-char 2)
                (skip-chars-forward " \t")
                (setq innercol (current-column))) ;; TODO: width=3 instead?
            ;; default block indentation
            (setq innercol (+ ciao-indent-width basecol))))
        ;; 4) Use basecol or innercol
        (cond
         ((eq closing 'paren) basecol)
         ((and (eq closing 'bar) (not (eq lparen 'neck))) basecol)
         (t innercol))))))

(defun ciao-skip-string-or-comments-backwards ()
  (let ((ppss0 (syntax-ppss))) ;; TODO: syntax-ppss can be very slow
    (if (or (nth 3 ppss0) (nth 4 ppss0)) ;; at string or comment
        (goto-char (1- (nth 8 ppss0)))))) ;; note: safe w.r.t. bobp

(defun ciao-scan-token-backwards (closing)
  ;; Scan backwards the previous lines until a stop point is
  ;; found. Stop points can be:
  ;;  - `paren' if unbalanced left parenthesis
  ;;  - `bar' a bar (skip if balance is not 0)
  ;;  - `neck' if neck (skip if balance is not 0)
  ;;  - `arrow' if arrow (skip if balance is not 0)
  ;;  - `base' if found some conservative point for base indentation
  (let ((pbal 0) ;; unbalanced parenthesis
        (prev nil)
        (lparen nil))
    (if (eq closing 'blockbase)
        (setq prev 'period) ;; same as period but do not look at clausehead
      (ciao-backward-nonblank) ;; Search backward nonblank, skipping comments
      (if (bobp) (setq lparen 'base) ;; Nothing above, stop
        ;; else detect prev
        (setq prev
              (cond ((eq (preceding-char) ?.) 'period) ;; assume new clause
                    ((eq (preceding-char) ?,) 'comma) ;; assume new arg
                    (t 'other)))))
    (while (not lparen)
      (cond
       ((looking-at "[\]})]") (setq pbal (1+ pbal)))
       ((looking-at "[\[{(]") (setq pbal (1- pbal)) (if (< pbal 0) (setq lparen 'paren)))
       ((eq pbal 0)
        (setq lparen
              (if (eq prev 'period)
                  nil ;; it was nil before
                (cond ((looking-at "\\(:-\\|:=\\|-->\\)") 'neck)
                      ((looking-at "\\(->\\)") 'arrow) ;; TODO: some trouble with '?', '=>'
                      ((looking-at "[;|]") 'bar)
                      (t nil)))))) ;; it was nil before
      ;; stop if needed
      (if (not lparen)
          (setq lparen
                (if (bobp) 'base0 ;; nothing else, stop
                  (if (and (eq pbal 0) (eq (current-column) 0))
                      ;; try early stop checks at column 0 (assume that the code above is indented)
                      (cond
                       ((and (eq prev 'period) ;; clause head?
                             (not closing)
                             (ciao--at-clause-head))
                        'base0)
                       ((eq prev 'comma) 'base0) ;; new arg?
                       ((eq closing 'blockbase) 'base0) ;; stop early for blockbase
                       (t nil))
                    nil))))
      ;; continue, move to previous non-word and non-blank symbol
      (if (not lparen)
          (progn
            (skip-syntax-backward "w-" (1+ (line-beginning-position)))
            (forward-char -1)
            (ciao-skip-string-or-comments-backwards))))
    (if (eq lparen 'base0)
        (progn (skip-chars-forward " \t") 'base)
      lparen)))

(defun ciao--at-clause-head ()
  (save-excursion 
    (ciao-backward-nonblank)
    (member (preceding-char) '(?. ?{))))

(defun ciao-backward-nonblank ()
  ;; Move just after the most recent nonblank in previous line(s),
  ;; skipping comments
  (beginning-of-line) ;; Move to beginning of line
  (if (bobp)
      t ;; Beggining of buffer, cannot move any further
    ;; Skip blanks and comments
    (forward-line -1)
    (while (and (not (bobp))
                (progn
                  (skip-chars-forward " \t")
                  (or (looking-at "\n")
                      (looking-at "%")
                      (looking-at "/\\*")
                      (nth 4 (syntax-ppss))))) ;; we are in a comment
      (forward-line -1))
    (ciao-move-end-of-line)))

(defun ciao-move-end-of-line ()
  "Last nonblank char in the line (which is not in a comment)."
  (beginning-of-line)
  (let* ((eolpos (save-excursion (end-of-line) (point))))
    (if (re-search-forward comment-start-skip eolpos 'move)
	(goto-char (match-beginning 0)))
    (skip-chars-backward " \t")))

;; (defun ciao-comment-indent ()
;;   "Compute Ciao comment indentation."
;;   (ciao-indent-level))
(defun ciao-comment-indent ()
  "Compute Ciao comment indentation."
  (cond ((looking-at "%%%") 0)
	((looking-at "%%") (ciao-indent-level))
	(t
	 (save-excursion
	       (skip-chars-backward " \t")
	       (max (1+ (current-column)) ;Insert one space at least
		    comment-column)))))

;; ---------------------------------------------------------------------------

;; TODO: Use with care, this is experimental.
(defun ciao-format-file ()
  "Format a Ciao program using `ciaofmt'."
  (interactive)
  (let* ((tmp_file_1 (concat (buffer-file-name) ".1.tmp"))
         (current-point (point))
         (current-start (window-start))
         (source-buffer (current-buffer)))
    (with-temp-file tmp_file_1
      (insert-buffer-substring source-buffer))
    (shell-command (concat (ciao-get-config :ciaofmt-bin) " " tmp_file_1) source-buffer)
    (delete-file tmp_file_1)
    (set-window-start (selected-window) current-start)
    (goto-char current-point)))

;;------------------------------------------------------------
;; Some aid for inserting program elements (very limited for now)
;;------------------------------------------------------------

;; TODO: It does not seem the right place for this.

(defun ciao-insert-script-header ()

  "Insert a (Unix) header at the top of the current buffer so that the
Ciao script interpreter will be called on this file if @em{run} from
the command line. It also makes the file ``executable'' (e.g.,
'@tt{chmod +x <file>}' in Unix). See @ref{The script interpreter} for
details."

  (interactive)
  (goto-char (point-min))
  (insert 
   (concat "#!/usr/bin/env ciao-shell\n"
           "% -*- mode: ciao; -*-\n"
	   "\n"))
  (set-file-modes (buffer-file-name) 448))

;;------------------------------------------------------------
;; Syntactic definitions for inferior modes
;;------------------------------------------------------------

;; TODO: Add pattern for 'bash'. Better idea: change the prompt of
;;   external shell used for LPdoc.
;;
;; (defcustom ciao-os-shell-prompt-pattern "\\(\\[[0-9]+\\]> \\|.*\\$ \\|.*> \\|[A-Z]:.*>\\)"
;; Refined inclusion of current Ubuntu default. Basically, 
;; eliminated spaces in (...>) prompt. May still need work.
;; This is a side effect of using this pattern also when looking for
;;   errors in ciao buffers because LPdoc still uses an OS shell
;;   instead of a Ciao top level: this is what needs to be fixed
;;   really.  --MH
;;
;; Note: The "^[^ ]" parts are needed to make sure that the prompt
;;   begins a line and is not a blank. This removes many false
;;   positives but it is still not optimal --JF

(defcustom ciao-os-shell-prompt-pattern
  "\\(^\\[[0-9]+\\]> \\|^\\$ \\|^[^ ].*\\$ \\|^[^ ]*> \\|^[A-Z]:.*>\\)"
  "Regular expression used to describe typical shell prompt
patterns (csh and bash), so that error location works in inferior
shells. This is useful for example so that errors are located
when generating documentation, and also when using the embedded
debugger or any other application in a shell. It is best to be as
precise as possible when defining this so that the standard Ciao
error location does not get confused."
  :group 'ciaolang
  :type 'string)

;; Prompt patterns -- left some out of custom because you need to be
;;                    really careful when changing these...

;; TODO: allow generic names in prompts?
(defun ciao-any-prompt-pattern ()
  "Matching any Ciao or other inferior process prompt"
  (concat
   "\\("
   "^\\(\\(\\|[0-9]+ \\|ciaopp \\|lpdoc \\|| \\)\\?-\\)" 
   "\\|" 
   ciao-os-shell-prompt-pattern 
   "\\)"))
;; "\\(^|* *\\?- *\\)\\|\\(^ciaopp \\?- *\\)") Old Ciao/SICStus prompt patterns

(defcustom ciao-locate-also-note-messages nil
  "If set, also when errors of type NOTE are detected the
corresponding file is visited and the location marked. It is set to
nil by default because sometimes the user prefers not to take any
action with respect to these messages (for example, many come from the
documenter, indicating that adding certain declarations the
documentation would be improved)."
  :group 'ciaoide
  :type 'boolean)

(defun ciao-error-or-prompt-pattern ()
  (concat 
  "\\("				      
  (if ciao-locate-also-note-messages
      "^\\({?WARNING.*:\\|{?ERROR.*:\\|{?NOTE.*:\\)"
    "^\\({?WARNING.*:\\|{?ERROR.*:\\)")
  "\\|"
  (ciao-any-prompt-pattern)
  "\\)"))


;; Provide ourselves:

(provide 'ciao-syntax)

;;; ciao-syntax.el ends here

