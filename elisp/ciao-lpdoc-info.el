;;; ciao-lpdoc-info --- Coloring info manuals generated LPdoc

;; Copyright (C) 2020 Free Software Foundation, Inc. and
;; M. Hermenegildo and others (herme@fi.upm.es, IMDEA/UPM-CLIP, Spain).

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

;; -------------------------------------------------------------------
;;; Commentary:
;;
;; This package provides coloring support for info manuals generated
;; by Ciao's LPdoc documenter.  It uses the faces in ciao-faces.el.
;; 
;; Inspired by info-colors.el
;; -------------------------------------------------------------------

;; Note:
;; 
;; The regular expressions in this file (and possibly some of the code
;; below) are highly dependent on the output produced by LPdoc, and
;; thus may need tweaking if this output changes. 

;;; Code:
(require 'ciao-faces)

;;;###autoload
(add-hook 'Info-selection-hook 'fontify-lpdoc-info-node)

(defvar fontifying-lpdoc-info-nodes t
    "Toggle fontification of this (LPdoc-generated) info node.")

;;;###autoload
(defun fontify-lpdoc-info-node ()
  "Fontify an `info' node generated by lpdoc."
  (interactive) ; for testing
  (if fontifying-lpdoc-info-nodes
      (save-excursion
        (let* ((inhibit-read-only t)
               (case-fold-search t))
;;------------------------
;; The main descriptions
          (goto-char (point-min))
          (while (re-search-forward
                  "^ \\(--\\)? \
\\(PREDICATE\\|REGTYPE\\|DECLARATION\\|PROPERTY\\|MODE\\)\\(:\\)\
 *\\(\\S-+\\)\
\\(\\( .*\\)?\\([\n] \\{8\\}.*\\)*\\)"
                  nil t)
            (put-text-property (match-beginning 1) (match-end 1)
                               'invisible t)
            (put-text-property
             (match-beginning 2) (match-end 2)
             'font-lock-face (cl-case (intern (match-string 2))
                               ('PREDICATE     'ciao-face-debug-call)
                               ('DECLARATION   'ciao-face-debug-exit)
                               ('REGTYPE       'ciao-face-debug-redo)
                               ('PROPERTY      'ciao-face-debug-redo)
                               ('MODE          'ciao-face-debug-fail)
                               ))
            (put-text-property (match-beginning 3) (match-end 3)
                               'invisible t)
            (put-text-property (match-beginning 4) (+ (match-end 4) 1)
                               'font-lock-face 'ciao-face-entry-assrt)
            (when (match-beginning 5)
              (put-text-property (match-beginning 5) (match-end 5)
                                 'font-lock-face
                                 'ciao-face-predicate-directory))
                               ; 'ciao-face-highlight-code
            )
;;------------------------
;; Modules, imports, exports, ...
          (goto-char (point-min))
          (while (re-search-forward
                  "^   \\(\\* Exports\\|\\* Library usage\\|\\* New operators defined\
\\|\\* New declarations defined\\|\\* New modes defined\\|\\* Implicit imports\\):"
                  nil t)
            (put-text-property (match-beginning 1) (+ (match-end 1) 1) ; +1 for the ':'
                               'font-lock-face 'ciao-face-builtin-directive)
            )
;;------------------------
;; Usages. More modules, imports, exports, ... Authors.
          (goto-char (point-min))
          (while (re-search-forward
                  "^ *\\(Author(s)\\|^Version\\|Usage [0-9]*\\|Usage\\|Other properties\
\\|- Packages\\|- Predicates\\|- Properties\\|- Multifiles\
\\|- Regular Types\\|- Packages\\|- System library modules\\|- System library modules\
\\):"
                  nil t)
            (put-text-property (match-beginning 1) (match-end 1)
                               'font-lock-face 'ciao-face-predicate-directive)
            )
;;------------------------
;; Messages from assertion parts:
;; See status_text_mode/4 and related in LPdoc.
          (goto-char (point-min))
          (while (re-search-forward
                  "^ *\\(\\(\
- The following properties \\|\
- If the following properties \\|\
then the following properties \\|\
- Call and exit \\)\
\\(are added\\|hold\\|should hold\\|do not hold\\|are proved to hold\\|are\\|are not\\|should be\\) \
\\(at call time:\\|upon exit:\\|globally:\\|compatible with:\\)\\|\
- Calls should, and exit will be compatible with:\\)"
                  nil t)
            (if (not (match-beginning 2))
                (put-text-property (match-beginning 1) (match-end 1)
                                   'font-lock-face 'ciao-face-check-assrt)
              (put-text-property
               (match-beginning 1) (match-end 4)
               'font-lock-face
               (cl-case (intern (match-string 3))
                 ('hold                  'ciao-face-check-assrt)   ; true/trust
                 ('are\ proved\ to\ hold 'ciao-face-checked-assrt) ; checked
                 ('should\ hold          'ciao-face-check-assrt)   ; check
                 ('do\ not\ hold         'ciao-face-false-assrt)   ; false
                 ;
                 ('are                   'ciao-face-check-assrt)   ; true/trush
                 ('should\ be            'ciao-face-check-assrt)   ; check (compat)
                 ('are\ not              'ciao-face-false-assrt)   ; false (compat)
                 ;
                 ('are\ added            'ciao-face-modedef-assrt) ; modes
                 ; 
                 ('otherwise             'ciao-face-quoted-atom)   ; 
                 )
               )))

;;------------------------
;; Take out texinfo's quotes for emphasis, tt, bf, etc.
          (goto-char (point-min))
          (while (re-search-forward
                  "\\(`\\)\\([^`']*\\)\\('\\)"
                  nil t)
            (put-text-property (match-beginning 1) (match-end 1)
                               'invisible t)
            (put-text-property (match-beginning 2) (match-end 2)
                               'font-lock-face 'ciao-face-user-directive)
            (put-text-property (match-beginning 3) (match-end 3)
                               'invisible t)
            )
;;------------------------
;; Properties in assertion parts:  `(module:pred/arity)'
          (goto-char (point-min))
          (while (re-search-forward
           "\\( *\\)\\((\\)\\(`\\)\\([^:`]*:[^/`]*/[^/`]*[0-9]*\\)\\('\\)\\()\\)"
                  nil t)
            (if (not (char-equal (char-before (match-beginning 1)) ?\n))
                (put-text-property (match-beginning 1) (match-end 1)
                                   'invisible t)
              )
            (put-text-property (match-beginning 2) (match-end 2)
                               'font-lock-face 'ciao-face-string)
            (put-text-property (match-beginning 3) (match-end 3)
                               'invisible t)
            (put-text-property (match-beginning 4) (match-end 4)
                               'font-lock-face 'ciao-face-quoted-atom)
            (put-text-property (match-beginning 5) (match-end 5)
                               'invisible t)
            (put-text-property (match-beginning 6) (match-end 6)
                               'font-lock-face 'ciao-face-string)
            )
;;------------------------
;; Make ISO marks nicer
          (goto-char (point-min))
          (while (re-search-forward
                  "\\(< \\* \\)\\(ISO\\)\\( \\* >\\)"
                  nil t)
            (put-text-property (match-beginning 1) (match-end 1)
                               'invisible t)
            (put-text-property (match-beginning 2) (match-end 2)
                               'font-lock-face 'ciao-face-debug-exit)
            (put-text-property (match-beginning 3) (match-end 3)
                               'invisible t)
;; Alt:
;;         (compose-region (match-beginning 1)
;;                         (match-end 1)
;;                         "✿")
            )
;;------------------------
          )
        (set-buffer-modified-p nil)))) ; <-- maybe restore to previous state?


(defun toggle-fontify-lpdoc-info-node ()
  "Toggle fontification of the `info' nodes generated by lpdoc."
  (interactive)
  (save-excursion
    (let* ((inhibit-read-only t))
      (if fontifying-lpdoc-info-nodes
          (progn
            (setq fontifying-lpdoc-info-nodes nil)
            (set-text-properties (point-min) (point-max) nil))
        (setq fontifying-lpdoc-info-nodes t)
        (fontify-lpdoc-info-node))
      )
    )
  (set-buffer-modified-p nil))
      

;; Provide ourselves:

(provide 'ciao-lpdoc-info)

;;; ciao-lpdoc-info.el ends here