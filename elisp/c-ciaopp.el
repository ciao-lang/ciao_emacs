;;; c-ciaopp.el --- CiaoPP mode for C (LLVM-based)
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

;;---------------------------------------------------------------------------
;; Emacs additional support for the CiaoPP preprocessor 
;; when working on C files (complementary to ciao.el).
;; (Very preliminary version!) --M. Hermenegildo
;;---------------------------------------------------------------------------

(require 'easymenu)
(require 'ciao-bindings) ; ciao-general-toolbar
(require 'ciao-help) ; ciao-goto-manuals, ciao-help-on-current-symbol
(require 'ciao-loading) ; ciao-find-last-run-errors,
			; ciao-unmark-last-run-errors
(require 'ciao-ciaopp)
(require 'ciao-help)

; ---------------------------------------------------------------------------
; Add a hook in the C mode to load c-ciaopp.el 

;;;###autoload
(defun load-c-ciaopp-mode ()
  ;; 'require' not necessary since this is being autoloaded (JFMC)
  ;; (require 'c-ciaopp)
  (c-ciaopp-setup))

;;;###autoload(add-hook 'c-mode-hook 'load-c-ciaopp-mode)

; ---------------------------------------------------------------------------

(defvar ciaopp-c-mode-map c-mode-map)

(defun c-ciaopp-setup ()
  (interactive)
  ;; option in the menu
  (easy-menu-define ciao-c-menu-ciaopp ciaopp-c-mode-map 
    "CiaoPP Mode Menus" ciao-mode-menus-c)
  ;; toolbar
  (ciao-c-setup-tool-bar))

; ---------------------------------------------------------------------------
; Key bindings

;; (define-key ciaopp-c-mode-map "\C-cA"    'ciao-analyze-buffer)
;; (define-key ciaopp-c-mode-map "\C-cT"    'ciao-check-assertions)
;; (define-key ciaopp-c-mode-map "\C-cO"    'ciao-optimize-buffer)
(define-key ciaopp-c-mode-map "\C-cM"    'c-browse-preprocessor-options)
(define-key ciaopp-c-mode-map "\C-c\C-v" 'ciao-show-preprocessor-output)
; Compat
(define-key ciaopp-c-mode-map "\C-c`"   'ciao-find-last-run-errors)
(define-key ciaopp-c-mode-map "\M-]"     'ciao-find-last-run-errors)
(define-key ciaopp-c-mode-map "\C-ce"    'ciao-unmark-last-run-errors)
;(define-key ciaopp-c-mode-map "\C-g"     'ciao-unmark-last-run-errors-and-quit)
(define-key ciaopp-c-mode-map "\C-c\C-r" 'run-ciao-preprocessor)
;; (define-key ciaopp-c-mode-map "\C-ch"    'ciao-fontify-buffer)
(define-key ciaopp-c-mode-map "\C-c\C-i" 'ciao-help-on-current-symbol)
(define-key ciaopp-c-mode-map "\C-c/"    'ciao-complete-current-symbol)
(define-key ciaopp-c-mode-map "\C-c\C-m" 'ciao-goto-manuals)
(define-key ciaopp-c-mode-map "\C-ct"    'run-ciao-toplevel)
(define-key ciaopp-c-mode-map "\C-cl"    'ciao-load-buffer)

; ---------------------------------------------------------------------------
; Menus

(defconst ciao-mode-menus-c
  (list "CiaoPP"
     "CiaoPP C Preprocessor (beta)"
     "----"
     "----"
;;     Commented by JNL 
;;     ["Analyze buffer"                         ciao-analyze-buffer t]
;;     ["Check buffer assertions"                ciao-check-assertions t]
;;     ["Optimize buffer"                        ciao-optimize-buffer t]
;;     ["Browse analysis/checking/optimizing options"         
;;                                           ciao-browse-preprocessor-options t]
     ["Browse analysis options"         c-browse-preprocessor-options t]
     "----"
     ["Show last preprocessor output file"     ciao-show-preprocessor-output t]
     ["Go to (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors t]
     "----"
;;      ["Preprocess buffer (w/previous options) and show output"  
;;                                 ciao-preprocess-buffer-and-show-output t]
     ["(Re)Start Ciao preprocessor"              run-ciao-preprocessor t]
;;      ["Update syntax-based coloring"             ciao-fontify-buffer t]
     "----"
     ["Go to manual page for symbol under cursor" ciao-help-on-current-symbol]
     ["Complete symbol under cursor"        ciao-complete-current-symbol t]
     ["Ciao manuals area in info index" ciao-goto-manuals t]
     "----"
     ["(Re)Start Ciao top level"                 run-ciao-toplevel t]
     ["(Re)Load buffer into top level"           ciao-load-buffer  t]
     "----"
     ["Customize all Ciao environment settings" 
                                       (customize-group 'ciao)] 
     ["Ciao environment (mode) version" ciao-report-mode-version t]
     )
  "Menus for CiaoPP mode.")

; ---------------------------------------------------------------------------
; Tool bar

(defun ciao-c-setup-tool-bar () 
  (interactive)
  (make-local-variable 'tool-bar-map) 
  (setq tool-bar-map (make-sparse-keymap))
  ;; General stuff (from standard tool bar); added only in FSF emacs.
  (ciao-general-toolbar tool-bar-map)
  ;; C/CiaoPP-specific stuff
  ;; Stuff that is not in menus will not work.
  (ciao-tool-bar-local-item-from-menu 
   'ciao-analyze-buffer "icons/ciaoanalysis" tool-bar-map ciaopp-c-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-check-assertions "icons/checkassertions" tool-bar-map ciaopp-c-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-optimize-buffer "icons/ciaopeval" tool-bar-map ciaopp-c-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'c-browse-preprocessor-options
   "icons/ciaocustomize" tool-bar-map ciaopp-c-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-find-last-run-errors "icons/jump_to" tool-bar-map ciaopp-c-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-unmark-last-run-errors "icons/clear" tool-bar-map ciaopp-c-mode-map)
  (tool-bar-add-item "icons/manuals" 
   'ciao-goto-manuals 'ciao-goto-manuals 
   :help "Go to area containing the Ciao system manuals")
  (ciao-tool-bar-local-item-from-menu 
   'ciao-help-on-current-symbol "icons/wordhelp" tool-bar-map ciaopp-c-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-complete-current-symbol "icons/complete" tool-bar-map ciaopp-c-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-customize-all
   "icons/preferences" tool-bar-map ciaopp-c-mode-map
   :help "Edit (customize) preferences for Ciao, CiaoPP, LPdoc")
;;   (ciao-tool-bar-local-item-from-menu 
;;    'ciao-fontify-buffer "icons/ciaorehighlight" tool-bar-map ciaopp-c-mode-map)
  )

; ---------------------------------------------------------------------------

; Find the Ciao documentation (including CiaoPP properties) while in
; C mode (i.e., when visiting .c files).
; Not used, word-help replaced by info-look
;; (ciao-help-add-manual "C/l" ciao-manuals)


;; Provide ourselves:

(provide 'c-ciaopp)

;;; c-ciaopp.el ends here

