;;; flycheck-ciao.el --- Flycheck checkers for ciao-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Miguel Angel Sanchez Ordaz

;; Author: Miguel Angel Sanchez Ordaz <ma.sanchez.ordaz@imdea.org>
;; Keywords: convenience languages tools
;; Package-Version: 0.1
;; Package-Requires: ((emacs "24.0") (flycheck "0.18"))

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
;; Flycheck support for ciao-mode including different checkers.
;;
;; ciaoc uses CiaoC for checking syntax errors.
;; ciaopp uses CiaoPP for checking assertions and syntax errors.
;;
;; Setup:
;; 1. Install package via package.el.
;; [ALT+X][RET]package-install-file[RET](Path of "flycheck-ciao.el")[RET]
;; 2. Insert the next line into your Emacs init file
;; 	(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-ciao-setup))
;; 3. Optional: Enable Flycheck Mode in all buffers where syntax checking is possible.
;; 	(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Code:

(require 'ciao)
(require 'flycheck)

(defconst flycheck-ciao-temporary-file-suffix
  "_flycheck_tmp_co"
  "Suffix used for flycheck temp files.")

(defun flycheck-ciao-skip-comments (errors)
  "Overlays the ERRORS in the first non-empty and non-comment line found.
For each error in ERRORS, determine if the line overlayed is empty or
comment by the State of Parse-Partial-Sexp.  Sets the error line to the
next non-empty and non-comment line.
Return ERRORS, with lines modifications."
  (seq-do
   (lambda (err)
     ; (save-excursion
     (with-current-buffer (flycheck-error-buffer err)
       ; (switch-to-buffer (flycheck-error-buffer err))
       (setf (flycheck-error-line err) (ciao-narrow-loc-ln0 (flycheck-error-pos err)))))
   errors)
  errors)

(defun flycheck-ciao-parse-output (output checker buffer)
  "Parse Ciao errors from OUTPUT.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively."
  (flycheck-ciao-register-time)
  (if (string-blank-p output)
      (flycheck-ciao-remove-temporary-files)
    (let ((errors nil)
          (filename nil)
          (level nil)
          (begin-line nil)
          (end-line nil)
          (message nil))
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (let ((error-found nil)
              (last-error nil))
        (while (not last-error)
          (setq error-found (ciao-locate-next-error (current-buffer)))
          (if (not error-found)
              (setq last-error t)
            (setq begin-line (ciao-error-get error-found 'ln0))
            (setq end-line (ciao-error-get error-found 'ln1))
            (let ((file-tmp (ciao-error-get error-found 'file)))
              (setq filename
                    (if (not file-tmp) (buffer-file-name buffer)
                      (replace-regexp-in-string (concat flycheck-ciao-temporary-file-suffix ".") "."  file-tmp))))
            (setq level (ciao-error-get error-found 'level))
            (setq message (ciao-error-get error-found 'message))
            (push (flycheck-error-new-at
                   (if (< begin-line 1) 1 begin-line)
                   0
                   level
                   message
                   :checker checker
                   :buffer buffer
                   :filename filename
                   :end-line end-line
                   )
                  errors))
          (goto-char (point-min))
          (forward-line (ciao-error-get error-found 'infln)))))
      (flycheck-ciao-register-time)
      (flycheck-ciao-remove-temporary-files)
      (nreverse errors))))

(defun flycheck-ciao-enable-check ()
  "Determines if current file should be checked by Flycheck."
  (if (string-match-p "^.+_co\.[a-zA-Z]+$" (buffer-file-name))
      nil
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- (ciao-narrow-loc-ln0 (point-min))))
      (let (enable-check ln0)
        (setq ln0 (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
        (if (string-match-p "^[ \t]*:-[ \t]*module" ln0)
            (setq enable-check t)
          (when (and (not (string-match-p "^[ \t]*:-[ \t]*package" ln0))
                     (not (search-backward-regexp "^[ \t]*\%[ \t]*\(included file\)" nil t)))) ;; TODO: make declaration for included file
	  (setq enable-check t))
        enable-check))))
         
(flycheck-define-checker ciaoc
  "A Ciao syntax checker using CiaoC for ciao-mode."
  :command ("ciaoc" ; overriden by flycheck-checker-executable-variable below
            "-c"
            "-op"
            (eval flycheck-ciao-temporary-file-suffix)
            (eval (flycheck-ciao-create-temporary-file)))
            ;;:command ((eval (concat ciao-bin-dir "/ciaoc")) "-c" source) ; does not work (https://github.com/flycheck/flycheck/issues/1515)
  :predicate flycheck-ciao-enable-check
  :error-parser flycheck-ciao-parse-output
  :error-filter
  (lambda (errors)
    (flycheck-ciao-skip-comments
     (flycheck-fill-empty-line-numbers
      (flycheck-sanitize-errors errors))))
  :modes ciao-mode
  )

(flycheck-def-option-var flycheck-ciaopp-flags nil ciaopp
  "A list of CiaoPP flags to eval when analyzing and checking assertions."
  :type '(repeat :tag "Flags" (string :tag "Flag"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(defun flycheck-ciao-get-flag-from-title (title)
  "Return CiaoPP flag from TITLE."
  (substring-no-properties title
			   (1+ (string-match "([a-z0-9_]+)" title))
			   (1- (length title))))

(defun flycheck-ciao-gmenu-ciaopp-flag (title defopt)
  "Append the corresponding CiaoPP flag from TITLE and DEFOPT to the list of flags for Flycheck."
  (let ((flag (flycheck-ciao-get-flag-from-title (string-trim title "\\`[ \t\n]*" "[ \t\n]*\\'")))
        (newvalue (string-trim defopt "\\`[ \t\n]*" "[ \t\n]*\\'")))
    (cond
     ((string= "menu_level" flag) (setq flycheck-ciaopp-flags nil))
     ((string= "inter_all" flag) nil)
     (t (setq flycheck-ciaopp-flags (append flycheck-ciaopp-flags
                                            (list (concat flag "=" newvalue)))))
     )))

(require 'ciao-server)

(flycheck-define-checker ciaopp
  "A Ciao syntax and assertions checker using CiaoPP-client for ciao-mode"
  :command ("ciaopp-client"
	    "-op"
            (eval flycheck-ciao-temporary-file-suffix)
            "-V"
            (eval (flycheck-ciao-create-temporary-file))
	    (option-list "-f" flycheck-ciaopp-flags concat)
	    )
  :predicate (lambda ()
               (when (not ciao-server-process)
                 (ciao-server-start))
               (flycheck-ciao-enable-check))
  :error-parser flycheck-ciao-parse-output
  :error-filter
  (lambda (errors)
     (flycheck-ciao-skip-comments
      (flycheck-fill-empty-line-numbers
       (flycheck-sanitize-errors errors))))
  :modes ciao-mode
  )

(flycheck-define-checker ciaopp-no-keep-alive
  "A Ciao syntax and assertions checker using CiaoPP for ciao-mode"
  :command ("ciaopp"
            "-op"
            (eval flycheck-ciao-temporary-file-suffix)
            "-V"
            (eval (flycheck-ciao-create-temporary-file))
	    (option-list "-f" flycheck-ciaopp-flags concat)
            )
  :predicate flycheck-ciao-enable-check
  :error-parser flycheck-ciao-parse-output
  :error-filter
  (lambda (errors)
     (flycheck-ciao-skip-comments
      (flycheck-fill-empty-line-numbers
       (flycheck-sanitize-errors errors))))
  :modes ciao-mode
  )

(flycheck-define-checker ciao-test
  "A Ciao syntax and test checker using ciaosh for ciao-mode."
  :command ("ciaosh"
	    "-e"
	    "use_module(library(unittest), [run_tests_in_module/1])"
            "-e"
	    "use_module(library(compiler/c_itf),[opt_suffix/2])"
            "-e"
            (eval (concat "opt_suffix(_, '" flycheck-ciao-temporary-file-suffix "')"))
	    "-e"
	    (eval (concat "run_tests_in_module('" (flycheck-ciao-create-temporary-file) "')"))
            "-e"
	    "halt")
  :predicate flycheck-ciao-enable-check
  :error-parser flycheck-ciao-parse-output
  :error-filter
  (lambda (errors)
    (flycheck-ciao-skip-comments
     (flycheck-fill-empty-line-numbers
      (flycheck-sanitize-errors errors))))
  :modes ciao-mode
  )

(flycheck-define-checker lpdoc
  "A Ciao documetation checker using LPDoc for ciao-mode"
  :command ("lpdoc"
            "-t"
            "nil"
            "-op"
            (eval flycheck-ciao-temporary-file-suffix)
            (eval (flycheck-ciao-create-temporary-file)))
  :predicate flycheck-ciao-enable-check
  :error-parser flycheck-ciao-parse-output
  :error-filter
  (lambda (errors)
     (flycheck-ciao-skip-comments
      (flycheck-fill-empty-line-numbers
       (flycheck-sanitize-errors errors))))
  :modes ciao-mode
  )

; Overriding executables
(set (flycheck-checker-executable-variable 'ciaoc)
     (concat (file-name-as-directory ciao-bin-dir) "ciaoc"))
(set (flycheck-checker-executable-variable 'ciaopp)
     (concat (file-name-as-directory ciao-bin-dir) "ciaopp-client"))
(set (flycheck-checker-executable-variable 'ciaopp-no-keep-alive)
     (concat (file-name-as-directory ciao-bin-dir) "ciaopp"))
(set (flycheck-checker-executable-variable 'ciao-test)
     (concat (file-name-as-directory ciao-bin-dir) "ciaosh"))
(set (flycheck-checker-executable-variable 'lpdoc)
     (concat (file-name-as-directory ciao-bin-dir) "lpdoc"))

(require 'easymenu)

(defconst ciao-mode-menus-flycheck
  (easy-menu-create-menu
   "Flycheck"
        '(["Enable on-the-fly syntax checking" flycheck-mode
           :style toggle :selected flycheck-mode
           :enable (or flycheck-mode
                       ;; Don't let users toggle the mode if there is no syntax
                       ;; checker for this buffer
                       (seq-find #'flycheck-checker-supports-major-mode-p
                                 flycheck-checkers))]
          ["Configuration"                 flycheck-verify-setup t]
          ["Show all errors"               flycheck-list-errors t]
          ; ["Enable/Disable Ciao checkers"       ciao-flycheck-change-checker-status t]
	  ["Measure time"                  flycheck-ciao-measure-time t]
          ))
  "Menus for the `ciao-mode' using inferior mode `flycheck-mode'.")

(defun flycheck-ciao-create-temporary-file ()
  "Create a temporary copy with of the current file to be checked by Flycheck."
  (let ((file-name (concat (concat (file-name-base (buffer-file-name))
				   (concat flycheck-ciao-temporary-file-suffix "."))
			   (file-name-extension (buffer-file-name))))
	(inhibit-message t)
	(message-log-max nil))
    (write-region nil nil file-name nil nil nil)
    (file-name-base (buffer-file-name))))

(defun flycheck-ciao-remove-temporary-files ()
  "Remove all temporary files generated by Flycheck."
  (let ((files (directory-files default-directory)))
    (mapc
     (lambda (file)
       (when (string-match-p (concat "^.+" flycheck-ciao-temporary-file-suffix ".*\\.[pl\\|po\\|itf\\|testin\\|testout\\|asr\\|ast]") file)
         (delete-file file)))
     files))
  nil)

;;;###autoload
(defun flycheck-ciao-setup ()
  "Setup Flycheck Ciao."
  (easy-menu-add-item ciao-mode-map '(menu-bar CiaoOpts)
                      ciao-mode-menus-flycheck)
  
  (add-to-list 'flycheck-checkers 'ciaopp-no-keep-alive)
  (add-to-list 'flycheck-checkers 'lpdoc)
  (add-to-list 'flycheck-checkers 'ciao-test)
  (add-to-list 'flycheck-checkers 'ciaoc)
  (add-to-list 'flycheck-checkers 'ciaopp)
  )

(defvar flycheck-ciao-time-log-path
  (expand-file-name "log.csv"
                    (expand-file-name "flycheck-ciao"
                                      (expand-file-name "contrib"
                                                        (ciao-get-config :bundledir-ciao-emacs))))
  "Path of the log time for Flycheck.")

(defvar-local flycheck-ciao-registered-times
  nil
  "Measured times when calling `flycheck-ciao-measure-time'.")

(defun flycheck-ciao-register-time ()
  "Register time between two steps by adding it to `flycheck-ciao-registered-times'."
  (when flycheck-ciao-registered-times
    (push (current-time) flycheck-ciao-registered-times)))

(defun flycheck-ciao-start-timer ()
  "Start timer for measuring time when checking with Flycheck a Ciao buffer."
  (when (eq major-mode 'ciao-mode)
    (push (current-time) flycheck-ciao-registered-times)))

(defun flycheck-ciao-end-timer ()
  "End timer for measuring time and save the times in a log file."
  (when flycheck-ciao-registered-times
    (flycheck-ciao-register-time)
    (when (not (file-exists-p flycheck-ciao-time-log-path))
      (with-temp-buffer
	(insert "date,filename,checker,report,parse,highlight")
	(write-file flycheck-ciao-time-log-path)))
    (let ((date (format-time-string "%Y-%m-%dT%T"))
          (filename (buffer-file-name))
          (highlight-t (pop flycheck-ciao-registered-times))
          (parse-t (pop flycheck-ciao-registered-times))
          (report-t (pop flycheck-ciao-registered-times))
          (initial-t (pop flycheck-ciao-registered-times))
          (log-new-record nil))
      (setq log-new-record
            (concat "\n"
		    date ","
		    filename ","
                    (format "%s" (flycheck-get-checker-for-buffer)) ","
		    (format "%0.09f" (float-time (time-subtract report-t initial-t))) ","
		    (format "%0.09f" (float-time (time-subtract parse-t report-t))) ","
		    (format "%0.09f" (float-time (time-subtract highlight-t parse-t)))))
      (with-temp-buffer
        (insert log-new-record)
	(append-to-file (point-min) (point-max) flycheck-ciao-time-log-path)))))

(defvar-local flycheck-ciao-measure-time-active
  nil
  "Boolean variable to determine if Flycheck times should be measured when checking Ciao buffers.")

(defun flycheck-ciao-measure-time ()
  "Measure time lasted each time Flycheck check a buffer."
  (interactive)
  (if flycheck-ciao-measure-time-active
      (progn
	(message "Flycheck measure time toggled off.")
	(setq flycheck-ciao-measure-time-active nil)
	(remove-hook 'flycheck-before-syntax-check-hook 'flycheck-ciao-start-timer)
        (remove-hook 'flycheck-after-syntax-check-hook 'flycheck-ciao-end-timer))
    (message "Flycheck measure time toggled on.")
    (setq flycheck-ciao-measure-time-active t)
    (add-hook 'flycheck-before-syntax-check-hook 'flycheck-ciao-start-timer)
    (add-hook 'flycheck-after-syntax-check-hook 'flycheck-ciao-end-timer)))

(provide 'flycheck-ciao)
;;; flycheck-ciao.el ends here
