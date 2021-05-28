;;; company-ciao.el --- Company completion backend for ciao-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  Miguel Angel Sanchez Ordaz

;; Author: Miguel Angel Sanchez Ordaz <ma.sanchez.ordaz@imdea.org>
;; Keywords: convenience languages tools
;; Package-Version: 0.1
;; Package-Requires: ((emacs "24.0") (company "0.8.11") (cl-lib "0.5"))

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
;; Company backend completion for ciao-mode.

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ciao)

;; Optional:
;; Enable Company Mode in all buffers where completion is possible.
;; (add-hook 'after-init-hook 'global-company-mode)

(defvar-local company-ciao-completions
  nil
  "List of Ciao completions for company-mode.")

(defvar-local company-ciao-cache
  (expand-file-name ".cache" (expand-file-name "company-ciao" (expand-file-name "contrib" (ciao-get-config :bundledir-ciao-emacs))))
  "Cache where completions are stored.")

(defun ciao-librowser-apropos-all ()
  "Get the output from calling a Ciao process using apropos/1 from librowser."
  (shell-command-to-string
   (concat (expand-file-name "ciaosh" (ciao-get-config :bin-dir))
	   " -e 'use_module(library(librowser))' -e 'apropos(\'.*\')' -e 'halt'")))

(defun ciao-get-completions ()
  "Get the list of Ciao completions used for company-mode."
  (let ((cache company-ciao-cache)
	(completions-string nil))
    (if company-ciao-completions company-ciao-completions
      (if (file-readable-p cache)
	  (with-temp-buffer
	    (insert-file-contents cache)
	    (setq completions-string (buffer-substring (point-min) (point-max)))
	    )
	(let (output with-arity without-arity)
	  (setq output (ciao-librowser-apropos-all))
	  (setq output (substring-no-properties output (string-match "^[a-zA-Z0-9\$\'_-]+:[a-zA-Z0-9\$\'_-]+/[0-9]+$" output)))
	  (setq with-arity (replace-regexp-in-string "^[a-zA-Z0-9\'_-]+:" "" output))
	  (setq without-arity (replace-regexp-in-string "\/[0-9]+$" "" with-arity))
	  (setq completions-string (concat with-arity without-arity))
	  (with-temp-buffer
            (insert completions-string)
            (write-file cache))))
	  (setq company-ciao-completions (split-string completions-string))
	  company-ciao-completions
	  )))

(defun company-ciao (command &optional arg &rest ignored)
  "Company Ciao backend."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ciao-backend))
    (prefix (and (eq major-mode 'ciao-mode)
                (company-grab-symbol)))
    (candidates
    (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
    (ciao-get-completions)))
    ))

;;;###autoload

;; Setup for company-ciao
(defun company-ciao-setup()
  "Setup Company Ciao."
  (add-to-list 'company-backends #'(company-ciao
				    :with company-dabbrev
                                    ))
  )

(provide 'company-ciao)
;;; company-ciao.el ends here
