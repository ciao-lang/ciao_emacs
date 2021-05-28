;;; ciao-emacs-plus.el --- Set of utilities for Emacs ciao-mode -*- lexical-binding: t; -*-

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
;; This package adds new functionalities the the ciao-mode based on
;; providing the Ciao support for various standard Emacs packages

;;; Code:

(require 'ciao)

;;;###autoload
(progn
  (package-refresh-contents)
  (mapc
   (lambda (package-dir)
     (let ((package-name (concat package-dir ".el"))
	   (ciao-emacs-contrib-dir (expand-file-name "contrib"
						     (ciao-get-config :bundledir-ciao-emacs))))
       (package-install-file
	(expand-file-name package-name
			  (expand-file-name package-dir
					    ciao-emacs-contrib-dir)))))
   '("flycheck-ciao" "company-ciao")))

(provide 'ciao-emacs-plus)
;;; ciao-emacs-plus.el ends here
