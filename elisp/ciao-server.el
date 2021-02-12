;;; ciao-server.el --- Interface to Ciao server process
;; Copyright (C) 2021 Jose F. Morales <jfmcjf@gmail.com>

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

(defvar ciao-server-process nil
  "The current Ciao server process.")

;;;###autoload
(defun ciao-server-start () 
  "Start a Ciao server process using `ciao-serve'.

If a server is already running, restart it. 

To stop the server do \\[ciao-server-stop].

To check from a Lisp program whether a Ciao server is running, use
the `ciao-server-process' variable."
  (interactive)
  (if ciao-server-process
      (progn
        (message "Restarting Ciao server")
        (ciao--server-kill))
    (message "Starting Ciao server"))
  (setq ciao-server-process 
        (start-process
         "ciao-serve" "*Ciao-Server*"
         (concat ciao-bin-dir "/ciao-serve"))))

;;;###autoload
(defun ciao-server-stop () 
  "Stop the Ciao server process"
  (interactive)
  (when (get-process "ciao-serve")
    (message "Stopping Ciao server")
    (ciao--server-kill)))

(defun ciao--server-kill ()
  "Kill the main server process and the daemons"
  (delete-process ciao-server-process)
  (setq ciao-server-process nil)
  (call-process (concat ciao-bin-dir "/ciao-serve")
                nil 
                "*Ciao-Server*"
                nil
                "stop"))


;; Provide ourselves:

(provide 'ciao-server)

;;; ciao-server.el ends here

