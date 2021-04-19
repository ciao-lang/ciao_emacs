;; ---------------------------------------------------------------------------
;; Some sane defaults

(setq-default indent-tabs-mode nil)
;;(setq pop-up-windows nil)
;;(tool-bar-mode 0)
;;(tooltip-mode  0)
;;(scroll-bar-mode 0)

;; ---------------------------------------------------------------------------
;; Initial frame 

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(setq default-frame-alist
      (append (list '(width  . 132) '(height . 50))))
                    ; '(vertical-scroll-bars . nil)
                    ; '(internal-border-width . 24)
                    ; '(font . "Courier 14"))))

;; ---------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; ---------------------------------------------------------------------------
;; Start an emacs server

(setq server-name "ciao-emacs")
(setenv "EMACS_SOCKET_NAME" "ciao-emacs") ;; Make sure that emacsclient see it
(server-start)

;; ---------------------------------------------------------------------------
;; Start Ciao

(switch-to-buffer "*Ciao*")
(insert "
Welcome to Ciao!

  Ciao is an open-source project publicly developed at 
    https://github.com/ciao-lang/ciao
  Please report issues at https://github.com/ciao-lang/ciao/issues

  New to Prolog? Visit https://ciao-lang.org/documentation.html
  for courses, tutorials, and general documentation.
  
  Visit the Ciao manual at https://ciao-lang.org/ciao/build/doc/ciao.html/

Useful emacs commands:

  M-x info   Info documentation (search for Ciao)
  C-x C-f    Find a file
  C-x o      Switch window
  C-x b      Switch buffer
  C-c C-l    Load into the Ciao top level

Some useful commands for editing, loading, and debugging:

  ?- working_directory(D,D).            % See the current directory
  ?- working_directory(_,'somedir').    % Change directory
  [?- use_module(library(emacs)).]
  ?- emacs_edit('file.pl').             % Edit file.pl in emacs (requires server-start)
  ?- use_module('file.pl').             % Load the module
  ?- make_exec('file').                 % Create an executable (requires main/{0,1})
  ?- process_call('file', [], []).      % Execute as a separate process

")
(ciao)
(goto-address-mode)
