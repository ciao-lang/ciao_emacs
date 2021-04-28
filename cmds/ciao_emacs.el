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


;; Create third-party directory
(let ((third-party-dir
       (expand-file-name "elisp" (expand-file-name "third-party"
						   (ciao-get-config
						    :root-dir)))))
      (if (not (file-directory-p third-party-dir))
	  (mkdir third-party-dir))

      ;; Set directory for installed packages
      (setq package-user-dir third-party-dir))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install third-party packages 
(if (not (package-installed-p 'use-package))
    (package-install 'use-package))

(if (not (package-installed-p 'flycheck))
    (package-install 'flycheck))

(if (not (package-installed-p 'company))
    (package-install 'company))

;; Install recommended extensions for third-party packages
(if (not (package-installed-p 'flycheck-pos-tip))
    (package-install 'flycheck-pos-tip))

(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))

(if (not (package-installed-p 'flycheck-color-mode-line))
    (package-install 'flycheck-color-mode-line))

(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; Add ciao_emacs contrib packages to our load-path
(let ((ciao-emacs-contrib-dir
       (expand-file-name "contrib" (ciao-get-config
		    		    :bundledir-ciao-emacs))))
  (add-to-list 'load-path (expand-file-name "flycheck-ciao" ciao-emacs-contrib-dir))
  (add-to-list 'load-path (expand-file-name "company-ciao" ciao-emacs-contrib-dir)))

;; Setup packages
(require 'flycheck-ciao)
(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook 'flycheck-ciao-setup))

(require 'company-ciao)
(eval-after-load 'company '(add-hook 'company-mode-hook 'company-ciao-setup))

;; Enable Flycheck and Company in buffers
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook #'global-company-mode)

;; ---------------------------------------------------------------------------
;; Theme

(use-package doom-themes
  :ensure t
  ;; :load-path "themes"
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;(load-theme 'doom-one-light t)
  (load-theme 'doom-nord t)
  )

;; ---------------------------------------------------------------------------
;; Start an emacs server

(setq server-name "ciao-emacs")
(setenv "EMACS_SOCKET_NAME" "ciao-emacs") ;; Make sure that emacsclient sees it
(server-start)

;; ---------------------------------------------------------------------------
;; Start Ciao

;; TODO: ciao sytem should be ciaosh, not ciao (use ciao for builder, etc.)
;; Add auxemacs.pl to toplevel args to preload it
(setq ciao-system
      (concat (ciao-get-config :bin-dir) "/ciaosh"))
(setq ciao-system-args
      (concat "-u " (concat (ciao-get-config :bundledir-ciao-emacs) "/cmds/auxemacs.pl")))

(switch-to-buffer "*Ciao*")
;; Insert big logo
(insert "

")
(insert "                                         ")
(insert-image
 (create-image (concat (ciao-get-config :root-dir) "/core/doc/common/ciao-logo.png")))

(insert "

                  Ciao is an open-source project publicly developed at https://github.com/ciao-lang/ciao
                  Please report issues at https://github.com/ciao-lang/ciao/issues
                
                  New to Prolog? Visit https://ciao-lang.org/documentation.html
                  for courses, tutorials, and general documentation.
                  
                  Visit the Ciao manual at https://ciao-lang.org/ciao/build/doc/ciao.html/

       ----------------------------------------------------------------------------------------------------------
       Useful emacs commands:
       
         M-x info   Info documentation (search for Ciao)
         C-x C-f    Find a file
         C-x o      Switch window
         C-x b      Switch buffer
         C-c C-l    Load into the Ciao top level
         C-x C-c    Exit
       ----------------------------------------------------------------------------------------------------------
       Some useful commands for editing, loading, and debugging:
       
         ?- pwd.                           % Show the current directory
         ?- ls.                            % List files in the current directory
         ?- cd('somedir').                 % Change directory
         ?- edit('file.pl').               % Edit file.pl
         ?- use_module('file.pl').         % Load the module
         ?- make_exec('file').             % Create an executable (requires main/{0,1})
         ?- process_call('file', [], []).  % Execute as a separate process                 
         ?- help.                          % Open help
         ?- search.                        % Open search box
       ----------------------------------------------------------------------------------------------------------

")
(defun ciao-insert-logo-richer (cproc procbuffer) t) ;; nicer way?
(advice-add 'ciao-insert-logo :override #'ciao-insert-logo-richer)
(ciao)
(goto-address-mode)
