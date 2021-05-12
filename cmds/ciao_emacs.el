;; ---------------------------------------------------------------------------
;; ciao_emacs.el -- Boot an emacs-based graphical user interface for Ciao
;; (C) 2021 The Ciao Development Team

;; ---------------------------------------------------------------------------
;; Some sane defaults

(setq-default indent-tabs-mode nil)
;;(setq pop-up-windows nil)
;;(tool-bar-mode 0)
;;(tooltip-mode  0)
;;(scroll-bar-mode 0)
;; Disable irritating visual line move
(setq line-move-visual nil)
;; Disable irritating ring bell function
(setq ring-bell-function 'ignore)
;; No blinking cursor
(blink-cursor-mode 0)

(setq mac-command-modifier      'meta ;'super
      ns-command-modifier       'meta ;'super
      mac-option-modifier       'meta
      ns-option-modifier        'meta
      mac-right-option-modifier 'none
      ns-right-option-modifier  'none)

;; ---------------------------------------------------------------------------
;; Initial frame 

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(setq default-frame-alist
      (append (list '(width  . 132) '(height . 50))))
                    ; '(vertical-scroll-bars . nil)
                    ; '(internal-border-width . 24)
                    ; '(font . "Courier 14"))))

(set-face-attribute 'default nil :height 140)

;; ---------------------------------------------------------------------------
;; Setup packages
;;
;; NOTE: Packages will be stored separatedly in the third-party/elisp
;; directory, so that there is no interaction with any pre-existing
;; emacs installation.

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
;; 3rd party dir, make sure that it exists
(setq third-party-dir (expand-file-name "third-party" (ciao-get-config :root-dir)))
(if (not (file-directory-p third-party-dir))
    (make-directory third-party-dir))

;; Local directory for ciao emacs data (so that it does not collide
;; with other emacs installations). Make sure that it exists
(setq ciao-emacs-init-dir (expand-file-name "emacs" third-party-dir))
(if (not (file-directory-p ciao-emacs-init-dir))
    (make-directory ciao-emacs-init-dir))

;; Set local directory for packages, make sure that it exists
(let ((dir (expand-file-name "elpa" ciao-emacs-init-dir)))
  (if (not (file-directory-p dir))
      (make-directory dir))
  ;; Set directory for installed packages
  (setq package-user-dir dir))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; ---------------------------------------------------------------------------
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

;(if (not (package-installed-p 'flycheck-inline))
;    (package-install 'flycheck-inline))
;
;(with-eval-after-load 'flycheck
;  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

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
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (add-hook 'after-init-hook #'global-company-mode)
(global-flycheck-mode)
(global-company-mode)

;; ---------------------------------------------------------------------------
;; xwidgets-webkit (optional)

(if (not (package-installed-p 'xwidgets-reuse))
    (package-install 'xwidgets-reuse))

;; ---------------------------------------------------------------------------
;; Markdown mode

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; ---------------------------------------------------------------------------
;; Treemacs sidebar (for easy file navigation)

(let ((f (expand-file-name "treemacs-persist" ciao-emacs-init-dir)))
  (if (not (file-exists-p f))
      (with-temp-file f
        (insert "* Examples\n")
        (insert "** Examples\n")
        (insert "- path :: ")
        (insert (expand-file-name "core/examples/misc" (ciao-get-config :root-dir)))
        (insert "\n")
        (insert "* Ciao\n")
        (insert "** Ciao\n")
        (insert "- path :: ")
        (insert (ciao-get-config :root-dir))
        (insert "\n"))))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          ;; TODO: use local instead?
          ;; treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-persist-file                  (expand-file-name "treemacs-persist" ciao-emacs-init-dir)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; Override .pl, .pro, .prolog extensions
    (treemacs-create-icon
     :file (expand-file-name "elisp/icons/prolog-icon.png"
                             (ciao-get-config :bundledir-ciao-emacs))
     :extensions ("pro" "prolog" "pl"))

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

;; (use-package treemacs-icons-dired
;;   :after (treemacs dired)
;;   :ensure t
;;   :config (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :after (treemacs magit)
;;   :ensure t)
;; 
;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

;; ---------------------------------------------------------------------------
;; Other customization

(use-package which-key
  :ensure t)

;; ---------------------------------------------------------------------------
;; Theme

(use-package doom-themes
  :ensure t
  ;; :load-path "themes"
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)
;  (load-theme 'doom-spacegrey t)
;  (load-theme 'doom-one t)
;  (load-theme 'doom-dracula t)
;  (load-theme 'doom-tomorrow-night t)
;  (load-theme 'doom-challenger-deep t)
;  (load-theme 'doom-opera t)
;  (load-theme 'doom-tomorrow-day t)
;  (load-theme 'doom-one-light t)
  )

;; ---------------------------------------------------------------------------
;; Further Ciao mode customization

;; Special faces for some Ciao properties
(font-lock-add-keywords 'ciao-mode
                        '(("\\<\\(atm\\|list\\|int\\|nonvar\\|var\\|gnd\\|ground\\|string\\|flt\\|term\\|num\\|nnegint\\|mshare\\|indep\\|not_fails\\|fails\\|non_det\\|is_det\\)\\>" . font-lock-keyword-face)))

;; ---------------------------------------------------------------------------
;; Start an emacs server

(setq server-name "ciao-emacs")
(setenv "EMACS_SOCKET_NAME" "ciao-emacs") ;; Make sure that emacsclient sees it
(server-start)

(ciao-server-start) ;; Enable Ciao server

;; ---------------------------------------------------------------------------
;; Enable other useful modes

(treemacs)
(which-key-mode)
(global-hl-line-mode)
(add-hook 'ciao-mode-hook 'display-line-numbers-mode)

;; ---------------------------------------------------------------------------
;; Ciao splash

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
(insert "                            ")
(insert-image
 (create-image (concat (ciao-get-config :root-dir) "/core/doc/common/ciao-logo.png")))

(insert "

    Ciao is an open-source project publicly developed at https://github.com/ciao-lang/ciao
    Please report issues at https://github.com/ciao-lang/ciao/issues
      
    New to Prolog? Visit https://ciao-lang.org/documentation.html
    for courses, tutorials, and general documentation.
        
    Visit the Ciao manual at https://ciao-lang.org/ciao/build/doc/ciao.html/

")
;;(insert "
;;       ----------------------------------------------------------------------------------------------------------
;;       Useful emacs commands:
;;       
;;         M-x info   Info documentation (search for Ciao)
;;         C-x C-f    Find a file
;;         C-x o      Switch window
;;         C-x b      Switch buffer
;;         C-c C-l    Load into the Ciao top level
;;         C-x C-c    Exit
;;")
;; (insert "
;;        ----------------------------------------------------------------------------------------------------------
;;        Some useful commands for editing, loading, and debugging:
;;        
;;          ?- pwd.                           % Show the current directory
;;          ?- ls.                            % List files in the current directory
;;          ?- cd('somedir').                 % Change directory
;;          ?- edit('file.pl').               % Edit file.pl
;;          ?- use_module('file.pl').         % Load the module
;;          ?- make_exec('file').             % Create an executable (requires main/{0,1})
;;          ?- process_call('file', [], []).  % Execute as a separate process                 
;;          ?- html_help.                     % Open help (HTML)
;;          ?- html_search.                   % Open search box (HTML)
;; ")
;; (insert "
;;        ----------------------------------------------------------------------------------------------------------
;; 
;; ")
(defun ciao-insert-logo-richer (cproc procbuffer) t) ;; nicer way?
(advice-add 'ciao-insert-logo :override #'ciao-insert-logo-richer)
(ciao)
(goto-address-mode)
(switch-to-buffer "*Ciao*")
(delete-other-windows)
