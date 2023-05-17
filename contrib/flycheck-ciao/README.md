# flycheck-ciao

Flycheck support for Ciao Prolog (`ciao-mode`) adding different checkers:

* `ciaopp`: uses CiaoPP for checking assertions, tests, and syntax.
* `lpdoc`: uses PDdoc for checking documentation.
* `ciaoc`: uses CiaoC for checking syntax and compilation.
* `ciao-test`: uses CiaoC for checking by testing.

## Installation and Setup
Emacs 24 is needed for using [flycheck](https://github.com/flycheck/flycheck).
If you don't have `flycheck` installed yet, installing this package
(`flycheck-ciao`) will do it automatically from MELPA repository, but first, as 
MELPA repository is not available in Emacs by default, you 
will have to add this code to your Emacs init file.

```	emacs-lisp
(require 'package)

(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
```
Install the `flycheck-ciao` package via the Emacs built-in package manager (`package.el`).
```
[ALT+X]package-install-file[RET](flycheck-ciao.el PATH)[RET]
```
>  Note: If you don't have `flycheck` installed and a warning message appears saying 
> that the `flycheck` package is unavailable, refresh the packages from the repositories.
> ```
> [ALT+X]package-refresh-contents[RET]
>``` 
>  Once the packages are refreshed, try installing again and 
>  `flycheck` should be installed along with `flycheck-ciao`.


Insert the next line into your Emacs init file for adding the checkers.
```	emacs-lisp
(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-ciao-setup))
```
If you use [use-package](https://github.com/jwiegley/use-package), you can insert instead
```	emacs-lisp
(use-package flycheck-ciao
  :after flycheck
  :hook
  (flycheck-mode . flycheck-ciao-setup)
  )
```
> **Optional**: Enable `flycheck-mode` in all buffers where syntax checking is possible.
> ```	emacs-lisp
> (add-hook 'after-init-hook #'global-flycheck-mode)
> ```

# Additional setup

- This can be useful: 

```	emacs-lisp
(defface flycheck-info
  ;; Use just margin mark (no underline) for info-level messages
  ;; (e.g., checked)
  '((t))
  "Flycheck face for informational messages."
  :group 'flycheck-faces)
```

- By default flycheck info messages (pop-ups) are shown in the
  minibuffer.  An alternative is flycheck-pos-tip-mode. The following
  mods fix a minor issue and allow font-lock-based highlighting of the
  messages in the pop-ups.

```	emacs-lisp
(with-eval-after-load 'flycheck
  (progn
    (flycheck-pos-tip-mode)
    (setq flycheck-pos-tip-timeout 0)
    ;; Fixes problem with tool tip width (was one character short)
    (defun pos-tip-tooltip-width (width char-width)
      "Calculate tooltip pixel width."
      (+ (* width char-width)
         (ash (+ pos-tip-border-width
	         pos-tip-internal-border-width)
	      2)))
    ;; Fix so that text properties are displayed
    (defun flycheck-pos-tip-error-messages (errors)
      "Display ERRORS, using a graphical tooltip on GUI frames."
      (when errors
        (if (display-graphic-p)
            (let ((message (flycheck-help-echo-all-error-messages errors))
                  (line-height (car (window-line-height))))
              (flycheck-pos-tip--check-pos)
              ;; MH: This is the relevant change:
              ;; pos-tip-show -> pos-tip-show-no-propertize
              (pos-tip-show-no-propertize message nil nil nil flycheck-pos-tip-timeout
                                          flycheck-pos-tip-max-width nil
                                          ;; Add a little offset to the tooltip to move it away
                                          ;; from the corresponding text in the buffer.  We
                                          ;; explicitly take the line height into account because
                                          ;; pos-tip computes the offset from the top of the line
                                          ;; apparently.
                                          nil (and line-height (+ line-height 5)))
              )
          (funcall flycheck-pos-tip-display-errors-tty-function errors))))))
```

