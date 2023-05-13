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
Install `flycheck-ciao` package via Emacs built-in package manager (`package.el`).
```
[ALT+X]package-install-file[RET](flycheck-ciao.el PATH)[RET]
```
>  Note: If you don't have `flycheck` installed and a warning message appears saying 
> `flycheck` package is unavailable, refresh the packages from the repositories.
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


