# Ciao Emacs Plus

This directory contains some extensions and goodies for the Ciao emacs
mode:

 - Flycheck and Company support for Ciao Prolog (`ciao-mode`):
   - Flycheck provides on the fly checking of assertions, tests,
     syntax, and documentation.
   - Company provides rich contextual help, by accessing the manuals.

 - `agda-input.el`: a copy of the Agda input method (MIT License) to type
   mathematican and other symbols in Unicode.

 - `outli.el`: a copy of [outli](https://github.com/jdtsmith/outli) (GPL License)
   an Org-like code outliner for section folding.

## Installation and Setup

Emacs 24 is needed for using [flycheck](https://github.com/flycheck/flycheck).
If you don't have `flycheck` or `company` installed yet, installing this package
(`ciao-emacs-plus`) will do it automatically. 

As the MELPA repository is not available in Emacs by default and it is
needed to install `flycheck`, you will have to add this code to your
Emacs init file:

```	emacs-lisp
(require 'package)

(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
```
Install `ciao-emacs-plus` package via Emacs built-in package manager (`package.el`).
```
[ALT+X]package-install-file[RET](ciao-emacs-plus.el PATH)[RET]
```
> Note: If you don't have `flycheck` or `company` installed and a message warns you 
> that a package is unavailable, try refreshing the packages from the repositories.
> ```
> [ALT+X]package-refresh-contents[RET]
>``` 
> Once the packages are refreshed, try installing again.

For `flycheck` support, add the next line into your Emacs init file for adding the checkers:
```	emacs-lisp
(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook 'flycheck-ciao-setup))
```
If you use [use-package](https://github.com/jwiegley/use-package), you can insert instead:
```	emacs-lisp
(use-package flycheck-ciao
  :after flycheck
  :hook
  (flycheck-mode . flycheck-ciao-setup)
  )
```
> **Optional**: Enable `flycheck-mode` in all buffers where syntax checking is possible:
> ```	emacs-lisp
> (add-hook 'after-init-hook 'global-flycheck-mode)
> ```

For Company support, insert the following line into your Emacs init file:
```	emacs-lisp
(eval-after-load 'company '(add-hook 'company-mode-hook 'company-ciao-setup))
```
If you use [use-package](https://github.com/jwiegley/use-package), you can insert instead:
```	emacs-lisp
(use-package company-ciao
  :after company
  :hook
  (company-mode . company-ciao-setup)
  )
```
> **Optional**: Enable `company-mode` in all buffers where possible:
> ```	emacs-lisp
> (add-hook 'after-init-hook 'global-company-mode)
> ```
See also the instructions and tips in each individual package. 
