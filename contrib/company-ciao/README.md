# company-ciao

Company completion support for `ciao-mode`.

## Installation and Setup
Emacs 24 is needed for using [company](http://company-mode.github.io/).
If you don't have `company` installed yet, installing this package
(`company-ciao`) will do it automatically from ELPA repository.

Install `company-ciao` package via Emacs built-in package manager (`package.el`).
```
[ALT+X]package-install-file[RET](company-ciao.el PATH)[RET]
```
>  Note: If you don't have `company` installed and a warning message appears saying 
> `company` package is unavailable, refresh the packages from the repositories.
> ```
> [ALT+X]package-refresh-contents[RET]
>``` 
>  Once the packages are refreshed, try installing again and 
>  `company` should be installed along with `company-ciao`.


Insert the next line into your Emacs init file for adding the checkers.
```	emacs-lisp
(eval-after-load 'company '(add-hook 'company-mode-hook #'company-ciao-setup))
```
If you use [use-package](https://github.com/jwiegley/use-package), you can insert instead
```	emacs-lisp
(use-package company-ciao
  :after company
  :hook
  (company-mode . company-ciao-setup)
  )
```
> **Optional**: Enable `company-mode` in all buffers where possible.
> ```	emacs-lisp
> (add-hook 'after-init-hook 'global-company-mode)
> ```


