# Logo and toolbar icons

See the `ciao-artwork` repository for original sources for some of the
images in this repository.

The following commands where used to generate the logo images for the
toplevel from `ciao-artwork`:
```
convert -resize x32 ciao-logo/ciao-logo-2018-128h.png ciao-logo.png
convert -resize x64 ciao-logo/ciao-logo-2018-128h.png ciao-logo@2x.png
convert -resize x32 ciaopp-logo/ciaopp-logo-2018-128h.png ciaopp-logo.png
convert -resize x64 ciaopp-logo/ciaopp-logo-2018-128h.png ciaopp-logo@2x.png
```

The `@2x.png` image is used by
[emacs Mac port](https://bitbucket.org/mituharu/emacs-mac) for retina
displays.
