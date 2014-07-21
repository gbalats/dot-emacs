dot-emacs
=========

This is my emacs configuration. It contains:

1. the *init* file
2. some git submodules under `site-lisp/`
3. some additional local packages under `site-lisp/etc`.


Installation
------------

To install everything just run the following commands:

    git submodule init
    git submodule update
    make


Post-installation
-----------------

The first time that emacs is executed (after installation via `make`),
it will automatically download a list of modules from the repositories
listed in the *init*. (This, however, requires the *package-system*
available for emacs versions &ge; 24.)

The list of modules that will be downloaded consist of:

1. the prelude packages listed in
   [prelude-packages.el](/site-lisp/etc/prelude-packages.el)
2. the [use-package][use-package] entries marked for download (via the
   `:ensure t` option) in [the init file](/init.el).


[use-package]: https://github.com/jwiegley/use-package
