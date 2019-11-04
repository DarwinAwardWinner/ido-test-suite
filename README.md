# A test suite for Emacs' ido

This is a test suite for the ido package included with Emacs. I'm
writing these tests so I can muck around in the ido internals and be
reasonably certain that I haven't broken anything in the process.

## Running the tests

First install the [cask](http://cask.readthedocs.io/en/latest/)
dependency manager. Then, from the package directory, run `cask
install` to install all the development dependencies, in particular
the [buttercup](https://github.com/jorgenschaefer/emacs-buttercup)
testing framework. Finally, to run the tests, execute `cask exec
buttercup -L .` (or just run `make`).
