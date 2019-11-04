# A test suite for Emacs' ido

## Running the tests

This package comes with a test suite. If you want to run it yourself,
first install the [cask](http://cask.readthedocs.io/en/latest/)
dependency manager. Then, from the package directory, run `cask
install` to install all the development dependencies, in
particular
[buttercup](https://github.com/jorgenschaefer/emacs-buttercup).
Finally, to run the tests, execute `cask exec buttercup -L .`. Please
run this test suite before submitting any pull requests, and note in
the pull request whether any of the tests fail.
