.PHONY: test

all: test test-included-ido

test:
	cask exec buttercup tests

test-included-ido:
	cask exec buttercup -L included-ido tests

all-tests: test test-included-ido
