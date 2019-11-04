.PHONY: test

all: test

test:
	cask exec buttercup -L . tests

all-tests: test
