all: clean
	@cabal run glados -- $(ARGS)

test: clean
	@cabal test --test-show-details=direct

integrationTest: clean
	@cabal run integrationTest --test-show-details=direct

unitTest: clean
	@cabal run unitTest --test-show-details=direct

clean:
	@rm -f *.tix

.PHONY: all test integrationTest unitTest clean