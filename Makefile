all: clean
	@cabal run glados -- $(ARGS)

test: clean
	@cabal test --test-show-details=direct

integration-test: clean
	@cabal run test-integration

clean:
	@rm -f *.tix