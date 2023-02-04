all: clean
	@cabal run glados -- $(ARGS)

test: clean
	@cabal test --test-show-details=direct

clean:
	@rm -f *.tix;