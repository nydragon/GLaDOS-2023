
all: clean
	@cabal run glados -- $(ARGS)

test: clean
	@cabal run test

clean:
	@rm -f *.tix;