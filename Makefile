all: clean
	@cabal run glados -- $(ARGS)

build: clean
	@rm -rf ./dist-newstyle
	@cabal build glados
	@find .  -wholename "*glados/glados" -exec cp {} . \;

tests: clean
	@cabal test --test-show-details=direct

unitTest: clean
	@cabal run unitTest --test-show-details=direct


integration-test: clean
	@cabal run test-integration --test-show-details=direct

clean:
	@rm -f *.tix

.PHONY: all build tests integrationTest unitTest clean