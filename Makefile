all: clean
	@cabal run glados -- $(ARGS)

build: clean
	@rm -rf ./dist-newstyle
	@cabal build glados
	@find .  -wholename "*glados/glados" -exec cp {} . \;

test: clean
	@cabal test --test-show-details=direct

unit-test: clean
	@cabal run unitTest --test-show-details=direct

integration-test: clean
	@cabal run integrationTest --test-show-details=direct

clean:
	@rm -f *.tix

.PHONY: all build tests integration-test unit-test clean