all: clean
	@cabal run glados -- $(ARGS)

build: clean
	@rm -rf ./dist-newstyle
	@cabal build glados
	@find .  -wholename "*glados/glados" -exec cp {} . \;

unitTest: clean
	@cabal run unitTest --test-show-details=direct


integration-test: clean
	@cabal run test-integration --test-show-details=direct

clean:
	@rm -f *.tix

.PHONY: all test integrationTest unitTest clean