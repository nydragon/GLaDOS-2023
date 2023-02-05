all: clean
	@cabal run glados -- $(ARGS)

test: clean
	@cabal test --test-show-details=direct


build: clean
	@rm -rf ./dist-newstyle
	@make all
	@find .  -wholename "*glados/glados" -exec cp {} . \;


integration-test: clean
	@cabal run test-integration

clean:
	@rm -f *.tix