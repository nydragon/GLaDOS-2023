all: clean
	@cabal run glados -- $(ARGS)

test: clean
	@cabal test --test-show-details=direct

integration-test: clean
	@rm -rf ./dist-newstyle
	@make all
	@find .  -wholename "*glados/glados" -exec cp {} . \;
	@cabal run test-integration

clean:
	@rm -f *.tix