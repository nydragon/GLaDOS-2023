SHELL := /bin/bash

all: clean
	@cabal run glados -- $(ARGS)

build:
ifeq ($(MAKECMDGOALS),build)
	@cabal build glados
	@cabal build runner
else
ifeq ($(filter glados,$(MAKECMDGOALS)),glados)
	@cabal build glados
endif

ifeq ($(filter runner,$(MAKECMDGOALS)),runner)
	@cabal build runner
endif
endif

# build: clean
# 	@rm -rf ./dist-newstyle
# 	@cabal build glados
# 	@find .  -wholename "*glados/glados" -exec cp {} . \;

test: clean
	@cabal test --test-show-details=direct

unit-test: clean
	@cabal run unitTest --test-show-details=direct

integration-test: clean
	@cabal run integrationTest --test-show-details=direct

clean:
	@rm -rf ./dist-newstyle

.PHONY: all build tests integration-test unit-test clean