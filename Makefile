SHELL := /bin/bash

# Binary Names
COMPILER_NAME = glados
RUNNER_NAME = runner

all: clean
	@cabal run glados -- $(ARGS)

build:
ifeq ($(filter $(COMPILER_NAME),$(MAKECMDGOALS)),$(COMPILER_NAME))
	@cabal build $(COMPILER_NAME)
	@find . -wholename "*$(COMPILER_NAME)/$(COMPILER_NAME)" -exec ln -fs {} ./$(COMPILER_NAME)_lnk \;
else ifeq ($(filter $(RUNNER_NAME),$(MAKECMDGOALS)),$(RUNNER_NAME))
	@cabal build $(RUNNER_NAME)
	@find . -wholename "*$(RUNNER_NAME)/$(RUNNER_NAME)" -exec ln -fs {} ./$(RUNNER_NAME)_lnk \;
else
	@cabal build $(COMPILER_NAME)
	@cabal build $(RUNNER_NAME)
	@find . -wholename "*$(COMPILER_NAME)/$(COMPILER_NAME)" -exec ln -fs {} ./$(COMPILER_NAME)_lnk \;
	@find . -wholename "*$(RUNNER_NAME)/$(RUNNER_NAME)" -exec ln -fs {} ./$(RUNNER_NAME)_lnk \;
endif

clean:
	@rm -rf ./dist-newstyle
	@rm -f $(RUNNER_NAME)_lnk
	@rm -f $(COMPILER_NAME)_lnk

re: clean build

test: clean
ifeq ($(filter unit,$(MAKECMDGOALS)),unit)
	@cabal run unit-tests --test-show-details=direct
else ifeq ($(filter integration,$(MAKECMDGOALS)),integration)
	@cabal run integration-tests --test-show-details=direct
else
	@cabal run unit-tests --test-show-details=direct
	@cabal run integration-tests --test-show-details=direct
endif

.PHONY: all build test clean glados