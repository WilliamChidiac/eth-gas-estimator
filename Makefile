BUILD=dune
BIN_DIR=_bin
BUILD_DIR=_build/default
BINARY=gas-estimator.exe
VERSION=src/common/version.ml
GIT=$(shell git log --pretty=format:'%H' -n 1)
RELEASE_VERSION=0.1
DATE=$(shell date)
SCRIPTS=scripts/

all: version build copy-binaries

build:
	$(BUILD) build

opam-switch:
	@opam switch create . ocaml-base-compiler.4.14.1 -y --no-install

deps:
	@scripts/install-deps.sh

copy-binaries:
	@mkdir -p $(BIN_DIR)
	@rm -f $(BIN_DIR)/$(BINARY)
	@cp -f $(BUILD_DIR)/src/main/main.exe $(BIN_DIR)/$(BINARY)
	@cp -f $(BUILD_DIR)/test/test.exe $(BIN_DIR)/tests.exe

format:
	@$(BUILD) build @fmt --auto-promote 2> /dev/null || exit 0

lint:
	@$(SCRIPTS)/lint.sh

version:
	@echo "let version = \"$(RELEASE_VERSION)\"" > $(VERSION)
	@echo "let date = \"$(DATE)\"" >> $(VERSION)
	@echo "let commit = \"$(GIT)\"" >> $(VERSION)
	@echo "let pp_version () =" >> $(VERSION)
	@echo "   Format.printf \"## Ethereum Gas estimator@.  Version %s @.  Latest release %s @.  Commit %s@.\"" >> $(VERSION)
	@echo "   version date commit" >> $(VERSION)

clean:
	$(BUILD) clean

alchemy: all
	./$(BIN_DIR)/$(BINARY) alchemy-mode