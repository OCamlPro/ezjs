-include autoconf/Makefile.config

all: build

build:
	dune build

clean:
	dune clean

install:
	dune install

ocp-build-conf:
	ocp-autoconf

ocp-build: ocp-build-conf
	ocp-build $(PROJECT_BUILD)

ocp-build-install: ocp-build-install $(PROJECT_INSTALL)

ocp-build-clean: ocp-build-clean $(PROJECT_CLEAN)

test: ocp-build
	cp _obuild/main/main.js test

chrome-extension-example: ocp-build
	cp _obuild/background-example/background-example.js examples/chrome-extension
	cp _obuild/popup-example/popup-example.js examples/chrome-extension
	cp _obuild/options-example/options-example.js examples/chrome-extension
	cp _obuild/chrome-example/chrome-example.js examples/chrome-extension

-include autoconf/Makefile.rules
