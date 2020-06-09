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

ocp-build-old: ocp-build-conf
	ocp-build $(PROJECT_BUILD)

ocp-build-install-old: ocp-build-install $(PROJECT_INSTALL)

ocp-build-clean-old: ocp-build-clean $(PROJECT_CLEAN)

-include autoconf/Makefile.rules
