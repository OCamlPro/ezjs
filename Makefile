all: build

build:
	dune build libs

clean:
	dune clean

install:
	dune install

doc:
	dune build @doc
