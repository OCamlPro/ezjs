all: build

build:
	dune build libs

clean:
	dune clean

install:
	dune install

doc:
	dune build @doc

example:
	dune build --profile release examples
	@cp -f _build/default/examples/indexedDB/indexedDB_example.bc.js examples/indexedDB/indexedDB-example.js
	@cp -f _build/default/examples/crypto/crypto_example.bc.js examples/crypto/crypto-example.js
