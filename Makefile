.PHONY: build clean

build:
	dune build @install

clean:
	dune clean
