.PHONY: build run clean cleanall

build:
	dune build @install
	[ -e bin ] || ln -s _build/install/default/bin bin

run: build
	bin/hashcode19

clean:
	dune clean
	rm -f bin

cleanall: clean
	rm -f solutions/*
