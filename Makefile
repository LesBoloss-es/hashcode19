.PHONY: build run clean cleanall

LOGLEVEL ?= debug
PROBLEMS ?= problems
SOLUTIONS ?= solutions
WORKERS ?= 1

build:
	dune build @install
	[ -e bin ] || ln -s _build/install/default/bin bin

run: build
	rm -f solutions/*.lock
	bin/hashcode19             \
		--log-level $(LOGLEVEL)  \
		--problems $(PROBLEMS)   \
		--solutions $(SOLUTIONS) \
		--workers $(WORKERS)

zip: clean
	zip -9 -r sources.zip *

clean:
	dune clean
	rm -f bin sources.zip
	@printf 'Try `make cleanall` to remove also solutions.\n'

cleanall: clean
	rm -f solutions/*
