.PHONY: build run clean cleanall

LOGLEVEL ?= debug
PROBLEMS ?= problems
SOLUTIONS ?= solutions
WORKERS ?= $(shell cat /proc/cpuinfo | sed -n 's/processor[^:]*:[^0-9]*\([0-9]*\)/\1/p' | sort -nr | head -n 1)

build:
	dune build @install
	[ -e bin ] || ln -s _build/install/default/bin bin

run: build
	bin/hashcode19             \
		--log-level $(LOGLEVEL)  \
		--problems $(PROBLEMS)   \
		--solutions $(SOLUTIONS) \
		--workers $(WORKERS)

clean:
	dune clean
	rm -f bin

cleanall: clean
	rm -f solutions/*
