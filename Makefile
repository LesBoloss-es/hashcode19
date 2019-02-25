.PHONY: build run clean cleanall

WORKERS ?= $(shell cat /proc/cpuinfo | sed -n 's/processor[^:]*:[^0-9]*\([0-9]*\)/\1/p' | sort -nr | head -n 1)
PROBLEMS ?= problems
SOLUTIONS ?= solutions

build:
	dune build @install
	[ -e bin ] || ln -s _build/install/default/bin bin

run: build
	bin/hashcode19             \
		--workers $(WORKERS)     \
		--solutions $(SOLUTIONS) \
		--problems $(PROBLEMS)

clean:
	dune clean
	rm -f bin

cleanall: clean
	rm -f solutions/*
