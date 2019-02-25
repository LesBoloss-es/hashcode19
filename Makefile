.PHONY: all run build clean

all: build
	@printf "Try to run `make run`."

run:
	bin/hashcode19 --problems problems

build:
	dune build @install
	[ -e bin ] || ln -s _build/install/default/bin bin

clean:
	dune clean
	rm -f bin
