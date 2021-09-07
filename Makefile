.PHONY: all
all: lib example

.PHONY: lib
lib:
	dune build @all

.PHONY: example
example:
	dune build

.PHONY: clean
clean:
	dune clean
