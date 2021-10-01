.PHONY: all
all: lib example

.PHONY: lib
lib:
	dune build @all

.PHONY: example
example:
	dune build example/luaclient.exe

.PHONY: clean
clean:
	dune clean
