
OCB = ocamlbuild

all: lib example

lib:
	make -C src all
	$(OCB) -use-ocamlfind -I src src/lua-std.cmxa src/lua-std.cma src/lua-std.cmxs

example:
	$(OCB) -use-ocamlfind -I src -I example example/luaclient.native

clean:
	make -C src clean
	make -C example clean
	$(OCB) -clean

.PHONY: all example lib clean
