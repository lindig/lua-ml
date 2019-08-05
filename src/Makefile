#
# Lua ML - an embeddable  Lua 2.5 interpreter implemented in OCaml
#

OCY = ocamlyacc

all: luaparser

clean:
	rm -f luaparser.ml luaparser.mli

.PHONY: luaparser
.ONESHELL: luaparser
luaparser:
	$(OCY) luaparsex.mly
	# Some handwritten bits are required for the parser interface
	rm luaparsex.mli
	mv luaparsex.ml luaparser.ml
	cp _luaparser.mli luaparser.mli
