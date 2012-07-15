
OCB = ocamlbuild

all:
	make -C lipsum all
	make -C src all
	make -C example all
	$(OCB) -I src src/lua-std.cmxa
	$(OCB) -I src -I example example/luaclient.native
	
clean:
	make -C lipsum clean
	make -C src clean
	make -C example clean
	$(OCB) -clean