
OCB = ocamlbuild
LIPSUM  = https://github.com/lindig/lipsum.git

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

update:
	git subtree pull --prefix lipsum $(LIPSUM) master --squash	

init: 
	git subtree add --prefix lipsum $(LIPSUM) master --squash	
