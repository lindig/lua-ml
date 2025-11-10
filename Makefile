.PHONY: all
all: lib example

.PHONY: lib
lib:
	dune build @all

.PHONY: example
example:
	dune build example/luaclient.exe

.PHONY: doc-noweb
doc-noweb:
	docker build -t luaml-noweb:latest -f doc/Dockerfile doc

	echo docker run --rm -v .:/mnt/project luaml-noweb:latest cp /opt/lua-ml/doc/luaclient.tex /mnt/project/doc/luaclient.tex
	docker run --rm -v .:/mnt/project luaml-noweb:latest cp /opt/lua-ml/luaclient.pdf /mnt/project/doc/luaclient.pdf

	echo docker run --rm -v .:/mnt/project luaml-noweb:latest cp /opt/lua-ml/doc/lua-ml.tex /mnt/project/doc/lua-ml.tex
	docker run --rm -v .:/mnt/project luaml-noweb:latest cp /opt/lua-ml/lua-ml.pdf /mnt/project/doc/lua-ml.pdf

.PHONY: clean
clean:
	dune clean
