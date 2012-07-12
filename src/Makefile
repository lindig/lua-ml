#
# Lua ML - an embeddable  Lua 2.5 interpreter implemented in Objective Caml
#
# Note: it would be nice to handle the extraction of source code from
# *.nw using OCamlBuild. This requires a custum plugin but I don't know
# how to do this yet.
#

OCB 	= 	ocamlbuild
OCY 	= 	ocamlyacc
LP 	= 	lipsum

# Code that must be tangled from literate programs

MLSRC =\
		lua.ml          \
		lua.mli         \
		luaast.ml       \
		luaast.mli      \
		luabaselib.ml   \
		luabaselib.mli  \
		luacamllib.ml   \
		luacamllib.mli  \
		luaclient.ml 	\
		luafloat.mll	\
		luahash.ml      \
		luahash.mli     \
		luainterp.ml    \
		luainterp.mli   \
		luaiolib.ml     \
		luaiolib.mli    \
		lualib.ml       \
		lualib.mli      \
		luamathlib.ml   \
		luamathlib.mli  \
		luaparser.ml	\
		luaparser.mli	\
		luarun.ml       \
		luarun.mli      \
		luascanner.mll	\
		luasrcmap.ml    \
		luasrcmap.mli   \
		luastrlib.ml    \
		luastrlib.mli   \
		luavalue.ml     \
		luavalue.mli    \

		
all:		$(MLSRC)
		$(OCB) lua-std.cmxa
		$(OCB) luaclient.native

clean:
		rm -f $(MLSRC)
		rm -f luaparsex.mly luaparsex.ml luaparsex.mli
		$(OCB) -clean
		
# rules to extract code from literate programs

%.ml: 		%.nw
		$(LP) tangle $@ $< > $@
		
%.mli: 		%.nw
		$(LP) tangle $@ $< > $@

luasrcmap.ml: 	luasrcmap.nw
		$(LP) tangle srcmap.ml $< > $@

luasrcmap.mli: 	luasrcmap.nw
		$(LP) tangle srcmap.mli $< > $@

luascanner.mll: luasyntax.nw
		$(LP) tangle $@ $< > $@

luafloat.mll:   luavalue.nw
		$(LP) tangle $@ $< > $@

luaparsex.mly: 	luasyntax.nw
		$(LP) tangle luaparser.mly $< > $@

luainterp.ml: 	luastdinterp.nw
		$(LP) tangle $@ $< > $@

luainterp.mli: 	luastdinterp.nw
		$(LP) tangle $@ $< > $@
				
#
# Ugly. To create luaparser.{ml,mli} we first build luaparsex.{ml,mli} 
# and then modify the .mli file.
#

luaparser.ml luaparser.mli: luaparsex.mly
		$(OCY) luaparsex.mly
		mv luaparsex.ml luaparser.ml
		( sed '/^val/,$$d' luaparsex.mli ;\
		$(LP) tangle luaparser.mli luasyntax.nw ) > luaparser.mli
		rm -f luaparsex.mli


		