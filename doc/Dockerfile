FROM debian:stable-slim

# texlive-plain-generic: path.sty
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        noweb \
        texlive-fonts-recommended \
        texlive-latex-base \
        texlive-plain-generic \
        && \
    rm -rf /var/lib/apt/lists/*

COPY noweb /opt/lua-ml/doc/noweb
COPY cmm /opt/lua-ml/doc/cmm
WORKDIR /opt/lua-ml

# Luaclient (A Sample Client for Lua-ML) is a separate document
# since it has its own `\documentclass[11pt]{article}`
RUN noweave -delay -filter doc/cmm/autodefs.ocaml \
    -latex \
    -index \
    doc/noweb/luaclient.nw > doc/luaclient.tex
# But:
#     $${\Tt{}module\ G\ =\ Lua.Run(I)\nwendquote}$$    
# causes:
#     ! LaTeX Error: Command \ttfamily invalid in math mode.
# so strip out the $$ and replace it with a bit of horizontal space.
RUN sed -E -i 's#^    \$\$(.*)\$\$#\\hspace*{2em} \1#' doc/luaclient.tex
RUN pdflatex doc/luaclient.tex && pdflatex doc/luaclient.tex

# Everything else goes into a "Lua-ML" document
RUN noweave -delay \
    -filter doc/cmm/autodefs.ocaml \
    -latex \
    -index \
    doc/noweb/lua.nw \
    #   Modules for building Lua libraries
    doc/noweb/lualib.nw \
    #   Core interpreter for Lua statements and expressions
    doc/noweb/luastdinterp.nw \
    #   Standalone Lua interpreters
    doc/noweb/luarun.nw \
    #   Lua values, parameterized by user data
    doc/noweb/luavalue.nw \ 
    #   luabaselib.mli
    doc/noweb/luabaselib.nw \
    #   Excerpts from the Caml library, imported into Lua
    doc/noweb/luacamllib.nw \
    #   luastrlib.mli
    doc/noweb/luastrlib.nw \
    #   luamathlib.mli
    doc/noweb/luamathlib.nw \
    #   Lua I/O library
    doc/noweb/luaiolib.nw \
    #   luahash.mli
    doc/noweb/luahash.nw \
    #   Abstract syntax for Lua
    doc/noweb/luaast.nw \
# 0.670 ! Undefined control sequence.
# 0.670 l.1720 \nwaddbox
# 0.670                 {apply : value -> state -> value list -> value list}    
    #   ! LaTeX Error: Command \ttfamily invalid in math mode.
    # doc/noweb/luasyntax.nw \
    #   The section 'Source Code Locations' belongs with luasyntax.
    # doc/noweb/luasrcmap.nw \
    > doc/lua-ml.inc

RUN cat doc/cmm/texheader.tex doc/lua-ml.inc doc/cmm/texfooter.tex > doc/lua-ml.tex
RUN pdflatex doc/lua-ml.tex && pdflatex doc/lua-ml.tex
