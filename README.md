
### Lipsum - Literate Programming Simplified

Lipsum is a command-line utility for literate programming. It stands in the
tradition of [Noweb](http://www.cs.tufts.edu/~nr/noweb/), a popular and
flexible literate programming system by Norman Ramsey. The idea of literate
programming is to keep documentation for programmers and program code in
one file and to arrange it in a way that helps understanding it best. To
actually compile or run the code it needs to be extracted from the literate
program and Lipsum is a tool to do this.

Like Noweb, Lipsum employs a minimal markup to arrange documentation and
code in a file. Also like Noweb, Lipsum is language agnostic and can be
used for almost any programming language and documentation.

        @ Echo prints each command line argument on a line by itself. This
        documentation chunk starts with @ and extends until the beginning
        of the named code chunk below. A lipsum file (`*.lp` by convention)
        is a sequence of code and documentation chunks. Each chunk extends
        until the beginning of the next one (or the end of file.)

        <<echo.c>>=
        /* <<copyright>> */
        #include <stdio.h>

        int main(int argc, char** argv)
        {
                int i;
                for (i=0; i<argc; i++)
                        puts(argv[i]);
                return 0;
        }

        @ By keeping the copyright notice in a chunk by itself it is easy
        to include it in several files. This documenation chunk starts with
        an @ followed by a space and extends until the beginning of the
        next chunk.  Inside of documentation, @ only has special meaning at
        the beginning of a line and hence is unlikely to interfear in most
        use cases.

        <<copyright>>=
        This code is in the public domain.

        @ Below we are extending the code chunk above. 

        <<copyright>>=
        This code is part of the documentation for Lipsum.


To extract the code for `echo.c` for compilation from the file `echo.lp`
using Lipsum, one would run Lipsum like this:

        $ lipsum expand echo.c echo.lp > echo.c
        $ cc -o echo echo.c
            
## Resources for Literate Programming

While literate programming isn't a mass phenomenon among programmers it has
a dedicated following. Here are some resources to learn about its concepts,
strengths, and weaknesses.

* [Noweb Homepage](http://www.cs.tufts.edu/~nr/noweb/)
* [Noweb on Wikipedia](http://en.wikipedia.org/wiki/Noweb)
* [Literate Programming on 
        Wikipedia](http://en.wikipedia.org/wiki/Literate_programming)

Literate programming enjoys popularity in the [R](www.r-project.org/)
community which uses a literate programming system called Sweave which is
also in the tradition of Noweb. R is a system for statistical analysis and
Sweave is mainly used to include statistical analysis into scientific
papers that are typeset with LaTeX.

## Why not using Noweb?

Noweb is a great tool with a flexible architecture that permits a user to
plug in filters to extend it. This makes its installation depend on various
filters that are part of its distribution and that are written in various
languages. While this is usually not a problem if you develop code mostly
for yourself, it adds one more dependency if you want to release code as
open source.

Lipsum is less ambitious: it is just one binary and almost all it does is
extracting code from a literate program. I am planning to use it in
combination with Markdown as a syntax for documentation and to include it
with literate programs that I release as open source.

## Implementation and Installation

Lipsum is implemented in [Objective Caml](http://caml.inria.fr/). While
Objective Caml is available on the Windows platform, this distribution
assumes a Unix environment. It is developed on Mac OS X but should compile
equally well on a Linux system. For compilation, the following tools are
required:

* Objective Caml
* pod2man (part of standard Perl distributions)
* Make
* Unix tools called from the Makefile: install, cp

To compile Lipsum, adjust the Makefile and run `make`. In particular, you
might want to adjust the `PREFIX` variable that controls where the lipsum
binary and the manual are getting installed.

        $ make
        $ make install 
        or
        $ make PREFIX=/usr/local install

The code inlcudes the ocaml-re library that comes with its own build
system. However, we don't rely on it but simply tell ocamlbuild(1) where to
find the relevant files.

## Documentation

Lipsum comes with a Unix manual page `lipsum.1` that is generated from
[`lipsum.pod`](lipsum/blob/master/lipsum.pod). POD is a simple markup
language, much like Markdown, that is used by the Perl community. To view
the manual page prior to installation use `nroff`:

        $ nroff -man lipsum.1 | less
        
After installation it is available using `man lipsum` as usual.

## Source Code

https://github.com/lindig/lipsum.git
https://github.com/ocaml/ocaml-re.git

The source code contains the OCaml-Re library for reguar expressions that
comes with its own license.

    src/        source code for lipsum
    ocaml-re/   library for regular expression

OCaml-Re is included as an Git Subtree to avoid dependencies on remote
repositories.

## License

Lipsum is distributed under the BSD-2 license. The license can be also
displayed by the program:

    $ lipsum copyright
    https://github.com/lindig/lipsum.git
    Copyright (c) 2012, 2013, Christian Lindig <lindig@gmail.com>
    All rights reserved.

    Redistribution and use in source and binary forms, with or
    without modification, are permitted provided that the following
    conditions are met:

    (1) Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
    (2) Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in
        the documentation and/or other materials provided with the
        distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
    CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
    INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
    USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
    AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.

    This program includes a library for regular expressions that
    is available from https://github.com/ocaml/ocaml-re.git.
    It was released under the GNU LESSER GENERAL PUBLIC LICENSE
    and was written by Jerome Vouillon 
    <Jerome.Vouillon@pps.univ-paris-diderot.fr>

## Author

Christian Lindig <lindig@gmail.com>

