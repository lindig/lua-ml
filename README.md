# Lua-ML

Lua-ML is an implementation of the [Lua](https://www.lua.org) 2.5 programming
language written in [OCaml](https://ocaml.org) and designed for extending
OCaml programs.

Highly configurable programs like games, text editors, or test generators
require extensibility since it's not possible to include functionality for every possible
use case.

For many such programs, embedding a small general purpose language can be a better option than implementing their own DSL, and definitely better than creating an accidentally Turing-complete configuration file format.

## Overview

Lua-ML is **not** a set of bindings for the PUC-Rio implementation written in C.
It's a complete implementation of a Lua 2.5 interpreter and runtime in OCaml.

This has a number of advantages:

### Modular runtime library

The Lua library is not a single module, but a set of OCaml modules and functors.
That allows you to exclude some modules from the runtime, add your own modules, or even completely replace the default standard library with your own modules.

### Type and memory safety

Registering functions and passing (embedding) values to Lua is as type safe as
everything else in OCaml, so errors in interfacing with Lua are caught at compile time.

Since there is no unmanaged code involved, Lua code cannot crash its host program
or access memory it's not supposed to access (assuming there are no memory safety bugs
in the OCaml runtime of course).

It *should* be safe to use it even for untrusted scripts, if you don't include
modules like `Luaiolib` into the runtime. Of course, you still should exercise
extreme caution if you actually choose to run untrusted scripts.

### Resistance to bit rot

Bindings usually require a specific version of the PUC-Rio implementation (e.g. 5.1)
and may stop working with newer versions, which makes software harder to build
and introduces new maintenance costs.

A pure OCaml implementation doesn't have that problem. The fact that this project
was revived with minimal effort after more than a decade of dormancy is telling.

### Disadvantages

* Incompatible with existing Lua libraries.
* Impelements, at this time, only antiquated Lua 2.5.

## Project status

Lua-ML is usable and works quite well, but there's still room for improvement,
especially in error reporting.

It doesn't make an API stability promise _yet_, which is why the versions are
0.9.x. I do promise to keep breaking changes to the minimum,
but there's a chance they will be necessary.

One problem with backporting improvements from post-2.5 Lua specifications
is that PUC-Rio Lua itself made a bunch of incompatible change on the way,
so future direction requires a discussion with the user community.

## Installation

```
opam install lua-ml
```

## Usage

There isn't much documentation now. Any help is welcome!

For an example application, take a look at `example/luaclient.ml`. It shows how to provide
a custom type (2-tuple) as userdata, register your own module, and run Lua code.

```sh
dune exec example/luaclient.exe
```

You can also read the original papers by Norman Ramsey:
* [Embedding an Interpreted Language Using Higher-Order Functions and Types](https://www.cs.tufts.edu/~nr/pubs/embedj-abstract.html)
* [ML Module Mania: A Type-Safe,
Separately Compiled, Extensible Interpreter](https://www.cs.tufts.edu/~nr/pubs/maniaws-abstract.html)

Lua-ML once was a literate program and a snapshot of the last pre-revival NoWeb version
is kept in `docs/noweb`. There's no easy way to make a PDF out of it, but reading the NoWeb
source can give a good insight into the internals.

A real life example of a project using Lua-ML is [soupault](https://github.com/dmbaturin/soupault),
a native but extensible static site generator/HTML processor.
It exposes the element tree of the page as an abstract type (userdata) and makes HTML manipulation
functions from [lambdasoup](https://github.com/aantron/lambdasoup) available to plugins.

Historical examples that used older Lua-ML versions include:
* [Quest Test Code Generator](http://code.google.com/p/quest-tester/)
* [C-- Compiler](http://web.archive.org/web/20150501125322/http://www.cminusminus.org/)


## History and Authors

Lua-ML was developed as part of the [C-- compiler](http://web.archive.org/web/20150501125322/http://www.cminusminus.org/)
project developed by [Norman Ramsey](https://www.cs.tufts.edu/~nr/) and was part of its source code.
The complicated build process of C-- made it hard to build and use in other programs.

Then Christian Lindig, who also worked on C-- from 2000 to 2002, extracted it from C-- and reworked it into a standalone library to preserve it and make easier to use.

In 2018-2019, effort of Gabriel Radanne and Daniil Baturin allowed Lua-ML to build with modern OCaml and become an OPAM package.

The current maintainer is Daniil Baturin <daniil+luaml@baturin.org>.

## Copyright

Lua-ML is distributed under the two-clause BSD license.
See the LICENSE file for details.
