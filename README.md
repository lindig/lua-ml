
# Lua-ML - an embeddable Lua 2.5 interpreter implemented in OCaml

Lua-ML is an implementation of the [Lua](http://www.lua.org) 2.5 programming
language in [Objective Caml](http://caml.inria.fr). Just like the original
Lua implementation in C, it is designed for being embedded into applications
as an extension language. In this case, for applications implemented in
Objective Caml.

Lua is designed to extend applications written in C and its API is tailored to
do that, using a stack-based design to pass values between C and Lua. Lua-ML
is specifically tailored to extend applications implemented in Objective Caml
and employs an interface that makes passing values between the two worlds
seamless.

## The Case for Extension Languages

Why would you want to use an extension language for your application? Highly
configurable software like a web server, text editor, compiler, game or test
generator has probably too many configurations as that these could be handled
by command-line options alone. This leads to the addition of configuration
files which typically permit to assign strings, numbers and booleans to names.
As the project matures, more flexibility is needed, for example some kind of
name space to manage the ever increasing number of options. At this point most
the design of most configuration files starts to break as it was never
designed for this.

An extension language provides much more expressive power that you might not
need initially: assigning values to names is syntactically as easy as in any
other format, yet it provides a vastly superior upgrade path. Used to its full
potential, a language like Lua permits users to extend an application with new
functionality that would be never possible in a traditional configuration file
as such a file has no notion of execution.

In summary: an extension language is a good choice for any application that
needs to be highly configurable by the user and potentially needs also to be
extensible by the user. Rather than designing your own, Lua provides a well
designed language for this purpose.

To make an application truly extensible it needs to be designed for that. This
means that the most important data types and functions are accessible from
the Lua side such that they can be manipulated by the user. How to do this
best is currently beyond the scope of this documentation. However, here are two examples that use an earlier version of Lua-ML:

* [Quesy Test Code Generator](http://code.google.com/p/quest-tester/)
* [C-- Compiler](http://www.cminusminus.org/)

## History and Raison d'être

Lua-ML was developed as part of the [C-- compiler](http://www.cminusminus.orf)
project and is part of its source code. The C-- compiler uses 
an elaborate build process which made it difficult to build just the Lua-ML
library and a sample applications. This is an attempt to untangle Lua-ML from
its C-- legacy to make it more easily available.

Lua-ML is originally implemented as a literate program with the
[Noweb](http://www.cs.tufts.edu/~nr/noweb/) literate programming system. This
distribution uses a simpler literate programming system
[Lipsum](https://github.com/lindig/lipsum.git) that is part of this
distribution.

## Authors

The Lua-ML interpreter was written by Norman Ramsey <nr@cs.tufts.edu>. It was
originally part of the C-- project at http://www.cminusminus.org/ and brought
to GitHub by Christian Lindig <lindig@gmail.com> who also work on the C--
project 2000 to 2002.

## Copyright

The source code for the Lua-ML interpreter is in the public domain except
where noted otherwise.
