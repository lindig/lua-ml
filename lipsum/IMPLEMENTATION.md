
# The Lipsum Implementation

This document provides an overview for the implementation of Lipsum. The
heart of the implementation is module `Litprog` that captures in type `t` a
literate program as it is read from a file and from which chunks are
expanded.

The program is driven by the main module `Lipsum` which evaluates the
command line, handles files, and error messages.

## Parser and Abstract Datatype `Lipsum.t`

Lipsum employs a scanner and parser to create an abstract syntax for an
input file which then is turned by `Lipsum.make` into a `Litprog.t` value.
The parser captures the high-level structure of an input file: a sequence
of code and documentation chunks. 


        litprog : chunks EOF
                | STR chunks EOF

        chunks  : chunks chunk
                |

        chunk   : code
                | doc

        doc     : AT STR

        code    : DEF body

        body    : body STR
                | body REF
                |

Scanning chops the input file into a sequence of tokens like `DEF`, `STR`
or `AT` which are consumed by the parser. These tokens are defined in the
parser but created by the scanner.

A document is represented both as a sequence of chunks and additionally as
a hash table that maps names (of chunks) to code and references to other
chunks.  While such a document is built by `Litprog.make` it is not checked
that all referenced chunks indeed exist and neither is the absence of
cycles checked.

## Scanning

Creating a sequence of tokens in the scanner is the trickiest part of the
implementation. Typically each token is defined by a regular expression
that captures its content. The is easy for tokens that represent a `<<chunk
name>` in angle brackets. It is, however, difficult to capture the content
that makes up the chunks because it is delimited by syntax like `<<chunk
name>` or `@` but there is no pattern to capture the content itself. The
scanner therefore reads an arbitrary string and the following delimiter but
must return these as two tokens. It does this using the `return` function.
The `token` function in the scanner stores one value and returns it the
next time it is called from the parser.

## Tangling 

Module `Tangle` implements emitting a named chunk by recursively expanding
all chunks that are referenced within it. It must detect cycles to avoid an
infinite loop during chunk expansion.
