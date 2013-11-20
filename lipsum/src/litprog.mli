(** This module implements a representation for a literate program which
    is a sequence of documentation and named code chunks. *)

exception NoSuchChunk of string (** a requested code chunk doesn't exist *)
exception Cycle of string       (** code chunks refers to itself *)

type code = 
    | Str of Tangle.position * string 
    | Ref of string
(** Code is a string or a reference to another chunk *)

type chunk = 
    | Doc of string 
    | Code of string * code list
(** A chunk is a document chunk or a named code chunk. The code 
    is modeled as a list of code and references *)


type doc = chunk list
type t 
(** A document is a list of chunks. Such a document can be turned into an
    abstract representation of type t *)

val make : chunk list -> t
val doc : t -> chunk list
(** make and doc convert between the two representations but only the
    abstract representation can be used for querying it *)

val code_chunks : t -> string list
(** Return the names of all code chunks. *)

val code_roots : t -> string list 
(** Code chunks to referred to by others are roots. Code_roots 
    returns all root code chunks *)

val tangle : t -> Tangle.t -> out_channel -> string -> unit 
(** Extract a named code chunk from a literate program by recursively
    resolving all references and writing the result to an output
    channel. The formatting is controlled by the Tangle.t value. The
    function may raise exceptions in case of undefined references or cyclic 
    references *)

val unknown_references : t -> string list
(** Scan t and find all undefined references to code chunks *)

(**/**)
val print : t -> unit (** For debugging: emit t to stdout *)
