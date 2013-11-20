(** Weaving is called the process of formatting a literate program
    as documentation (in contrast to formatting its embedded code 
    for compilation). This module provides formatters for weaving 
    a literate program. *)

exception NoSuchFormat of string (** requested format doesn't exist *)

type t = out_channel -> Litprog.doc -> unit 
(* emit doc to channel *)

val lookup  : string -> t 
(** lookup named format *)

val formats : string list 
(** names of available formats *)

