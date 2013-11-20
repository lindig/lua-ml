(** This module provides formats for emitting code from a literate program.
    This process is called traditionally called tangling. Typically a
    format will want to emit file positions that refer back to the literate
    program such that compiler error messages can be traced back to the
    original source, i.e., the literate program. 

    The module maintains a directory of named formatters which can be
    looked up. Currently there is no way to add new formatters
    programatically. *)

exception NoSuchFormat of string (** raised if lookup fails *)

type position = 
    { file :    string  (** original source file *)
    ; line :    int     (** original line in source file *)
    ; column :  int     (** original column in source file *)
    ; offset:   int     (** byte offset from beginning of source file *)
    }

type t = out_channel -> position -> string -> unit
(** emits code originating from position to output channel *)

(** available formats *)
val lookup  : string -> t (* NoSuchFormat *)
val formats : string list

val plain   : t
(** a very basic formatter *)
