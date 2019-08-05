(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* modified by Norman Ramsey to provide threading via a `next' function *)

(* $Id: luahash.nw,v 1.8 2004-08-03 22:13:33 nr Exp $ *)

(* Hash tables are hashed association tables, with in-place modification. *)

(*** Generic interface *)

type ('a, 'b) t
        (* The type of hash tables from type ['a] to type ['b]. *)

val create : ('a -> 'a -> bool) -> int -> ('a,'b) t
        (* [Luahash.create eq n] creates a new, empty hash table, with
           initial size [n].  Function eq is used to compare equality of keys
           For best results, [n] should be on the
           order of the expected number of elements that will be in
           the table.  The table grows as needed, so [n] is just an
           initial guess. *)

val population : ('a, 'b) t -> int
        (* number of key-value pairs in a table (as distinct from its size) *)

val clear : ('a, 'b) t -> unit
        (* Empty a hash table. *)

val find : ('a, 'b) t -> 'a -> 'b
        (* [Luahash.find tbl x] returns the current binding of [x] in [tbl],
           or raises [Not_found] if no such binding exists. *)

val find_all : ('a, 'b) t -> 'a -> 'b list
        (* [Luahash.find_all tbl x] returns the list of all data
           associated with [x] in [tbl].
           The current binding is returned first, then the previous
           bindings, in reverse order of introduction in the table. *)

val mem :  ('a, 'b) t -> 'a -> bool
        (* [Luahash.mem tbl x] checks if [x] is bound in [tbl]. *)

val remove : ('a, 'b) t -> 'a -> unit
        (* [Luahash.remove tbl x] removes the current binding of [x] in [tbl],
           restoring the previous binding if it exists.
           It does nothing if [x] is not bound in [tbl]. *)

val replace : ('a, 'b) t -> key:'a -> data:'b -> unit
        (* [Luahash.replace tbl x y] replaces the current binding of [x]
           in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
           a binding of [x] to [y] is added to [tbl].
           This is functionally equivalent to [Luahash.remove tbl x]
           followed by [Luahash.add tbl x y], except that Luahash has no [add]. *)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
        (* [Luahash.iter f tbl] applies [f] to all bindings in table [tbl].
           [f] receives the key as first argument, and the associated value
           as second argument. The order in which the bindings are passed to
           [f] is unspecified. Each binding is presented exactly once
           to [f]. *)

val first : ('a, 'b) t -> 'a * 'b
val next  : ('a, 'b) t -> 'a -> 'a * 'b
        (* Used to iterate over the contents of the table, Lua style.
           Raises Not_found when the contents are exhausted *)


(*** The polymorphic hash primitive *)

val hash : 'a -> int
        (* [Luahash.hash x] associates a positive integer to any value of
           any type. It is guaranteed that
                if [x = y], then [hash x = hash y]. 
           Moreover, [hash] always terminates, even on cyclic
           structures. *)

