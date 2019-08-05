module type S = sig
  module Value : Luavalue.S
  module Ast   : Luaast.S with module Value = Value
  type state = Value.state
  type value = Value.value
  exception Error of string
  type compiled = unit -> value list
  val compile : srcdbg:(Luasrcmap.map * bool) -> Ast.chunk list -> state -> compiled
  type startup_code = (string -> unit) -> unit
  val pre_mk  : unit -> state * startup_code (* produce a fresh, initialized state *)
  val error   : string -> 'a   (* error fallback *)

  val getglobal : state -> value -> value
    (* get the named global variable *)
  val fallback  : string -> state -> value list -> value list
    (* invoke named fallback on given state and arguments *)
  val with_stack  : Value.srcloc -> state -> ('a -> 'b) -> 'a -> 'b
    (* evaluates function with given srcloc on activation stack *)

  val setfallback : state -> string -> value -> value
    (* sets fallback, returns previous one *)
  val register_globals :           (string * value) list -> state -> unit
    (* registers values as named global variables *)
  val register_module  : string -> (string * value) list -> state -> unit
    (* register_module t l inserts members of l into global table t, 
       creating t if needed *)
end

module Make (T : Luavalue.USERDATA)
            (L : Lualib.USERCODE with type 'a userdata' = 'a T.t) :
    S with type 'a Value.userdata'  = 'a T.t
