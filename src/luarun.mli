module type INTERP = sig
  module Value : Luavalue.S
  type value = Value.value
  type state = Value.state
  val mk        : unit -> state
  val dostring  : state -> string -> value list
  val dofile    : state -> string -> value list
end
module Make (I : INTERP) : sig end
