type pos            = int
type rgn            = pos * pos
val null            : rgn
type location       = string    (* file   *)
                    * int       (* line   *)
                    * int       (* column *)
type map
val mk:             unit -> map (* empty map *)
val sync :          map -> pos -> location -> unit
val nl :            map -> pos -> unit
val last :          map -> location
val location :      map -> pos -> location
val dump:           map -> unit
type point          = map * pos
type region         = map * rgn
module Str:
sig
    val point       : point  -> string
    val region      : region -> string
end
