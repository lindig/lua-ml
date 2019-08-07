type ('a, 'b, 'c) ep = { embed : 'a -> 'b; project : 'b -> 'a; is : 'c -> bool }
type ('a, 'b, 'c) synonym_for_ep = ('a, 'b, 'c) ep 
  = { embed : 'a -> 'b; project : 'b -> 'a; is : 'c -> bool }
module type S = sig
  type 'a userdata'
  type srcloc
  type initstate

  module rec LuaValueBase : sig
    type value =
      Nil
    | Number   of float
    | String   of string
    | Function of srcloc * func
    | Userdata of userdata
    | Table    of table
    and func  = value list -> value list
    and table = value Luahash.t
    and userdata  = value userdata'
    val eq : value -> value -> bool
  end and  LuahashKey : sig
    type t
    val hash : t -> int
    val equal : t -> t -> bool
  end
  and Luahash : Hashtbl.S with type key = LuaValueBase.value

  type value = LuaValueBase.value
  and func  = value list -> value list
  and table = value Luahash.t
  and userdata  = value userdata'
 

  type state = { globals : table
              ; fallbacks : (string, value) Hashtbl.t
              ; mutable callstack : activation list
              ; mutable currentloc : Luasrcmap.location option (* supersedes top of stack *)
              ; startup : initstate
              }
  and activation = srcloc * Luasrcmap.location option

  val caml_func : func -> value (* each result unique *)
  val lua_func  : file:string -> linedefined:int -> func -> value
  val srcloc    : file:string -> linedefined:int -> srcloc (* must NOT be reused *)
  val eq        : value -> value -> bool
  val to_string : value -> string
  val activation_strings : state -> activation -> string list
  type objname = Fallback of string | Global of string | Element of string * value
  val objname : state -> value -> objname option
     (* 'fallback', 'global', or 'element', name *)

  val state : unit -> state (* empty state, without even fallbacks *)
  val at_init : state -> string list -> unit  (* run code at startup time *)
  val initcode : state -> (string -> unit) -> unit (* for the implementation only *)
  module Table : sig
    val create : int -> table
    val find   : table -> key:value -> value   (* returns Nil if not found *)
    val bind   : table -> key:value -> data:value -> unit
    val of_list : (string * value) list -> table
    val next : value Luahash.t -> value -> (value * value)
    val first : value Luahash.t -> value * value
  end
  exception Projection of value * string
  val projection : value -> string -> 'a
  type ('a, 'b, 'c) ep = ('a, 'b, 'c) synonym_for_ep 
    = { embed : 'a -> 'b; project : 'b -> 'a; is : 'c -> bool }
  type 'a map  = ('a, value, value) ep
  type 'a mapf  (* used to build function maps that curry/uncurry *)
  val float    : float  map
  val int      : int    map
  val bool     : bool   map
  val string   : string map
  val userdata : userdata map
  val unit     : unit   map
  val option : 'a map -> 'a option map
  val default : 'a -> 'a map -> 'a map
  val list    : 'a map -> 'a list map   (* does not project nil *)
  val optlist : 'a map -> 'a list map   (* projects nil to empty list *)
  val value  : value map
  val table  : table map
  val record : 'a map -> (string * 'a) list map
  val enum   : string -> (string * 'a) list -> 'a map
  val ( -->  ) : 'a map  -> 'b map  -> ('a -> 'b) map
  val ( **-> ) : 'a map  -> 'b mapf -> ('a -> 'b) mapf
  val result   : 'a map  -> 'a mapf
  val resultvs : value list mapf                   (* functions returning value lists*)
  val resultpair:'a map  -> 'b map  -> ('a * 'b)       mapf
  val dots_arrow:'a map  -> 'b map  -> ('a list -> 'b) mapf     (* varargs functions *)
  val results  : ('a -> value list) -> (value list -> 'a) -> 'a mapf  
                                    (* 'a represents multiple results (general case) *)
  val func     : 'a mapf -> 'a map                 (* function *)
  val closure  : 'a mapf -> 'a map                 (* function or table+apply method *)
  val efunc    : 'a mapf -> 'a -> value            (* efunc f = (closure f).embed *)
  type alt                              (* an alternative *)
  val alt    : 'a mapf -> 'a -> alt     (* create an alternative *)
  val choose : alt list -> value        (* dispatch on type/number of args *)
  val ( <|> ) : 'a map -> 'a map -> 'a map 
  val ( <@ ) : 'a map -> ('a -> 'b) -> 'b map   (* apply continuation after project *)
end
module type USERDATA = sig
  type 'a t                             (* type parameter will be Lua value *)
  val tname : string  (* name of this type, for projection errors *)
  val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val to_string : ('a -> string) -> 'a t -> string
end

module Make (U : USERDATA) : S with type 'a userdata'  = 'a U.t 
