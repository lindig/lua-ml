module type VALUE = sig
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
    val find   : table -> key:value -> value
    val bind   : table -> key:value -> data:value -> unit
    val of_list : (string * value) list -> table
    val next : value Luahash.t -> value -> (value * value)
    val first : value Luahash.t -> value * value
  end
  exception Projection of value * string
  val projection : value -> string -> 'a
  type ('a, 'b, 'c) ep = ('a, 'b, 'c) Luavalue.ep
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
  val ( <@  ) : 'a map -> ('a -> 'b) -> 'b map   (* apply continuation after project *)
end
module type USERDATA = sig
  type 'a t                             (* type parameter will be Lua value *)
  val tname : string  (* name of this type, for projection errors *)
  val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val to_string : ('a -> string) -> 'a t -> string
end
module type AST = Luaast.S
module Parser : sig
  module type S =
    sig
      type chunk
      val chunks : (Lexing.lexbuf  -> Luaparser_tokens.token) -> Lexing.lexbuf -> chunk list
    end
  module type MAKER = functor (Ast : AST) -> S with type chunk = Ast.chunk
  module MakeStandard : MAKER
end
module Lib : sig
  module type USERTYPE = sig
  type 'a t                             (* type parameter will be Lua value *)
  val tname : string  (* name of this type, for projection errors *)
  val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val to_string : ('a -> string) -> 'a t -> string
end
module type TYPEVIEW = sig
  type 'a combined
  type 'a t  (* the individual type of which this is a view *)
  val makemap : ('a combined, 'b, 'b) Luavalue.ep -> ('b -> string -> 'a t)
                -> ('a t, 'b, 'b) Luavalue.ep
end
module type COMBINED_CORE = sig
  type 'a also_t
  module type VIEW = TYPEVIEW with type 'a combined = 'a also_t
  module TV1  : VIEW
  module TV2  : VIEW
  module TV3  : VIEW
  module TV4  : VIEW
  module TV5  : VIEW
  module TV6  : VIEW
  module TV7  : VIEW
  module TV8  : VIEW
  module TV9  : VIEW
  module TV10 : VIEW
end
module type COMBINED_VIEWS = sig
  type 'a t
  include COMBINED_CORE with type 'a also_t = 'a t
end
module type COMBINED_TYPE = sig
  include USERTYPE
  include COMBINED_CORE with type 'a also_t = 'a t
end
module type CORE = sig
  module V : Luavalue.S
  val error : string -> 'a  (* error fallback *)
  val getglobal : V.state -> V.value -> V.value
  val fallback : string -> V.state -> V.value list -> V.value list
    (* invoke named fallback on given state and arguments *)
  val setfallback : V.state -> string -> V.value -> V.value
    (* sets fallback, returns previous one *)
  val apply : V.value -> V.state -> V.value list -> V.value list

  val register_globals :           (string * V.value) list -> V.state -> unit
    (* registers values as named global variables *)
  val register_module  : string -> (string * V.value) list -> V.state -> unit
    (* register_module t l inserts members of l into global table t, 
       creating t if needed *)
end
module type BARECODE = 
  functor (C : CORE) -> sig
    val init : C.V.state -> unit
  end
module type USERCODE = sig
  type 'a userdata'  (* the userdata' tycon of the core on which lib depends *)
  module M : functor (C : CORE with type 'a V.userdata' = 'a userdata') -> sig
    val init : C.V.state -> unit
  end
end
module WithType
  (T : USERTYPE) (L : BARECODE) : USERCODE with type 'a userdata' = 'a T.t
module Combine : sig
  module T10 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE)
             (T5 : USERTYPE) (T6 : USERTYPE) (T7 : USERTYPE) (T8 : USERTYPE)
             (T9 : USERTYPE) (T10 : USERTYPE)
   : COMBINED_TYPE with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t
                   with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t
                   with type 'a TV5.t = 'a T5.t with type 'a TV6.t = 'a T6.t
                   with type 'a TV7.t = 'a T7.t with type 'a TV8.t = 'a T8.t
                   with type 'a TV9.t = 'a T9.t with type 'a TV10.t = 'a T10.t
  module T1 (T1 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t
module T2 (T1 : USERTYPE) (T2 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t
module T3 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t
module T4 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t
module T5 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE) (T5 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t with type 'a TV5.t = 'a T5.t
module T6 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE) (T5 : USERTYPE) (T6 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t with type 'a TV5.t = 'a T5.t with type 'a TV6.t = 'a T6.t
module T7 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE) (T5 : USERTYPE) (T6 : USERTYPE) (T7 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t with type 'a TV5.t = 'a T5.t with type 'a TV6.t = 'a T6.t with type 'a TV7.t = 'a T7.t
module T8 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE) (T5 : USERTYPE) (T6 : USERTYPE) (T7 : USERTYPE) (T8 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t with type 'a TV5.t = 'a T5.t with type 'a TV6.t = 'a T6.t with type 'a TV7.t = 'a T7.t with type 'a TV8.t = 'a T8.t
module T9 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE) (T5 : USERTYPE) (T6 : USERTYPE) (T7 : USERTYPE) (T8 : USERTYPE) (T9 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t with type 'a TV5.t = 'a T5.t with type 'a TV6.t = 'a T6.t with type 'a TV7.t = 'a T7.t with type 'a TV8.t = 'a T8.t with type 'a TV9.t = 'a T9.t

  module C10 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C5 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C6 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C7 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C8 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C9 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C10 : USERCODE with type 'a userdata' = 'a C1.userdata') :
    USERCODE with type 'a userdata' = 'a C1.userdata'
    module C1 (C1 : USERCODE)
  : USERCODE with type 'a userdata' = 'a C1.userdata'
  module C2 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'
  module C3 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'
  module C4 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'
  module C5 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C5 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'
  module C6 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C5 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C6 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'
  module C7 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C5 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C6 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C7 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'
  module C8 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C5 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C6 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C7 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C8 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'
  module C9 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C5 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C6 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C7 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C8 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C9 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'

end
module Lift (T : COMBINED_TYPE) (View : TYPEVIEW with type 'a t = 'a T.t) :
  COMBINED_VIEWS with type 'a t = 'a View.combined
     with type 'a TV1.t = 'a T.TV1.t
     with type 'a TV2.t = 'a T.TV2.t
     with type 'a TV3.t = 'a T.TV3.t
     with type 'a TV4.t = 'a T.TV4.t
     with type 'a TV5.t = 'a T.TV5.t
     with type 'a TV6.t = 'a T.TV6.t
     with type 'a TV7.t = 'a T.TV7.t
     with type 'a TV8.t = 'a T.TV8.t
     with type 'a TV9.t = 'a T.TV9.t
     with type 'a TV10.t = 'a T.TV10.t

end
module type EVALUATOR = sig
  module Value : VALUE
  module Ast   : AST with module Value = Value
  type state = Value.state
  type value = Value.value
  exception Error of string
  type compiled = unit -> value list
  val compile : srcdbg:(Luasrcmap.map * bool) -> Ast.chunk list -> state -> compiled
  type startup_code = (string -> unit) -> unit
  val pre_mk  : unit -> state * startup_code (* produce a fresh, initialized state *)
  val error   : string -> 'a    (* error fallback *)

  val getglobal : state -> value -> value
    (* get the named global variable *)
  val fallback : string -> state -> value list -> value list
    (* invoke named fallback on given state and arguments *)
  val with_stack  : Value.srcloc -> state -> ('a -> 'b) -> 'a -> 'b
    (* evaluate function with given srcloc on activation stack *)

  val setfallback : state -> string -> value -> value
    (* sets fallback, returns previous one *)
  val register_globals :           (string * value) list -> state -> unit
    (* registers values as named global variables *)
  val register_module  : string -> (string * value) list -> state -> unit
    (* register_module t l inserts members of l into global table t, 
       creating t if needed *)
end

module MakeEval
    (T : Lib.USERTYPE)
    (L : Lib.USERCODE with type 'a userdata' = 'a T.t)
    : EVALUATOR with type 'a Value.userdata' = 'a T.t
module Empty : sig
  module Type : Lib.USERTYPE
  module Library : Lib.USERCODE
end
module type INTERP = sig
  include EVALUATOR
  module Parser : Luaparser.S with type chunk = Ast.chunk
  val do_lexbuf : sourcename:string -> state -> Lexing.lexbuf -> value list
  val dostring  : ?file:string -> state -> string -> value list
  val dofile    : state -> string -> value list
  val mk        : unit -> state
end
module MakeInterp (MakeParser : Parser.MAKER) (I : EVALUATOR)
    : INTERP with module Value = I.Value and module Ast = I.Ast
module Run (I : INTERP) : sig end  (* runs interpreter on Sys.argv *)
