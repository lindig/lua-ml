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
  val setfallback : V.state -> string -> V.value -> V.value
    (* sets fallback, returns previous one *)
  val apply : V.value -> V.state -> V.value list -> V.value list
  val register_globals :           (string * V.value) list -> V.state -> unit
  val register_module  : string -> (string * V.value) list -> V.state -> unit
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

module WithType (T : USERTYPE) (L : BARECODE) : USERCODE with type 'a userdata' = 'a T.t
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
    (C10: USERCODE with type 'a userdata' = 'a C1.userdata') :
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
     and type 'a TV1.t = 'a T.TV1.t
     and type 'a TV2.t = 'a T.TV2.t
     and type 'a TV3.t = 'a T.TV3.t
     and type 'a TV4.t = 'a T.TV4.t
     and type 'a TV5.t = 'a T.TV5.t
     and type 'a TV6.t = 'a T.TV6.t
     and type 'a TV7.t = 'a T.TV7.t
     and type 'a TV8.t = 'a T.TV8.t
     and type 'a TV9.t = 'a T.TV9.t
     and type 'a TV10.t = 'a T.TV10.t
module Empty : sig
  module Type : COMBINED_TYPE
  module Library : USERCODE with type 'a userdata' = 'a Type.t
end
