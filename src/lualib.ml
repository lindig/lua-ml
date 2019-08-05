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

module Unused = struct
  module Type : USERTYPE = struct
    type 'a t = unit
    let tname = "unused type"
    let eq _ x y = true
    let to_string _ _ = "<this can't happen -- value of unused type>"
  end (* Type *)

  module Bare =
      functor (C : CORE) -> struct
        let init g = ()
      end (*Unused.Bare*)

  module Typeful (L : USERCODE) =
    struct
      type 'a userdata' = 'a L.userdata'
      module M (C : CORE with type 'a V.userdata' = 'a userdata') = struct
        let init g = ()
      end (*M*)
    end (*Unused.Typeful*)
end

module Combine = struct
    module T10 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE)
             (T5 : USERTYPE) (T6 : USERTYPE) (T7 : USERTYPE) (T8 : USERTYPE) 
             (T9 : USERTYPE) (T10 : USERTYPE)
   : COMBINED_TYPE with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t
                   with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t
                   with type 'a TV5.t = 'a T5.t with type 'a TV6.t = 'a T6.t
                   with type 'a TV7.t = 'a T7.t with type 'a TV8.t = 'a T8.t
                   with type 'a TV9.t = 'a T9.t with type 'a TV10.t = 'a T10.t =
  struct
    type 'a t =
      | T1 of 'a T1.t
      | T2 of 'a T2.t
      | T3 of 'a T3.t
      | T4 of 'a T4.t
      | T5 of 'a T5.t
      | T6 of 'a T6.t
      | T7 of 'a T7.t
      | T8 of 'a T8.t
      | T9 of 'a T9.t
      | T10 of 'a T10.t
    type 'a also_t = 'a t
    let allnames = [T1.tname; T2.tname; T3.tname; T4.tname; T5.tname;
                    T6.tname; T7.tname; T8.tname; T9.tname; T10.tname]
    let tname = String.concat " or " (List.filter ((<>) Unused.Type.tname) allnames)
    let tname = match tname with "" -> Unused.Type.tname | n -> n

    let eq eqvs x y = match x, y with
    | T1 x, T1 y -> T1.eq eqvs x y
    | T2 x, T2 y -> T2.eq eqvs x y
    | T3 x, T3 y -> T3.eq eqvs x y
    | T4 x, T4 y -> T4.eq eqvs x y
    | T5 x, T5 y -> T5.eq eqvs x y
    | T6 x, T6 y -> T6.eq eqvs x y
    | T7 x, T7 y -> T7.eq eqvs x y
    | T8 x, T8 y -> T8.eq eqvs x y
    | T9 x, T9 y -> T9.eq eqvs x y
    | T10 x, T10 y -> T10.eq eqvs x y
    | _, _ -> false

    let to_string vs x = match x with
    | T1 x -> T1.to_string vs x
    | T2 x -> T2.to_string vs x
    | T3 x -> T3.to_string vs x
    | T4 x -> T4.to_string vs x
    | T5 x -> T5.to_string vs x
    | T6 x -> T6.to_string vs x
    | T7 x -> T7.to_string vs x
    | T8 x -> T8.to_string vs x
    | T9 x -> T9.to_string vs x
    | T10 x -> T10.to_string vs x
        
  module type VIEW = TYPEVIEW with type 'a combined = 'a t
  module V = Luavalue
  module TV1 = struct
    type 'a combined = 'a also_t
    type 'a t = 'a T1.t
    let makemap (upper : ('a combined, 'b, 'b) V.ep) fail =
      { V.embed   = (fun x -> upper.V.embed (T1 x))
      ; V.project = (fun x -> match upper.V.project x with
                    | T1 x -> x
                    | _ -> fail x T1.tname)
      ; V.is      = (fun x -> upper.V.is x &&
                              match upper.V.project x with T1 x -> true | _ -> false)
      } 
  end
    module TV2 = struct
    type 'a combined = 'a also_t
    type 'a t = 'a T2.t
    let makemap upper fail =
      { V.embed   = (fun x -> upper.V.embed (T2 x))
      ; V.project = (fun x -> match upper.V.project x with
                    | T2 x -> x
                    | _ -> fail x T2.tname)
      ; V.is      = (fun x -> upper.V.is x &&
                              match upper.V.project x with T2 x -> true | _ -> false)
      } 
  end

  module TV3 = struct
    type 'a combined = 'a also_t
    type 'a t = 'a T3.t
    let makemap upper fail =
      { V.embed   = (fun x -> upper.V.embed (T3 x))
      ; V.project = (fun x -> match upper.V.project x with
                    | T3 x -> x
                    | _ -> fail x T3.tname)
      ; V.is      = (fun x -> upper.V.is x &&
                              match upper.V.project x with T3 x -> true | _ -> false)
      } 
  end

  module TV4 = struct
    type 'a combined = 'a also_t
    type 'a t = 'a T4.t
    let makemap upper fail =
      { V.embed   = (fun x -> upper.V.embed (T4 x))
      ; V.project = (fun x -> match upper.V.project x with
                    | T4 x -> x
                    | _ -> fail x T4.tname)
      ; V.is      = (fun x -> upper.V.is x &&
                              match upper.V.project x with T4 x -> true | _ -> false)
      } 
  end

  module TV5 = struct
    type 'a combined = 'a also_t
    type 'a t = 'a T5.t
    let makemap upper fail =
      { V.embed   = (fun x -> upper.V.embed (T5 x))
      ; V.project = (fun x -> match upper.V.project x with
                    | T5 x -> x
                    | _ -> fail x T5.tname)
      ; V.is      = (fun x -> upper.V.is x &&
                              match upper.V.project x with T5 x -> true | _ -> false)
      } 
  end

  module TV6 = struct
    type 'a combined = 'a also_t
    type 'a t = 'a T6.t
    let makemap upper fail =
      { V.embed   = (fun x -> upper.V.embed (T6 x))
      ; V.project = (fun x -> match upper.V.project x with
                    | T6 x -> x
                    | _ -> fail x T6.tname)
      ; V.is      = (fun x -> upper.V.is x &&
                              match upper.V.project x with T6 x -> true | _ -> false)
      } 
  end

  module TV7 = struct
    type 'a combined = 'a also_t
    type 'a t = 'a T7.t
    let makemap upper fail =
      { V.embed   = (fun x -> upper.V.embed (T7 x))
      ; V.project = (fun x -> match upper.V.project x with
                    | T7 x -> x
                    | _ -> fail x T7.tname)
      ; V.is      = (fun x -> upper.V.is x &&
                              match upper.V.project x with T7 x -> true | _ -> false)
      } 
  end

  module TV8 = struct
    type 'a combined = 'a also_t
    type 'a t = 'a T8.t
    let makemap upper fail =
      { V.embed   = (fun x -> upper.V.embed (T8 x))
      ; V.project = (fun x -> match upper.V.project x with
                    | T8 x -> x
                    | _ -> fail x T8.tname)
      ; V.is      = (fun x -> upper.V.is x &&
                              match upper.V.project x with T8 x -> true | _ -> false)
      } 
  end

  module TV9 = struct
    type 'a combined = 'a also_t
    type 'a t = 'a T9.t
    let makemap upper fail =
      { V.embed   = (fun x -> upper.V.embed (T9 x))
      ; V.project = (fun x -> match upper.V.project x with
                    | T9 x -> x
                    | _ -> fail x T9.tname)
      ; V.is      = (fun x -> upper.V.is x &&
                              match upper.V.project x with T9 x -> true | _ -> false)
      } 
  end

  module TV10 = struct
    type 'a combined = 'a also_t
    type 'a t = 'a T10.t
    let makemap upper fail =
      { V.embed   = (fun x -> upper.V.embed (T10 x))
      ; V.project = (fun x -> match upper.V.project x with
                    | T10 x -> x
                    | _ -> fail x T10.tname)
      ; V.is      = (fun x -> upper.V.is x &&
                              match upper.V.project x with T10 x -> true | _ -> false)
      } 
  end

  end (* Combine.T10 *)
  module T1 (T1 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t   = 
    T10 (T1) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type)
module T2 (T1 : USERTYPE) (T2 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t   = 
    T10 (T1) (T2) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type)
module T3 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t   = 
    T10 (T1) (T2) (T3) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type)
module T4 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t   = 
    T10 (T1) (T2) (T3) (T4) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type)
module T5 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE) (T5 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t with type 'a TV5.t = 'a T5.t   = 
    T10 (T1) (T2) (T3) (T4) (T5) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type)
module T6 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE) (T5 : USERTYPE) (T6 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t with type 'a TV5.t = 'a T5.t with type 'a TV6.t = 'a T6.t   = 
    T10 (T1) (T2) (T3) (T4) (T5) (T6) (Unused.Type) (Unused.Type) (Unused.Type) (Unused.Type)
module T7 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE) (T5 : USERTYPE) (T6 : USERTYPE) (T7 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t with type 'a TV5.t = 'a T5.t with type 'a TV6.t = 'a T6.t with type 'a TV7.t = 'a T7.t   = 
    T10 (T1) (T2) (T3) (T4) (T5) (T6) (T7) (Unused.Type) (Unused.Type) (Unused.Type)
module T8 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE) (T5 : USERTYPE) (T6 : USERTYPE) (T7 : USERTYPE) (T8 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t with type 'a TV5.t = 'a T5.t with type 'a TV6.t = 'a T6.t with type 'a TV7.t = 'a T7.t with type 'a TV8.t = 'a T8.t   = 
    T10 (T1) (T2) (T3) (T4) (T5) (T6) (T7) (T8) (Unused.Type) (Unused.Type)
module T9 (T1 : USERTYPE) (T2 : USERTYPE) (T3 : USERTYPE) (T4 : USERTYPE) (T5 : USERTYPE) (T6 : USERTYPE) (T7 : USERTYPE) (T8 : USERTYPE) (T9 : USERTYPE)  : COMBINED_TYPE
 with type 'a TV1.t = 'a T1.t with type 'a TV2.t = 'a T2.t with type 'a TV3.t = 'a T3.t with type 'a TV4.t = 'a T4.t with type 'a TV5.t = 'a T5.t with type 'a TV6.t = 'a T6.t with type 'a TV7.t = 'a T7.t with type 'a TV8.t = 'a T8.t with type 'a TV9.t = 'a T9.t   = 
    T10 (T1) (T2) (T3) (T4) (T5) (T6) (T7) (T8) (T9) (Unused.Type)

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
    USERCODE with type 'a userdata' = 'a C1.userdata'   = 
  struct
    type 'a userdata' = 'a C1.userdata'
    module M  (C : CORE with type 'a V.userdata' = 'a userdata') = struct
      module M1 = C1.M(C)
      module M2 = C2.M(C)
      module M3 = C3.M(C)
      module M4 = C4.M(C)
      module M5 = C5.M(C)
      module M6 = C6.M(C)
      module M7 = C7.M(C)
      module M8 = C8.M(C)
      module M9 = C9.M(C)
      module M10 = C10.M(C)
      let init g =
        begin
          M1.init  g;
          M2.init  g;
          M3.init  g;
          M4.init  g;
          M5.init  g;
          M6.init  g;
          M7.init  g;
          M8.init  g;
          M9.init  g;
          M10.init g
        end
    end (*Combine.C10.M*)
  end (* Combine.C10*)
  module C1 (C1 : USERCODE)
  : USERCODE with type 'a userdata' = 'a C1.userdata'   = 
    C10 (C1) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1))
  module C2 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'   = 
    C10 (C1) (C2) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1))
  module C3 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'   = 
    C10 (C1) (C2) (C3) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1))
  module C4 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'   = 
    C10 (C1) (C2) (C3) (C4) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1))
  module C5 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C5 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'   = 
    C10 (C1) (C2) (C3) (C4) (C5) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1))
  module C6 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C5 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C6 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'   = 
    C10 (C1) (C2) (C3) (C4) (C5) (C6) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1))
  module C7 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C5 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C6 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C7 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'   = 
    C10 (C1) (C2) (C3) (C4) (C5) (C6) (C7) (Unused.Typeful(C1)) (Unused.Typeful(C1)) (Unused.Typeful(C1))
  module C8 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C5 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C6 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C7 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C8 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'   = 
    C10 (C1) (C2) (C3) (C4) (C5) (C6) (C7) (C8) (Unused.Typeful(C1)) (Unused.Typeful(C1))
  module C9 (C1 : USERCODE)
    (C2 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C3 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C4 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C5 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C6 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C7 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C8 : USERCODE with type 'a userdata' = 'a C1.userdata')
    (C9 : USERCODE with type 'a userdata' = 'a C1.userdata')
  : USERCODE with type 'a userdata' = 'a C1.userdata'   = 
    C10 (C1) (C2) (C3) (C4) (C5) (C6) (C7) (C8) (C9) (Unused.Typeful(C1))

end
module WithType (T : USERTYPE) (L : BARECODE) : USERCODE with type 'a userdata' = 'a T.t
=
    struct
      type 'a userdata' = 'a T.t
      module M (C : CORE with type 'a V.userdata' = 'a userdata') = struct
        module M' = L (C)
        let init g = M'.init g
      end (*M*)
    end (*WithType*)
module Empty = struct
  module Type = Combine.T1 (Unused.Type)
  module Library = 
    struct
      type 'a userdata' = 'a Type.t
      module M (C : CORE with type 'a V.userdata' = 'a userdata') = struct
        let init g = ()
      end (*M*)
    end (*Empty.Library*)
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
     with type 'a TV10.t = 'a T.TV10.t =
  struct
    type 'a t = 'a View.combined
    type 'a also_t = 'a t
    module type VIEW = TYPEVIEW with type 'a combined = 'a also_t
    module Lift (T : T.VIEW) : VIEW with type 'a t = 'a T.t = struct
      type 'a combined = 'a also_t
      type 'a t = 'a T.t
      let makemap upper fail =
        let fail' x y = ignore(fail x y); assert false in
        let upper = View.makemap upper fail' in
        T.makemap upper fail
    end

    module TV1  = Lift(T.TV1)
    module TV2  = Lift(T.TV2)
    module TV3  = Lift(T.TV3)
    module TV4  = Lift(T.TV4)
    module TV5  = Lift(T.TV5)
    module TV6  = Lift(T.TV6)
    module TV7  = Lift(T.TV7)
    module TV8  = Lift(T.TV8)
    module TV9  = Lift(T.TV9)
    module TV10 = Lift(T.TV10)
  end (* Lift *)
