module IO = Luaiolib
module Make (T : Lua.Lib.TYPEVIEW with type 'a t = 'a Luaiolib.t)
    : Lua.Lib.USERCODE with type 'a userdata' = 'a T.combined =
  struct
    type 'a userdata' = 'a T.combined
    module M (C : Lua.Lib.CORE with type 'a V.userdata' = 'a userdata') =
     struct
       module V = C.V
       let ( **-> ) = V.( **-> )
       let ( --> ) = V.( --> )
       let init = 
let ( **->> ) x y = x **-> V.result y in
let a      = V.value in
let b      = V.value in
let list   = V.list in
let string = V.string in
let int    = V.int in
let bool   = V.bool in
let ef     = V.efunc in
let caml_modules =
  let swap (x, y) = (y, x) in
  List.map (fun (m, vs) -> (m, V.LuaValueBase.Table (V.Table.of_list (List.map swap vs))))
  ["Filename", 
       (let extension s =
         try		
           let without = Filename.chop_extension s in
           let n = String.length without in
           String.sub s n (String.length s - n)
         with Invalid_argument _ -> "" in
       let chop s = try Filename.chop_extension s with Invalid_argument _ -> s in
       [ ef (string **-> string **->> V.bool) Filename.check_suffix, "check_suffix"
       ; ef (string **->> string) chop, "chop_extension"
       ; ef (string **->> string) extension, "extension"
       ; ef (string **-> string **->> string) Filename.concat, "concat"
       ; ef (string **->> string) Filename.basename, "basename"
       ; ef (string **->> string) Filename.dirname, "dirname"
       ; ef (string **-> string **->> string) Filename.temp_file, "temp_file"
       ; ef (string **->> string) Filename.quote, "quote"
       ])
  ; "List",
       [ ef (list a **->> int)                   List.length,     "length"
       ; ef (list a **->> list a)                List.rev,        "rev"
       ; ef (list a **-> list a **->> list a)    List.append,     "append"
       ; ef (list a **-> list a **->> list a)    List.rev_append, "rev_append"
       ; ef (list (list a) **->> list a)         List.concat,     "concat"
       ; ef ((a --> b) **-> list a **->> list b) List.map,        "map"
       ; ef ((a --> V.unit) **-> list a **->> V.unit) List.iter,  "iter"
       ; ef ((a --> b) **-> list a **->> list b)    List.rev_map, "rev_map"
       ; ef ((a --> bool) **-> list a **->> bool)   List.for_all, "for_all"
       ; ef ((a --> bool) **-> list a **->> bool)   List.exists,  "exists"
       ; ef ((a --> bool) **-> list a **->> list a) List.filter,  "filter"
       ; ef (V.func (a **-> a **->> int) **-> list a **->> list a) List.sort, "sort"
       ; ef (V.func (a **-> a **->> int) **-> list a **->> list a) List.stable_sort,
                         "stable_sort"
       ] 
  ] in

         C.register_module "Caml" caml_modules
     end (*M*)
  end (*Make*)
