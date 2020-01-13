module Add (MakeParser : Luaparser.MAKER) (I : Luainterp.S) = struct
  module Parser = MakeParser (I.Ast)
  module P = Parser
  module V = I.Value
  let lex map buf = Luascanner.token buf map
let do_lexbuf ~sourcename:filename g buf =
  let map = Luasrcmap.mk () in
  let _ = Luasrcmap.sync map 0 (filename, 1, 1) in
  try
    let chunks = P.chunks (lex map) buf in
    let pgm = I.compile ~srcdbg:(map, false) chunks g in
    match pgm () with
    | [] -> [I.Value.LuaValueBase.String "executed without errors"]
    | answers -> answers
  with
  | Parsing.Parse_error ->
    let file, line, _ = Luasrcmap.last map in
    let errmsg = Printf.sprintf "%s: Syntax error on line %d" file line in
    failwith errmsg
  | I.Error s -> failwith (Printf.sprintf "Runtime error: %s" s)
  | I.Value.Projection (v, w) -> (failwith ("Error projecting to " ^ w); [])


let dostring ?(file="<string>") g s =
  let abbreviate s =
    if String.length s < 200 then s
    else String.sub s 0 60 ^ "..." in
  I.with_stack (V.srcloc ("dostring('" ^ abbreviate s ^ "')") 0) g
    (do_lexbuf ~sourcename:file g) (Lexing.from_string s)

let dofile g infile =
  try
    let f = match infile with "-" -> stdin | _ -> open_in infile in
    let close () = if infile <> "-" then close_in f else () in
    try 
      let answer = I.with_stack (V.srcloc ("dofile('" ^ infile ^ "')") 0) g
                     (do_lexbuf ~sourcename:infile g) (Lexing.from_channel f)
      in  (close(); answer)
    with e -> (close (); raise e)
  with Sys_error msg -> [V.LuaValueBase.Nil; V.LuaValueBase.String ("System error: " ^ msg)]

  let ( **-> ) = V.( **-> )
  let ( **->> ) x y = x **-> V.result y

  let next t key =
    let k, v =
      try match key with
      | V.LuaValueBase.Nil -> V.Table.first t
      | _   -> V.Table.next t key
      with Not_found -> V.LuaValueBase.Nil, V.LuaValueBase.Nil
    in [k; v]

  let objname g v =
    let tail = [] in
    let ss = match V.objname g v with
    | Some (V.Fallback n) -> "`" :: n :: "' fallback" :: tail
    | Some (V.Global n)   -> "function " :: n :: tail
    | Some (V.Element (t, V.LuaValueBase.String n)) -> "function " :: t :: "." :: n :: tail
    | Some (V.Element (t, v))    -> "function " :: t :: "[" :: V.to_string v :: "]" :: tail
    | None -> "unnamed " :: V.to_string v :: tail in
    String.concat "" ss



  let luabaselib g =
    [ "dofile",      V.efunc (V.string **-> V.resultvs) (dofile g)
    ; "dostring",    V.efunc (V.string **-> V.resultvs) (dostring g)
        (* should catch Sys_error and turn into an error fallback... *)
    ; "size",        V.efunc (V.table **->> V.int) V.Luahash.length
    ; "next",        V.efunc (V.table **->  V.value **-> V.resultvs) next
    ; "nextvar",     V.efunc (V.value **->  V.resultvs) (fun x -> next g.V.globals x)
    ; "tostring",    V.efunc (V.value **->> V.string) V.to_string
    ; "objname",     V.efunc (V.value **->> V.string) (objname g)
    ; "print",       V.caml_func 
                        (fun args ->
                          List.iter (fun x -> print_endline (V.to_string x)) args;
                          flush stdout;
                          [])
    ; "tonumber",    V.efunc (V.float **->> V.float) (fun x -> x)
    ; "type",        V.efunc (V.value **->> V.string)
                     (function
                     | V.LuaValueBase.Nil            -> "nil"
                     | V.LuaValueBase.Number _       -> "number"
                     | V.LuaValueBase.String _       -> "string"
                     | V.LuaValueBase.Table _        -> "table"
                     | V.LuaValueBase.Function (_,_) -> "function"
                     | V.LuaValueBase.Userdata _     -> "userdata")
    ; "assert",      V.efunc (V.value **-> V.default "" V.string **->> V.unit)
                     (fun c msg -> match c with
                     | V.LuaValueBase.Nil -> I.error ("assertion failed: " ^ msg)
                     | _ -> ())
    ; "error",       V.efunc (V.string **->> V.unit) I.error
    ; "setglobal",   V.efunc (V.value **-> V.value **->> V.unit)
                     (fun k v -> V.Table.bind g.V.globals k v)
    ; "getglobal",   V.efunc (V.value **->> V.value) (I.getglobal g)
    ; "setfallback", V.efunc (V.string **-> V.value **->> V.value) (I.setfallback g)
    ] 

  include I
  let mk () =
    let g, init = I.pre_mk () in
    I.register_globals (luabaselib g) g;
    init (fun s -> ignore (dostring g s));
    g
end
