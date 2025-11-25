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

module MakeFromAST
             (A : Luaast.S)
             (L : Lualib.USERCODE with type 'a userdata' = 'a A.Value.userdata') :
    S with module Value = A.Value and module Ast = A = struct
  module Value = A.Value
  module Ast   = A
  module I = struct
    type state = Value.state
    type value = Value.value
    (* begin with internal abbreviations *)
    module A = Ast
    module V = Ast.Value
    let proj_string g v =
  let what = match v with
  | V.LuaValueBase.Table t ->
      let l = try (V.list V.value).V.project v with _ -> [] in
      let not_nil = function V.LuaValueBase.Nil -> false | _ -> true in
      if Value.Luahash.length t = List.length l && List.for_all not_nil l then
        "{ " ^ String.concat ", " (List.map V.to_string l) ^ " }"
      else
        V.to_string v
  | _ -> V.to_string v in
  let spr = Printf.sprintf in
  match V.objname g v with
  | Some (V.Fallback n)     -> spr "%s (fallback %s)" what n
  | Some (V.Global n)       -> spr "'%s %s'" what n
  | Some (V.Element (s, v)) -> spr "'%s %s[%s]'" what s (V.to_string v)
  | None -> what
let currentloc_tostack g =
  match g.V.callstack with
  | (info, _) :: t -> g.V.callstack <- (info, g.V.currentloc) :: t
  | [] -> ()

let currentloc_fromstack g =
  match g.V.callstack with
  | (_, where) :: _ -> g.V.currentloc <- where
  | [] -> ()

type var = Global | Local of int
let lookup rho x =
  let rec look = function
    | [] -> Global
    | h :: t when h = x -> Local (List.length t)
    | _ :: t -> look t
  in look rho


let notnil = function
  | V.LuaValueBase.Nil -> false
  | _ -> true

let with_stack info g f x =
  let _ = currentloc_tostack g in
  let _ = g.V.callstack <- (info, None) :: g.V.callstack in
  let _ = currentloc_fromstack g in
  let pop () = g.V.callstack <- List.tl g.V.callstack; currentloc_fromstack g in
  let answer = try f x with e -> (pop(); raise e) in
  let _ = pop() in
  answer

exception Errorfallback of V.value list

let error s = raise (Errorfallback [V.LuaValueBase.String s])

exception Error of string

let default_error_fallback g args =
  let () = currentloc_tostack g in
  let msg = match args with V.LuaValueBase.String s :: _ -> s | _ -> "??error w/o message??" in
  let stack_trace = List.map (fun a -> V.activation_strings g a) g.V.callstack |> List.map (fun ss -> String.concat "" ss) |> String.concat "\n" in
  let msg = Printf.sprintf "%s\nStack trace:\n%s" msg stack_trace in
  raise (Error msg)

let dump_state g = 
  let err = prerr_string in
  let rec value = function
    | V.LuaValueBase.Table t -> tab t ""
    | v -> err (V.to_string v)
  and tab t sfx =
    err "{"; Value.Luahash.iter (fun k d -> err " "; value k; err "="; value d; err ",") t;
    err "}"; err sfx in
  let stab t sfx =
    err "{"; Hashtbl.iter (fun k d -> err " "; err k; err "="; value d; err ",") t;
    err "}"; err sfx in
  err "state is: \n";
  err "  globals =\n    ";
  tab g.V.globals "\n";
  err "  fallbacks =\n    ";
  stab g.V.fallbacks "\n";
  default_error_fallback g [V.LuaValueBase.String "Stack trace is:"]


let rec fallback fbname g args =
  let call f g args = match f with
  | V.LuaValueBase.Function (info, f) -> with_stack info g f args
  | v when fbname <> "function" -> fallback "function" g (v :: args)
  | _ -> default_error_fallback g [V.LuaValueBase.String "`function' fallback not a function"] in
  let fbval = try Hashtbl.find g.V.fallbacks fbname
              with Not_found -> begin
  prerr_string "no fallback named `";
  prerr_string fbname;
  prerr_endline "' (probably registered an impure function as pure)";
  let () = dump_state g in
  assert false (* can't have any unknown fallbacks *)
end
 in
  call fbval g args
let catcherrorfallback g vs =
  ignore (fallback "error" g vs);
  raise (Error "Error fallback returned a value")

let apply f g args = match f with
  | V.LuaValueBase.Function (info, f) ->
         ( try (with_stack info g f args) with
         | V.Projection (v, what) -> fallback "error" g
         [V.LuaValueBase.String ("cannot convert value " ^ proj_string g v ^ " to " ^ what)]

         | Errorfallback vs -> catcherrorfallback g vs
        (*** need the stack trace
         | Invalid_argument msg -> fallback "error" g [V.String ("Function raised Invalid_argument " ^ msg)]

         ***)
         ) 
  | v -> fallback "function" g (v :: args)
(*unboxval*)

let fb1 name state args = match fallback name state args with
  | [] -> V.LuaValueBase.Nil
  | h :: _ -> h
(*unboxval*)
let arith opname op =
  let opname = V.LuaValueBase.String opname in (* allocate early and share *)
  let f x y g = try
    let x = V.float.V.project x in
    let y = V.float.V.project y in
    V.float.V.embed (op x y)
  with V.Projection (_, _) -> fb1 "arith" g [x; y; opname]
  in f
(*unboxval*)
let negate x g = try
  let x = V.float.V.project x in
  V.float.V.embed (~-. x)
  with V.Projection (_, _) -> fb1 "arith" g [x; V.LuaValueBase.Nil; V.LuaValueBase.String "umn"]
let order opname nop sop =
  let opname = V.LuaValueBase.String opname in
  let f x y g =
    match x, y with
    | V.LuaValueBase.Number x, V.LuaValueBase.Number y -> V.bool.V.embed (nop x y)
    | _ -> try let x = V.string.V.project x in
               let y = V.string.V.project y in
               V.bool.V.embed (sop x y)
           with V.Projection (_, _) -> fb1 "order" g [x; y; opname]
  in f
(*unboxval*)
let concat x y g =
  try let x = V.string.V.project x in
      let y = V.string.V.project y in
      V.string.V.embed (x ^ y)
  with V.Projection (_, _) -> fb1 "concat" g [x; y]

(* All Lua numbers are floats,
   so for the purpose of modulus calculation,
   we have to force them to ints *)
let fmod x y =
  let x = int_of_float x in
  let y = int_of_float y in
  float_of_int (x mod y)

let binop = function
  | A.Plus   -> arith "add" (+.)
  | A.Minus  -> arith "sub" (-.)
  | A.Times  -> arith "mul" ( *. )
  | A.Div    -> arith "div" ( /. )
  | A.Mod    -> arith "mod" fmod
  | A.Pow    -> fun x y g -> fb1 "arith" g [x; y; V.LuaValueBase.String "pow"]
  | A.Lt     -> order "lt" (<)  (<)
  | A.Le     -> order "le" (<=) (<=)
  | A.Gt     -> order "gt" (>)  (>)
  | A.Ge     -> order "ge" (>=) (>=)
  | A.Eq     -> fun x y _ -> V.bool.V.embed (V.eq x y)
  | A.Ne     -> fun x y _ -> V.bool.V.embed (not (V.eq x y))
  | A.And    -> assert false (* short circuit *)
  | A.Or     -> assert false (* short circuit *)
  | A.Concat -> concat
  | A.Not    -> assert false (* unary *)

let unop = function
  | A.Minus  -> negate
  | A.Not    -> fun v _ -> (match v with V.LuaValueBase.Nil -> V.LuaValueBase.Number 1.0 | _ -> V.LuaValueBase.Nil)
  | _        -> assert false (* all other operators are binary *)
(*unboxval*)
let index g t key = match t with
| V.LuaValueBase.Table t ->
    (match V.Table.find t ~key with
    | V.LuaValueBase.Nil -> fb1 "index" g [V.LuaValueBase.Table t; key]
    | v -> v)
| _ -> fb1 "gettable" g [t; key]

let settable g t key v = match t with
| V.LuaValueBase.Table t -> V.Table.bind t ~key ~data:v
| _ -> ignore (fallback "settable" g [t; key; v])
let getglobal g k =
  match V.Table.find g.V.globals ~key:k with
  | V.LuaValueBase.Nil -> fb1 "getglobal" g [k]
  | v -> v
let setglobal g k v = V.Table.bind g.V.globals ~key:k ~data:v
let setlocal locals n v = Array.set locals n v    (* could be made unsafe *)
let getlocal locals n   = Array.get locals n 

let rec getlocals locals n count =
  if count = 0 then []
  else getlocal locals n :: getlocals locals (n+1) (count-1)
let rec extend rho = function
  | A.Stmt' (_, s) -> extend rho s
  | A.Local (vs, _) -> List.rev_append vs rho
  | _ -> rho

    type compiled = unit -> value list
type 'a cont  = V.value array -> 'a  (* for exp1 *)

let block_compiler srcmap g = 
  let append argv rest = match rest with [] -> argv | _ -> argv @ rest in
        (* optimizes common case *)
let rec exp1 localref =
  let rec exp1 rho e loc theta = 
    let finish v l = setlocal l loc v; theta l in
    match e with
    | A.Var x -> localref loc;
                 (match rho x with
                  | Global  -> fun l -> finish (getglobal g (V.LuaValueBase.String x)) l
                  | Local n -> fun l -> finish (getlocal l n) l)
    | A.Lit v -> localref loc; fun l -> finish v l
    | A.Index (tab, key) ->
        let tabloc = loc in
        let keyloc = loc + 1 in
        let theta l = finish (index g (getlocal l tabloc) (getlocal l keyloc)) l in
        exp1 rho tab tabloc (exp1 rho key keyloc theta) 
    | A.Table (lists, bindings) ->
        localref loc;                     (* needed if table is empty *)
        let tabloc = loc in
        let vloc   = loc + 1 in
        let tbl l = match getlocal l tabloc with V.LuaValueBase.Table t -> t | _ -> assert false in
        let rec listbind n theta = function
          | [] -> bind theta bindings
          | h::t ->
              let theta = listbind (n +. 1.0) theta t in
              let theta = fun l -> V.Table.bind (tbl l) ~key:(V.LuaValueBase.Number n) ~data:(getlocal l vloc);
                                   theta l
              in  exp1 rho h vloc theta
                (* PERHAPS FOR LAST ELEMENT IN LIST, SHOULD CAPTURE *ALL* RESULTS? *)
        and bind theta = function
          | [] -> theta
          | (n, h) :: t ->
              let theta = bind theta t in
              let theta = fun l -> V.Table.bind (tbl l) ~key:(V.LuaValueBase.String n) ~data:(getlocal l vloc);
                                   theta l
              in  exp1 rho h vloc theta in
        let size  = List.length bindings + List.length lists in
        let theta = listbind 1.0 theta lists in
        fun l ->
          let t = V.Table.create size in
          setlocal l tabloc (V.LuaValueBase.Table t);
          theta l
    | A.Binop (e1, op, e2) ->
        let short_circuit theta_t theta_f = fun l ->
          match getlocal l loc with
          | V.LuaValueBase.Nil -> theta_f l
          | _ ->     theta_t l in
        ( match op with
        | A.And -> exp1 rho e1 loc (short_circuit (exp1 rho e2 loc theta) theta)
        | A.Or  -> exp1 rho e1 loc (short_circuit theta (exp1 rho e2 loc theta))
        | _ ->
            let loc1 = loc in
            let loc2 = loc + 1 in
            let op = binop op in
            exp1 rho e1 loc1 (
            exp1 rho e2 loc2 (
            fun l -> finish (op (getlocal l loc1) (getlocal l loc2) g) l)))
    | A.Unop (op, e) ->
        let op = unop op in
        exp1 rho e loc (fun l -> finish (op (getlocal l loc) g) l)
    | A.Call _ -> exp localref rho e loc (fun _ -> theta)
in  exp1
and exp localref rho e loc theta = 
  let finish  vs l = match vs with
  | v :: vs -> setlocal l loc v; theta vs l
  | []      -> setlocal l loc V.LuaValueBase.Nil; theta [] l in
  match e with
  | A.Call c -> localref loc; call localref c rho loc finish
  | _ -> exp1 localref rho e loc (theta [])
and explist localref rho es loc theta = match es with
| [] -> theta []
| [e] -> exp localref rho e loc theta
| e :: es -> exp1 localref rho e loc (explist localref rho es (loc+1) theta) 
and call localref c rho loc theta = match c with
| A.Funcall (f, args) ->
    let argcount = List.length args in
    let argloc = loc + 1 in
    exp1 localref rho f loc (
    explist localref rho args argloc
      (fun vs l ->
        let fv   = getlocal l loc in
        let argv = getlocals l argloc argcount in
        theta (apply fv g (append argv vs)) l))
| A.Methcall (obj, meth, args) ->
    let mloc     = loc in
    let selfloc  = mloc + 1 in
    let argloc   = selfloc + 1 in
    let argcount = List.length args + 1 in
    let meth     = V.LuaValueBase.String meth in
    exp1 localref rho obj selfloc (
      let theta_m = explist localref rho args argloc
          (fun vs l ->
            let fv = getlocal l loc in
            let argv = getlocals l selfloc argcount in
            theta (apply fv g (append argv vs)) l) in
      fun l -> setlocal l mloc (index g (getlocal l selfloc) meth); theta_m l)
 in
  let high_local_limit = ref 0 in
  let localref n = if n >= !high_local_limit then high_local_limit := n+1 in
  let local_size () = !high_local_limit in
  let bcomp ~debug =
      let rec stmt rho s (theta: 'a cont)  (ret:value list -> 'a) = match s with
  | A.Stmt' (charpos, s) ->
      if debug then
        (* might make interesting example for paper *)
        let where = Luasrcmap.location srcmap charpos in
        let restore = ref (fun () -> ()) in  (* will restore currentloc *)
        let theta' l = (!restore(); theta l) in
        let ret' ans = (!restore(); ret ans) in
        let stheta = stmt rho s theta' ret' in
        fun l -> let n = g.V.currentloc in
                 ( restore := (fun () -> g.V.currentloc <- n)
                 ; g.V.currentloc <- Some where
                 ; stheta l
                 ) 
            (* hard to maintain current line if exn raised ... *)
      else
        stmt rho s theta ret
  | A.WhileDo (cond, body) ->
      let loop_cont = ref theta in  (* to become loop continuation *)
      let goto_head l = !loop_cont l in
      let condloc = List.length rho in
      let body = block rho body goto_head ret in
      let loop =
        exp1 localref (lookup rho) cond condloc
          (fun l -> if notnil (getlocal l condloc) then body l else theta l) in
      let _ = loop_cont := loop in
      loop
  | A.RepeatUntil (body, cond) ->
      let loop_test = ref theta in (* to become loop-end continuation *)
      let goto_test l = !loop_test l in
      let condloc = List.length rho in
      let body = block rho body goto_test ret in
      let loop =
        exp1 localref (lookup rho) cond condloc
          (fun l -> if notnil (getlocal l condloc) then theta l else body l) in
      let _ = loop_test := loop in
      body
  | A.If (c, t, alts, f) ->
      let alts = (c, t) :: alts in
      let f = block rho (match f with None -> [] | Some ss -> ss) theta ret in
      let condloc = List.length rho in
      let add (cond, body) f =
        let body = block rho body theta ret in
        exp1 localref (lookup rho) cond condloc (
          fun l -> if notnil (getlocal l condloc) then body l else f l) in
      List.fold_right add alts f
  | A.Return es ->
      let loc = List.length rho in
      let result_count = List.length es in 
      explist localref (lookup rho) es loc 
      (fun vs l -> ret (append (getlocals l loc result_count) vs))
  | A.Local (vs, es) ->
      stmt (List.rev_append vs rho) (A.Assign (List.map (fun x -> A.Lvar x) vs, es))
      theta ret
  | A.Assign (vs, es) ->
      let rhscount = List.length es in
      lvars localref (lookup rho) (List.length rho) vs (fun setlvs loc ->
        explist localref (lookup rho) es loc (fun vs l ->
          setlvs l (append (getlocals l loc rhscount) vs);
          theta l))
  | A.Callstmt c ->
      call localref c (lookup rho) (List.length rho) (fun _ l -> theta l)
and block rho body (theta:'a cont) (ret:V.value list -> 'a) = match body with
  | [] -> theta
  | s :: ss -> stmt rho s (block (extend rho s) ss theta ret) ret
and lvar localref rho lv lhsloc nextlvar = 
  match lv with
  | A.Lvar x -> 
      let setx = match rho x with
      | Global  -> fun _ v -> setglobal g (V.LuaValueBase.String x) v
      | Local n -> fun l v -> setlocal l n v in
      nextlvar setx lhsloc
  | A.Lindex (t, key) ->
      let keyloc = lhsloc + 1 in
      let setidx = exp1 localref rho t lhsloc (exp1 localref rho key keyloc (fun l ->
            let t   = getlocal l lhsloc in
            let key = getlocal l keyloc in
            (fun v -> settable g t key v))) in
      nextlvar setidx (lhsloc+2)
and lvars localref rho loc lvs finish = match lvs with
  | [] -> finish (fun _l _vs -> ()) loc
  | h :: t ->
      lvar localref rho h loc (fun setter loc ->
        lvars localref rho loc t (fun setlvs loc ->
          let setlvs l vs =
            let v, vs = match vs with h::t -> h, t | [] -> V.LuaValueBase.Nil, [] in
            setter l v;
            setlvs l vs in
          finish setlvs loc))
 in
(*inboxval*)
      block
  in  bcomp, local_size
let value_list = V.list V.value
let lambda (src, debug) (file, line, _col) args varargs body state =
  let rho = let args' = List.rev args in if varargs then "arg" :: args' else args' in
  let block, count = block_compiler src state in
  let body = block ~debug rho body (fun _ -> []) (fun results -> results) in
  let n = max (count()) (List.length rho) in
  let srcloc = V.srcloc ~file ~linedefined:line in
  srcloc, 
  fun argv ->
    let locals = Array.make n V.LuaValueBase.Nil in
    let rec walk n formals actuals = match formals with
      | [] -> if varargs then Array.set locals n (value_list.V.embed actuals)
      | _ :: fs ->
          let a, a's = match actuals with [] -> V.LuaValueBase.Nil, [] | h :: t -> h, t in
          (Array.set locals n a; walk (n+1) fs a's)  in
    let _ = walk 0 args argv in
    body locals
(*unboxval*)
let func (info, f) = V.LuaValueBase.Function (info, f)
let chunk ((smap, _) as srcdbg) block rho g = function
  | A.Debug _ -> assert false (* must never get here *)
  | A.Statement s -> block rho [s]
  | A.Fundef (pos, f, (args, varargs), body) ->
      let v = func (lambda srcdbg (Luasrcmap.location smap pos) args varargs body g) in
      block rho [A.Stmt'(pos, A.Assign ([f], [A.Lit v]))]
  | A.Methdef (pos, obj, meth, (args, varargs), body) ->
      let args = "self" :: args in
      let v = func (lambda srcdbg (Luasrcmap.location smap pos) args varargs body g) in
      block rho [A.Stmt'(pos, A.Assign ([A.Lindex (obj, A.Lit (V.LuaValueBase.String meth))],
                                        [A.Lit v]))]
let extendchunk rho = function
  | A.Statement s -> extend rho s
  | _ -> rho

let compile ~srcdbg cs g = 
  let block, count = block_compiler (fst srcdbg) g in
  let ret = fun results -> results in
  let rec chunks ((smap, debug) as srcdbg) rho = function
    | [] -> fun _ -> []
    | A.Debug dbg :: t -> chunks (smap, dbg) rho t
    | h :: t -> chunk srcdbg (block ~debug) rho g h
                (chunks srcdbg (extendchunk rho h) t) ret in
  let theta = chunks srcdbg [] cs in
  let locals = Array.make (count()) V.LuaValueBase.Nil in
  fun () -> theta locals

    let errorfallback s g = fun _args -> fallback "error" g [V.LuaValueBase.String s]
let arithfallback g   = function
  | [V.LuaValueBase.Number x; V.LuaValueBase.Number y; V.LuaValueBase.String s] when s = "pow" -> [V.LuaValueBase.Number (x ** y)]
  | args -> errorfallback "unexpected type at conversion to number" g args
let funcfallback g  = function 
    | f::args   -> 
        let args' = String.concat ", " (List.map V.to_string args) in 
        let call  = Printf.sprintf "%s(%s)" (V.to_string f) args' in
        fallback "error" g [V.LuaValueBase.String ("call expr is "^call)]
    | _args      -> fallback "error" g [V.LuaValueBase.String "call expr not a function"]
      
let fbs g =
  [ "arith",     arithfallback g
  ; "order",     errorfallback "unexpected type at comparison" g
  ; "concat",    errorfallback "unexpected type at conversion to string" g
  ; "index",     (fun _args -> [V.LuaValueBase.Nil])
  ; "getglobal", (fun _args -> [V.LuaValueBase.Nil])
  ; "gettable",  errorfallback "indexed expression not a table" g
  ; "settable",  errorfallback "indexed expression not a table" g
  ; "function",  funcfallback g
  ; "error",     default_error_fallback g
  ] 
let add_fallbacks g =
  List.iter (fun (k, f) -> Hashtbl.add g.V.fallbacks k (V.caml_func f)) (fbs g)
let setfallback g fbname fb =
  let fb' = try Hashtbl.find g.V.fallbacks fbname with Not_found -> V.LuaValueBase.Nil in
  let _   = Hashtbl.replace g.V.fallbacks fbname fb in
  fb'

    let register_global g k v =
  match getglobal g k with
  | V.LuaValueBase.Nil -> setglobal g k v
  | _ -> Printf.ksprintf failwith "Global variable '%s' is already set" (V.to_string k)

let register_globals l g = List.iter (fun (k, v) -> register_global g (V.LuaValueBase.String k) v) l

let register_module tabname members g =
  let t = getglobal g (V.LuaValueBase.String tabname) in
  let t = match t with
  | V.LuaValueBase.Nil       -> V.Table.create (List.length members)
  | V.LuaValueBase.Table t   -> t
  | _           -> catcherrorfallback g
                   [V.LuaValueBase.String ("Global value " ^ tabname ^ " is not (table or nil)")] in
  let _ = register_global g (V.LuaValueBase.String tabname) (V.LuaValueBase.Table t) in
  let bind (k, v) = match V.Table.find t ~key:(V.LuaValueBase.String k) with
  | V.LuaValueBase.Nil -> V.Table.bind t ~key:(V.LuaValueBase.String k) ~data:v
  | _ ->
      Printf.ksprintf failwith "Duplicate '%s' registered in module '%s'" k tabname in
  List.iter bind members

      
(* let nil = A.Lit V.LuaValueBase.Nil *)
(* let three = A.Lit (V.LuaValueBase.Number 3.0) *)

(* let ret = A.Return ([nil; three]) *)

(* let test_state = V.state () *)

(* let bogusmap = Luasrcmap.mk () *)
(* let stmts l = compile ~srcdbg:(bogusmap, false) (List.map (fun s -> A.Statement s) l) *)

(* let num n = A.Lit (V.LuaValueBase.Number (float n)) *)
(* let rtest = stmts [ret] *)
(* let sum = stmts [A.Return ([A.Binop (three, A.Plus, three)])] *)
(* let exp = stmts [A.Return ([A.Binop (three, A.Times, A.Binop (num 2, A.Minus, three))])] *)
(* let x = A.Var "x" *)
(* let gets x e = A.Assign ([A.Lvar x], [e]) *)
(* let binary op e1 e2 = A.Binop (e1, op, e2) *)
(* let lt = binary A.Lt *)
(* let times = binary A.Times *)
(* let loop = [ gets "x" (num 10) *)
(*                ; A.WhileDo (lt x (num 100), [gets "x" (times (num 2) x)]) *)
(*                ; A.Return [x]] *)

(* let test _ = *)
(*   [ "return nil and three", rtest test_state *)
(*   ; "sum of three and three", sum test_state *)
(*   ; "expression minus three", exp test_state *)
(*   ; "result of loop", stmts loop test_state *)
(*   ] *) 

  end
  module Core = struct
    include I
  end
  module L' = L.M(Core)
  include I
  type startup_code = (string -> unit) -> unit
  let pre_mk () = (* raw state + fallbacks + library initialization + startup code*)
    let g = V.state() in
    begin
      add_fallbacks g;
      L'.init g;
      g, V.initcode g
    end

end (* MakeFromAST *)

module Make  (T : Luavalue.USERDATA)
             (L : Lualib.USERCODE with type 'a userdata' = 'a T.t) :
    S with type 'a Value.userdata'  = 'a T.t = struct
  module V = Luavalue.Make(T)
  module A   = Luaast.Make (V)
  include MakeFromAST (A) (L)
end
