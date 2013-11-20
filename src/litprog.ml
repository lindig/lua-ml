

module SM = Map.Make(String)
module SS = Set.Make(String)
module T  = Tangle

exception NoSuchChunk of string
exception Cycle       of string



type code =
    | Str       of T.position * string
    | Ref       of string

type chunk =
    | Doc       of string
    | Code      of string * code list

type doc = chunk list

type t = 
    { code:     (code list) SM.t
    ; chunks:   chunk list
    }

let printf = Printf.printf
let (@@) f x = f x

let empty =
    { code      = SM.empty 
    ; chunks    = []
    }

let append key v map =
    if SM.mem key map
    then SM.add key ((SM.find key map)@v) map
    else SM.add key v map

let make chunks =
    let add t = function
        | Doc(str)   as d ->    { t with chunks = d :: t.chunks }
        | Code(n,cs) as c ->    { code   = append n cs t.code
                                ; chunks = c::t.chunks
                                }
    in
    let t = List.fold_left add empty chunks in
        { t with chunks = List.rev t.chunks}

let doc t = t.chunks

let code_chunks t = 
    SM.fold (fun name _ names -> name::names) t.code []

let code_roots t = 
    let add name _ names = SS.add name names in
    let roots = SM.fold add t.code SS.empty in
    let rec traverse_chunk roots = function
        | Doc(_)       -> roots
        | Code(_,code) -> List.fold_left traverse_code roots code 
    and traverse_code roots = function
        | Str(_,_)  -> roots
        | Ref(n)    -> SS.remove n roots
    in
        SS.elements @@ List.fold_left traverse_chunk roots t.chunks

let references t = 
    let code refs = function
        | Str(_)        -> refs
        | Ref(r)        -> SS.add r refs in
    let chunk refs = function
        | Doc(_)        -> refs
        | Code(_,cs)    -> List.fold_left code refs cs 
    in
        List.fold_left chunk SS.empty t.chunks

let unknown_references t =
    let (++) = SS.add in
    let (--) = SS.diff in
    let refs = references t in
    let defs = SM.fold 
                (fun name _ names -> name ++ names) t.code SS.empty in
        SS.elements @@ refs -- defs 

let lookup name map =
    try
        SM.find name map
    with 
        Not_found -> raise (NoSuchChunk name)


let tangle t emit io chunk =
    let rec loop pred = function
        | []                  -> ()
        | Str(pos,s)::todo    -> emit io pos s; loop pred todo
        | Ref(s)::todo        ->
            if SS.mem s pred then
                raise (Cycle s)
            else
                ( loop (SS.add s pred) (lookup s t.code)
                ; loop pred todo
                )
    in
        loop SS.empty (lookup chunk t.code)

(* Just for debugging during development
 *)

let excerpt s =
    let str = String.escaped s in
    let len = String.length str in
        if len < 40 then str 
        else String.sub str 0 10 ^ "..." ^ String.sub str (len - 10) 10
             
let code = function
    | Str(p,str) -> printf "%3d (%4d): %s\n" 
                        p.T.line p.T.offset (excerpt str)
    | Ref(str)   -> printf "<<%s>>\n"        str
            
let chunk map = function 
    | Doc(str)       -> Printf.printf "@ %s\n"  (excerpt str)
    | Code(name,cs)  -> 
            ( Printf.printf "<<%s>>=\n" name
            ; List.iter code cs
            )

let print litprog = List.iter (chunk litprog.code) litprog.chunks
