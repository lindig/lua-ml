
module SM = Map.Make(String)
module LP = Litprog

exception NoSuchFormat of string

type t = out_channel -> LP.doc -> unit

let (@@) f x = f x
let fprintf  = Printf.fprintf

module Markdown = struct
    
    let output_code io =
        String.iter (function
            | '\n' -> output_string io "\n    "
            |  c   -> output_char io c
            )        
            
    let code io = function
        | LP.Str(_,str) -> output_code io str
        | LP.Ref(str)   -> fprintf io "<<%s>>" str   

    let chunk io = function
        | LP.Doc(str)          -> output_string io str
        | LP.Code(name, src)   -> 
            ( output_code io @@ Printf.sprintf "    <<%s>>=\n" name
            ; List.iter (code io) src
            ; output_char io '\n'
            )
        
    let weave io chunks = List.iter (chunk io) chunks
end

let formats =
    let add map (keys,value) = 
        List.fold_left (fun m k -> SM.add k value m) map keys
    in
    List.fold_left add SM.empty
    [ ["plain";"markdown"], Markdown.weave
    ]

let lookup fmt = 
    try 
        SM.find fmt formats
    with
        Not_found -> raise (NoSuchFormat fmt)

(* this shadows the table above *)
let formats = List.map fst @@ SM.bindings formats    


