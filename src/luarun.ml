module type INTERP = sig
  module Value : Luavalue.S
  type value = Value.value
  type state = Value.state
  val mk        : unit -> state
  val dostring  : ?file:string -> state -> string -> value list
  val dofile    : state -> string -> value list
end
module Make (I : INTERP) = struct
  module V = I.Value
  let state = I.mk()
  let dumpstate = ref false
  let run infile = ignore (I.dofile state infile)
  let run_interactive infile =
    let rec loop n pfx =
      let line = input_line infile in
      if String.length line > 0 && String.get line (String.length line - 1) = '\\' then
        loop n (pfx ^ String.sub line 0 (String.length line - 1) ^ "\n")
      else
        begin
          ignore (I.dostring state (pfx ^ line ^ "\n"));
          flush stdout; flush stderr;
          loop (n+1) ""
        end
    in  try loop 1 "" with End_of_file -> ()
  let rec args = function
    | "-dump" :: a's -> (dumpstate := true; args a's)
    | "-new"  :: a's -> args a's
    | [] -> run_interactive stdin
    | files -> List.iter run files
  
  let _ = args (List.tl (Array.to_list (Sys.argv)))
  
  let _ = if !dumpstate then
    begin
      print_endline "final state: ";
      V.Luahash.iter (fun k v -> print_string "  ";
        print_string (V.to_string k); print_string " |-> ";
        print_endline (V.to_string v)) state.V.globals
    end
end

