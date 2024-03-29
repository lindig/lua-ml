type 'a t = In of in_channel | Out of out_channel
type 'a state = { mutable currentin  : in_channel
                ; mutable currentout : out_channel
                } 
type 'a alias_for_t = 'a t
module T = struct
  type 'a t     = 'a alias_for_t
  let tname = "I/O channel"
  let eq _ x y = match x, y with
  | In x,    In y    -> x = y
  | Out x,   Out y   -> x = y
  | _, _ -> false
  let to_string _ = function
    | In _ -> "<input>"
    | Out _ -> "<output>"
end
module V = Luavalue
let out upper fail =
      { V.embed   = (fun x -> upper.V.embed (Out x))
      ; V.project = (fun x -> match upper.V.project x with
                    | Out x -> x
                    | _ -> fail x "output file")
      ; V.is      = (fun x -> upper.V.is x && match upper.V.project x with
                                              | Out _ -> true | _ -> false)
      } 
let in' upper fail =
      { V.embed   = (fun x -> upper.V.embed (In x))
      ; V.project = (fun x -> match upper.V.project x with
                    | In x -> x
                    | _ -> fail x "input file")
      ; V.is      = (fun x -> upper.V.is x && match upper.V.project x with
                                              | In _ -> true | _ -> false)
      } 
module Make (T : Lua.Lib.TYPEVIEW with type 'a t = 'a t)
    : Lua.Lib.USERCODE with type 'a userdata' = 'a T.combined =
  struct
    type 'a userdata' = 'a T.combined
    module M (C : Lua.Lib.CORE with type 'a V.userdata' = 'a userdata') =
     struct
       module V = C.V
       let ( **-> ) = V.( **-> )
       let ( **->> ) x y = x **-> V.result y
       let ( *****->> ) = V.dots_arrow
       let init g =  (* g needed for readfrom, writeto, appendto *)
         let io = {currentin = stdin; currentout = stdout} in
         let file = T.makemap V.userdata V.projection in
let infile  = in' file V.projection in
let outfile = out file V.projection in

let wrap_err = function
  | V.LuaValueBase.Function (l, f) ->
      V.LuaValueBase.Function(l, fun args -> try f args with Sys_error s -> [V.LuaValueBase.Nil; V.LuaValueBase.String s])
  | v -> raise (V.Projection (v, "function")) in

(* errfunc -- a function that returns nil, string on error *)
let errfunc   ty f = wrap_err (V.efunc ty f)  in
let errchoose alts = wrap_err (V.choose alts) in

(* succeed, succeed2: return non-nil on success *)
let succeed (f : 'a -> unit) (x : 'a) = (f x; "OK") in
let succeed2 f x y = ((f x y : unit); "OK") in

let setglobal s v = V.Table.bind g.V.globals ~key:(V.LuaValueBase.String s) ~data:v in

let readfrom =
  let setinput file = 
    (io.currentin <- file; setglobal "_INPUT" (infile.V.embed file); file) in
  let from_string s =
    if String.get s 0 = '|' then
      setinput (Unix.open_process_in (String.sub s 1 (String.length s - 1)))
    else
      setinput (open_in s) in
  let from_other _ = C.error "bad args to readfrom" in
  [ V.alt (V.string **->> infile) from_string
  ; V.alt (V.unit   **->> infile) (fun () -> (close_in io.currentin; setinput stdin))
  ; V.alt (infile   **->> infile) setinput
  ; V.alt (V.value  **->> infile) from_other
  ]  in

let open_out_append s =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 s  in

let open_out_string append s =
  match String.get s 0 with
  | '|' -> if append then raise (Sys_error "tried to appendto() a pipe")
           else Unix.open_process_out (String.sub s 1 (String.length s - 1))
  | _   -> if append then open_out_append s else open_out s in

let writeto' append = 
  let setoutput file = 
    (io.currentout <- file; setglobal "_OUTPUT" (outfile.V.embed file); file) in
  let to_nil () = (close_out io.currentout; setoutput stdout) in
  let to_other _ = 
    let funname = if append then "appendto" else "writeto" in
    C.error ("bad args to " ^ funname) in
  [ V.alt (V.string **->> outfile)  (fun s -> setoutput (open_out_string append s))
  ; V.alt (V.unit   **->> outfile)  to_nil
  ; V.alt (outfile  **->> outfile)  setoutput
  ; V.alt (V.value  **->> V.value)  to_other
  ]  in
    
let read = function
  | None -> (try Some (input_line io.currentin) with End_of_file -> None)
  | Some _ -> C.error ("I/O library does not implement read patterns")  in

let getopt x d = match x with Some v -> v | None -> d  in

let date = function
  | Some _ -> C.error ("I/O library does not implement read patterns")
  | None ->
      let t = Unix.localtime (Unix.time ()) in
      let s = string_of_int in
      let mm = t.Unix.tm_mon + 1 in
      let yyyy = t.Unix.tm_year + 1900 in
      let dd = t.Unix.tm_mday in
      s mm ^ "/" ^ s dd ^ "/" ^ s yyyy in

let tmpname () = Filename.temp_file "lua" "" in

let write_strings file l = (List.iter (output_string file) l; flush file; 1) in

let io_builtins =
  [ "readfrom",  errchoose readfrom
  ; "open_out",  V.efunc (V.string **->> outfile) (open_out_string false)
  ; "close_out", V.efunc (outfile  **->> V.unit)  close_out
  ; "open_in",   V.efunc (V.string **->> infile)  open_in
  ; "close_in",  V.efunc (infile   **->> V.unit)  close_in
  ; "writeto",   errchoose (writeto' false)
  ; "appendto",  errchoose (writeto' true)
  ; "remove",    errfunc (V.string **->> V.string) (succeed Sys.remove)
  ; "rename",    errfunc (V.string **-> V.string **->> V.string) (succeed2 Sys.rename)
  ; "tmpname",   V.efunc (V.unit **->> V.string) tmpname
  ; "read",      V.efunc (V.option V.string **->> V.option V.string) read
  ; "write",     errchoose 
                 [ V.alt (V.string *****->> V.int)   (* eta-expand to delay eval *)
                                               (fun l -> write_strings io.currentout l)
                 ; V.alt (outfile **-> V.string *****->> V.int) write_strings
                 ]                          
  ; "date",      V.efunc (V.option V.string **->> V.string) date 
  ; "exit",      V.efunc (V.option V.int **->> V.unit) (fun n -> exit (getopt n 0))
  ; "getenv",    V.efunc (V.string **->> V.option V.string)
                 (fun s -> try Some (Sys.getenv s) with Not_found -> None)
  ; "execute",   V.efunc (V.string **->> V.int) Sys.command
  ; "_STDIN",    infile.V.embed  stdin
  ; "_STDOUT",   outfile.V.embed stdout
  ; "_STDERR",   outfile.V.embed stderr
  ; "_INPUT",    infile.V.embed  io.currentin
  ; "_OUTPUT",   outfile.V.embed io.currentout
  ] in

         C.register_globals io_builtins g
     end (*M*)
  end (*Make*)
