(** This is the main module that evaluates the command line and drives
    the program. *)

module S  = Scanner
module P  = Parser
module LP = Litprog
module T  = Tangle
module RX = Re      (** regular expression   *)
module G  = Re_glob (** shell-style globbing *)

(** Establish short names for modules; this avoids opening them *)

exception Error of string
let error fmt = Printf.kprintf (fun msg -> raise (Error msg)) fmt
let eprintf   = Printf.eprintf
let printf    = Printf.printf
let giturl    = "https://github.com/lindig/lipsum.git"


let (@@) f x = f x

let copyright () =
    List.iter print_endline
    [ giturl
    ; "Copyright (c) 2012, 2013, Christian Lindig <lindig@gmail.com>"
    ; "All rights reserved."
    ; ""
    ; "Redistribution and use in source and binary forms, with or"
    ; "without modification, are permitted provided that the following"
    ; "conditions are met:"
    ; ""
    ; "(1) Redistributions of source code must retain the above copyright"
    ; "    notice, this list of conditions and the following disclaimer."
    ; "(2) Redistributions in binary form must reproduce the above copyright"
    ; "    notice, this list of conditions and the following disclaimer in"
    ; "    the documentation and/or other materials provided with the"
    ; "    distribution."
    ; ""
    ; "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND"
    ; "CONTRIBUTORS \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES,"
    ; "INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF"
    ; "MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE"
    ; "DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR"
    ; "CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,"
    ; "SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT"
    ; "LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF"
    ; "USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED"
    ; "AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT"
    ; "LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN"
    ; "ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE"
    ; "POSSIBILITY OF SUCH DAMAGE."
    ; ""
    ; "This program includes a library for regular expressions that"
    ; "is available from https://github.com/ocaml/ocaml-re.git."
    ; "It was released under the GNU LESSER GENERAL PUBLIC LICENSE"
    ; "and was written by Jerome Vouillon "
    ; "<Jerome.Vouillon@pps.univ-paris-diderot.fr>"
    ]


(** finally calls a function and makes sure that a cleanup function 
    is called before return even in the presence of exceptions. However,
    exceptions in the cleanup function are not caught *)
type 'a result = Success of 'a | Failed of exn
let finally: ('a -> 'b) -> 'a -> ('a -> unit) -> 'b = fun f x cleanup ->
    let result =
        try Success (f x) with exn -> Failed exn
    in
        ignore @@ cleanup x; 
        match result with
        | Success y  -> y 
        | Failed exn -> raise exn

(** Attach a file name to the input source that we are reading. This is
    most useful when we are reading from stdin and no file name
    was attached *)
let set_filename (fname:string) (lexbuf:Lexing.lexbuf)  =
    ( lexbuf.Lexing.lex_curr_p <-  
        { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname }
    ; lexbuf
    )

(** Open a named file (or stdin), setup a lexer and call f on the lexer for
    the result. If a file was opened, it is closed before the result 
    is returned *)
let apply (f: Lexing.lexbuf -> 'a) = function
    | None      -> f @@ set_filename "stdin" @@ Lexing.from_channel stdin 
    | Some path -> 
        let io      = open_in path in
        let close _ = close_in io in
        let lexbuf  = set_filename path @@ Lexing.from_channel io in
            finally f lexbuf close
            
let scan lexbuf =
    let rec loop lexbuf =
        match S.token' lexbuf with
        | P.EOF  -> print_endline @@ S.to_string P.EOF
        | tok  -> ( print_endline @@ S.to_string tok
                  ; loop lexbuf
                  )
    in
        loop lexbuf

let escape lexbuf   = Escape.escape stdout lexbuf
(** read input and emit it again, escaping characters where needed
    to turn this into a code chunk in a literate program *)

let litprog lexbuf  = LP.make @@ P.litprog S.token' lexbuf
(** create a literate program by parsing the input *)

let parse lexbuf    = LP.print @@ litprog lexbuf
(** emit a literate program for debugging *)

(** expand chunk from litprog into a file named like chunk, using format *)
let tangle_to_file (litprog:LP.t) (format:Tangle.t) (chunk:string) = 
    finally (fun io -> LP.tangle litprog format io chunk) 
            (open_out chunk) close_out

let tangle_roots fmt lexbuf =
    let lp    = litprog lexbuf   in
    let fmt   = T.lookup fmt     in
    let roots = LP.code_roots lp in
        List.iter (tangle_to_file lp fmt) roots

let compile glob =
    try
        RX.compile @@ RX.whole_string @@ G.globx glob 
    with
        G.Parse_error -> error "syntax error in pattern '%s'" glob

let tangle_matching_roots fmt glob lexbuf =
    (** only expand roots matching glob *)
    let rx    = compile glob in (* rx can be used for matching a string *)
    let lp    = litprog lexbuf in
    let fmt   = T.lookup fmt in
    let roots = List.filter (RX.execp rx) @@ LP.code_roots lp in
        List.iter (tangle_to_file lp fmt) roots

let tangle fmt chunk lexbuf =
    LP.tangle (litprog lexbuf) (T.lookup fmt) stdout chunk

let weave lexbuf =
    (Weave.lookup "plain") stdout @@ LP.doc @@ litprog lexbuf

let chunks lexbuf =
    List.iter print_endline @@ LP.code_chunks @@ litprog lexbuf

let roots lexbuf =
    List.iter print_endline @@ LP.code_roots @@ litprog lexbuf

let check lexbuf =
    List.iter print_endline @@ LP.unknown_references @@ litprog lexbuf

let help io =
    let print_endline s = (output_string io s; output_char io '\n') in
    let this = "lipsum"  in
    let spac = "      "  in
    List.iter print_endline 
    [ this^" is a utility for literate programming"
    ; ""
    ; this^" help                       emit help to stdout"
    ; this^" roots [file.lp]            list root chunks"
    ; this^" chunks [file.lp]           list all chunks"
    ; this^" check [file.lp]            emit undefined code chunks"
    ; this^" tangle [-f fmt] file.c [file.lp]   extract file.c from file.lp"
    ; this^" expand -f fmt glob [file.lp]"  
    ; spac^"                            extract all root chunks matching" 
    ; spac^"                            pattern glob to individual files"
    ; this^" tangle -f                  show tangle formats available"
    ; this^" prepare [file]             prepare file to be used as chunk"
    ; this^" copyright                  display copyright notice"
    ; ""
    ; "See the manual lipsum(1) for documentation."
    ; ""
    ; "Debugging commands:"
    ; this^" scan [file.lp]             tokenize file and emit tokens"
    ; this^" parse [file.lp]            parse file and emit it"
    ; ""
    ; giturl
    ; "Copyright (c) 2012, 2013 Christian Lindig <lindig@gmail.com>"
    ]

let tangle_formats () = print_endline @@ String.concat " " T.formats

let optional = function (** We expect zero or one file names *) 
    | []        -> None 
    | [path]    -> Some path 
    | args      -> error "expected a single file name but found %d" 
                        (List.length args) 

let plain = tangle "plain"


let tangle_cmd = function
    | "-f"::fmt::chunk::paths -> apply (tangle fmt chunk) (optional paths)
    | "-f"::[]                -> tangle_formats ()
    | chunk::paths            -> apply (plain chunk) (optional paths)
    | _                       -> help stderr; exit 1

let expand_cmd = function
    | "-f"::fmt::glob::paths  -> apply (tangle_matching_roots fmt glob) 
                                        (optional paths)
    | _                       -> help stderr; exit 1
      
let main () =
    let argv    = Array.to_list Sys.argv in
    let args    = List.tl argv in
        match args with
        | "scan" ::args     -> apply scan  @@ optional args
        | "parse"::args     -> apply parse @@ optional args
        | "tangle" :: args  -> tangle_cmd args
        | "expand" :: args  -> expand_cmd args
        | "chunks"::args    -> apply chunks @@ optional args
        | "roots"::args     -> apply roots @@ optional args
        | "prepare"::args   -> apply escape @@ optional args
        | "weave"::args     -> apply weave @@ optional args        
        | "check"::args     -> apply check @@ optional args        
        | "help"::_         -> help stdout; exit 0
        | "-help"::_        -> help stdout; exit 0
        | "copyright"::_    -> copyright (); exit 0
        | []                -> help stderr; exit 1
        | _                 -> help stderr; exit 1


let () = 
    try 
        main (); exit 0
    with 
    | Error(msg)         -> eprintf "error: %s\n" msg; exit 1
    | Failure(msg)       -> eprintf "error: %s\n" msg; exit 1
    | Scanner.Error(msg) -> eprintf "error: %s\n" msg; exit 1
    | Sys_error(msg)     -> eprintf "error: %s\n" msg; exit 1
    | T.NoSuchFormat(s)  -> eprintf "unknown tangle format %s\n" s; exit 1
    | LP.NoSuchChunk(msg)-> eprintf "no such chunk: %s\n" msg; exit 1
    | LP.Cycle(s)        -> eprintf "chunk <<%s>> is part of a cycle\n" s;
                            exit 1
    (*
    | _                  -> Printf.eprintf "some unknown error occurred\n"; 
                            exit 1  
    *)
