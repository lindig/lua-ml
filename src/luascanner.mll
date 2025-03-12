{
    module P = Luaparser_tokens     (* tokens are defined here *)
    
    exception Scan of string
    
    let error msg   = raise (Scan msg)

    (* called at *every* newline in the source code *)
    let nl lexbuf map =
        let next = (Lexing.lexeme_start lexbuf) + 1     in
            Luasrcmap.nl map next
            
    let location lexbuf map =
        Luasrcmap.location map (Lexing.lexeme_start lexbuf)
         
    let get         = Lexing.lexeme
    let getchar     = Lexing.lexeme_char
    let strlen      = String.length
    let pos_start   = Lexing.lexeme_start
    let pos_end     = Lexing.lexeme_end

    let keywords    = Hashtbl.create 27
    let keyword s   = Hashtbl.find keywords s
    let _ = Array.iter (fun (str,tok) -> Hashtbl.add keywords str tok)
      [| ("and"         , P.AND)
      ;  ("case"        , P.CASE)
      ;  ("do"          , P.DO)
      ;  ("else"        , P.ELSE)
      ;  ("elseif"      , P.ELSEIF)
      ;  ("end"         , P.END)
      ;  ("function"    , P.FUNCTION)
      ;  ("globmatch"   , P.GLOBMATCH)
      ;  ("if"          , P.IF)
      ;  ("local"       , P.LOCAL)
      ;  ("nil"         , P.NIL)
      ;  ("not"         , P.NOT)
      ;  ("of"          , P.OF)
      ;  ("or"          , P.OR)
      ;  ("repeat"      , P.REPEAT)
      ;  ("return"      , P.RETURN)
      ;  ("then"        , P.THEN)
      ;  ("until"       , P.UNTIL)
      ;  ("while"       , P.WHILE) 
      |]

}


let digit       = ['0'-'9']
let alpha       = ['a'-'z' 'A'-'Z']
let misc        = ['_']

let sign        = ['+' '-']
let exp         = ['e''E'] sign? digit+

let number      = digit+ exp?
                | digit+ '.' digit+ exp?

let id          = (alpha | misc) (alpha | misc | digit)*
let ws          = [' ' '\t' '\r'] (* newline gets extra treatment! *)
let nl          = '\n'

(* map is a Luasrcmap.map value that is used to record all newlines
   such that every character position can be translated into
   a file/line/column triple *)

rule token = parse      (* raise Error in case of error *)
    eof         { fun _ ->  P.EOF }
  | ws+         { fun map ->  token lexbuf map }
  | nl          { fun map -> nl lexbuf map ; token lexbuf map }
  
  | nl '$'      { fun map -> (* skip pragma line *)
                  ( nl lexbuf map
                  ; skip lexbuf map
                  )
                }
  | '$'         { fun map -> (* skip pragma line *) 
                  if Lexing.lexeme_start lexbuf = 0 then 
                    skip lexbuf map     
                  else
                    error "illegal character '$' (a misplaced pragma not on its own line?)" 
                }
       
  | nl "$line"  ws+ { fun map -> line_pragma  lexbuf map }
  | nl "$file"  ws+ { fun map -> file_pragma  lexbuf map }  
  | nl "$debug" ws+ { fun map -> debug_pragma lexbuf map }
  
  | "$line" ws+ { fun map -> 
                  if Lexing.lexeme_start lexbuf = 0 then 
                    line_pragma lexbuf map 
                  else 
                    error "illegal $line pragma" 
                }
  | "$file" ws+ { fun map -> 
                  if Lexing.lexeme_start lexbuf = 0 then
                    file_pragma lexbuf map 
                  else 
                    error "illegal $file pragma"
                }  
       
  | "$debug" ws+ { fun map -> 
                   if Lexing.lexeme_start lexbuf = 0 then 
                     debug_pragma lexbuf map 
                   else 
                     error "illegal $debug pragma" 
                 }
  | '#'         { fun map ->  (* skip # line if it's the first line *)
                  if Lexing.lexeme_start lexbuf = 0 then 
                    skip lexbuf map     
                  else
                    error "illegal character '#'" 
                }
  | id          { fun _ ->  let s = get lexbuf in
                    try keyword s with Not_found -> P.NAME s
                }

  | number      { fun _ ->  
                  let s = get lexbuf in P.NUMBER (float_of_string s) 
                }
  
  | ".."        { fun _ ->  P.CONC  }
  | "..."       { fun _ ->  P.DOTS  }
  | "<="        { fun _ ->  P.LE    }
  | "=="        { fun _ ->  P.EQ    }
  | "=>"        { fun _ ->  P.ARROW }
  | ">="        { fun _ ->  P.GE    }
  | "~="        { fun _ ->  P.NE    }
  | '('         { fun _ ->  P.LPAR  }
  | ')'         { fun _ ->  P.RPAR  }
  | '*'         { fun _ ->  P.STAR  }
  | '+'         { fun _ ->  P.PLUS  }
  | ','         { fun _ ->  P.COMMA }
  | '-'         { fun _ ->  P.MINUS }
  | '.'         { fun _ ->  P.DOT   }
  | '/'         { fun _ ->  P.SLASH }
  | '%'         { fun _ ->  P.PERCENT }
  | ':'         { fun _ ->  P.COLON }
  | ';'         { fun _ ->  P.SEMI  }
  | '<'         { fun _ ->  P.LT    }
  | '='         { fun _ ->  P.GETS  }
  | '>'         { fun _ ->  P.GT    }
  | '['         { fun _ ->  P.LSQ   }
  | ']'         { fun _ ->  P.RSQ   }
  | '^'         { fun _ ->  P.HAT   }
  | '{'         { fun _ ->  P.LBRA  }
  | '}'         { fun _ ->  P.RBRA  }

 (* this token is defined in the LUA lex.c file but is not used
  | '~'         { fun map ->  P.TILDE }   
  *)

  | "--[["
                { fun map ->  longcomment lexbuf map }
  | "--"
                { fun map ->  shortcomment lexbuf map }
  | '\''        { fun map ->  shortstring lexbuf map "'" (Buffer.create 80) }
  | '"'         { fun map ->  shortstring lexbuf map "\"" (Buffer.create 80) }
  | "[["        { fun map ->  longstring  lexbuf 1 map (Buffer.create 160) }

  | _           { fun _ ->  error     
                                ( Printf.sprintf 
                                  "illegal character `%s' at character %d" 
                                  (Char.escaped (Lexing.lexeme_char lexbuf 0))
                                  (Lexing.lexeme_start lexbuf)
                                )  
                }

and shortcomment =
  parse
  | eof
                { fun _ ->  P.EOF }
  | nl
                { fun map -> nl lexbuf map; token lexbuf map }
  | _
                { fun map -> shortcomment lexbuf map }

and longcomment =
  parse
  | eof
                { fun _ -> error "end of file in a --[[ ... ]] comment" }
  | "]]"
                { fun map -> token lexbuf map }
  | nl
                { fun map -> nl lexbuf map; longcomment lexbuf map }
  | _
                { fun map -> longcomment lexbuf map }

and skip = parse        (* skip to end of line *)
    eof         { fun _ ->  P.EOF        }
  | [^'\n']+    { fun map ->  skip lexbuf map }
  | nl          { fun map ->  nl lexbuf map ; token lexbuf map }
  | _           { fun _ ->  error     
                                ( Printf.sprintf 
                                  "illegal character `%s' at character %d" 
                                  (Char.escaped (Lexing.lexeme_char lexbuf 0))
                                  (Lexing.lexeme_start lexbuf)
                                )  
                }

and file_pragma = parse
    eof         { fun _ -> error "illegal $file pragma" }
  | [^'\n']+    { fun map -> 
                    let file = get lexbuf in
                    let loc  = (file, 1, 1) in
                    let pos  = Lexing.lexeme_start lexbuf in 
                        ( Luasrcmap.sync map pos loc
                        ; token lexbuf map
                        )
                }
  | _           { fun _ ->  error     
                                ( Printf.sprintf 
                                  "illegal character `%s' at character %d" 
                                  (Char.escaped (Lexing.lexeme_char lexbuf 0))
                                  (Lexing.lexeme_start lexbuf)
                                )  
                }
and line_pragma = parse
    eof         { fun _ -> error "illegal $line pragma" }
  | digit+      { fun map -> 
                    let s       = get lexbuf in
                    let line    = int_of_string s in
                    let line    = line - 1 in   (* fencepost error *)
                    let pos     = Lexing.lexeme_start lexbuf in 
                    let (f,_l,_) = Luasrcmap.last map in
                        ( Luasrcmap.sync map pos (f,line,1)
                        ; token lexbuf map
                        )
                }        
  | _           { fun _ ->  error     
                                ( Printf.sprintf 
                                  "illegal character `%s' at character %d" 
                                  (Char.escaped (Lexing.lexeme_char lexbuf 0))
                                  (Lexing.lexeme_start lexbuf)
                                )  
                }



and debug_pragma = parse
    eof         { fun _ -> error "illegal $debug pragma" }
  | digit+      { fun _ -> 
                    let s       = get lexbuf in
                    let debug   = int_of_string s in
                    P.DEBUG_PRAGMA debug
                }        
  | _           { fun _ ->  error     
                                ( Printf.sprintf 
                                  "illegal character `%s' at character %d" 
                                  (Char.escaped (Lexing.lexeme_char lexbuf 0))
                                  (Lexing.lexeme_start lexbuf)
                                )  
                }



and longstring = parse  (* parse a [[ .. ]] string *)
    eof         { fun _ _ _ -> error "end of file in [[..]] string" }
  | "]]"        { fun n map buf ->
                    if n = 1 then P.STRING (Buffer.contents buf) 
                    else ( Buffer.add_string buf "]]"
                         ; longstring lexbuf (n-1) map buf
                         )
                }
  | "[["        { fun n map buf ->
                    ( Buffer.add_string buf "[["
                    ; longstring lexbuf (n+1) map buf
                    )
                }
  | [^']' '[' '\n']+
  | ']' 
  | '['         { fun n map buf -> 
                  let s = get lexbuf in
                  ( Buffer.add_string buf s
                  ; longstring lexbuf n map buf
                  )
                }
  | nl          { fun n map buf -> 
                   ( Buffer.add_char buf '\n'
                   ; nl lexbuf map
                   ; longstring lexbuf n map buf
                   )
                }  
  | _           { fun _ _ _ ->  error     
                                ( Printf.sprintf 
                                  "illegal character `%s' at character %d" 
                                  (Char.escaped (Lexing.lexeme_char lexbuf 0))
                                  (Lexing.lexeme_start lexbuf)
                                )  
                }

and shortstring = parse (* parse an eos delimited string *)
    eof         { fun _ _ buf -> 
                  error ("end of file in string: " ^ Buffer.contents buf) } 
  | '\n'        { fun _ _ buf ->
                  error ("end of line in string: " ^ Buffer.contents buf) }
  | '\\' _      { fun map eos buf -> 
                  let c = getchar lexbuf 1 in
                  let k = match c with
                      | 'n'  -> '\n'
                      | 't'  -> '\t'
                      | 'r'  -> '\r'
                      | '\n' -> '\n'
                      | '0'  -> '\000'
                      | _    -> c
                  in   
                     ( Buffer.add_char buf k
                     ; shortstring lexbuf map eos buf
                     )
                 }
  | [^'"' '\'' '\n' '\\']+  
                 { fun map eos buf -> 
                   let s = get lexbuf  in
                     ( Buffer.add_string buf s
                     ; shortstring lexbuf map eos buf
                     )
                 }
  | ['"' '\'' ]  { fun map eos buf ->
                   let s = get lexbuf in
                      if s = eos then
                        P.STRING (Buffer.contents buf)
                      else
                        ( Buffer.add_string buf s
                        ; shortstring lexbuf map eos buf
                        )
                  }
  | _             { fun _ _ _ -> assert false }




{
        let tok2str = function
          | P.AND         -> "AND"
          | P.ARROW       -> "ARROW"
          | P.CASE        -> "CASE"
          | P.COLON       -> "COLON"
          | P.COMMA       -> "COMMA"
          | P.CONC        -> "CONC"
          | P.DEBUG_PRAGMA n-> "DEBUG_PRAGMA" ^ (string_of_int n)
          | P.DO          -> "DO"
          | P.DOT         -> "DOT"
          | P.DOTS        -> "DOTS"
          | P.ELSE        -> "ELSE"
          | P.ELSEIF      -> "ELSEIF"
          | P.END         -> "END"
          | P.EOF         -> "EOF"
          | P.EQ          -> "EQ"
          | P.FUNCTION    -> "FUNCTION"
          | P.GE          -> "GE"
          | P.GETS        -> "GETS"
          | P.GLOBMATCH   -> "GLOBMATCH"
          | P.GT          -> "GT"
          | P.HAT         -> "HAT"
          | P.IF          -> "IF"
          | P.LBRA        -> "LBRA"
          | P.LE          -> "LE"
          | P.LOCAL       -> "LOCAL"
          | P.LPAR        -> "LPAR"
          | P.LSQ         -> "LSQ"
          | P.LT          -> "LT"
          | P.MINUS       -> "MINUS"
          | P.NAME x      -> "NAME("^x^")"  
          | P.NE          -> "NE"
          | P.NIL         -> "NIL"
          | P.NOT         -> "NOT"
          | P.NUMBER _    -> "NUMBER" 
          | P.OF          -> "OF"
          | P.OR          -> "OR"
          | P.PLUS        -> "PLUS"
          | P.RBRA        -> "RBRA"
          | P.REPEAT      -> "REPEAT"
          | P.RETURN      -> "RETURN"
          | P.RPAR        -> "RPAR"
          | P.RSQ         -> "RSQ"
          | P.SEMI        -> "SEMI"
          | P.SLASH       -> "SLASH"
          | P.PERCENT     -> "PERCENT"
          | P.STAR        -> "STAR"
          | P.STRING x    -> "STRING("^x^")" 
          | P.THEN        -> "THEN"
          | P.UNARY       -> "UNARY"
          | P.UNTIL       -> "UNTIL"
          | P.WHILE       -> "WHILE"
          | P.WRONGTOKEN  -> "WRONGTOKEN"
    }          
