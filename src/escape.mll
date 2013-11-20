{ 
    module L = Lexing
    
    (* col0 is true, iff a match starts at the beginning of a line *)
    let col0 lexbuf = 
        let p = lexbuf.L.lex_start_p
        in
            p.L.pos_cnum = p.L.pos_bol
}

rule escape io = parse
      eof                       { () }
    | "@<<"                     { output_string io "@@<<" ; escape io lexbuf }
    | "<<"                      { output_string io "@<<"  ; escape io lexbuf }
    | "@"                       { if col0 lexbuf                   
                                  then  ( output_string io "@@"
                                        ; escape io lexbuf
                                        )
                                  else  ( output_char io '@'
                                        ; escape io lexbuf         
                                        )                          
                                } 
    | '\n'                      { Lexing.new_line lexbuf
                                ; output_char io '\n'
                                ; escape io lexbuf
                                }                                
    | _                         { output_char io (L.lexeme_char lexbuf 0)
                                ; escape io lexbuf                 
                                }                                  
{
    
}


