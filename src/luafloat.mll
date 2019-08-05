let digit       = ['0'-'9']
let sign        = ['+' '-']
let exp         = ['e''E'] sign? digit+
let number      = sign? digit+ exp?
                | sign? digit+ '.' digit+ exp?
rule length = parse number { Lexing.lexeme_end lexbuf } | _ { -1 }
