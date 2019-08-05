type token =
  | EOF
  | COMMA
  | LBRA
  | RBRA
  | LT
  | GT
  | COLON
  | SEMI
  | HAT
  | STAR
  | SLASH
  | LPAR
  | RPAR
  | GETS
  | PLUS
  | MINUS
  | DOT
  | LSQ
  | RSQ
  | WRONGTOKEN
  | NIL
  | IF
  | THEN
  | ELSE
  | ELSEIF
  | WHILE
  | DO
  | REPEAT
  | UNTIL
  | CASE
  | GLOBMATCH
  | OF
  | END
  | RETURN
  | LOCAL
  | FUNCTION
  | DOTS
  | ARROW
  | NUMBER of (float)
  | STRING of (string)
  | NAME of (string)
  | DEBUG_PRAGMA of (int)
  | AND
  | OR
  | EQ
  | NE
  | LE
  | GE
  | CONC
  | UNARY
  | NOT

module type S = sig
  type chunk
  val chunks : (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> chunk list
end
module type MAKER = functor (Ast : Luaast.S) -> S with type chunk = Ast.chunk
module MakeStandard : MAKER
