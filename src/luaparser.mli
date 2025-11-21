module type S = sig
  type chunk
  val chunks : (Lexing.lexbuf  -> Luaparser_tokens.token) -> Lexing.lexbuf -> chunk list
  exception Error
end
module type MAKER = functor (Ast : Luaast.S) -> S with type chunk = Ast.chunk
module MakeStandard : MAKER
