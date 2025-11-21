module type S = sig
  type chunk
  val chunks : (Lexing.lexbuf  -> Luaparser_tokens.token) -> Lexing.lexbuf -> chunk list
  exception Error
end

module type MAKER = functor (Ast : Luaast.S) -> S with type chunk = Ast.chunk
  
module MakeStandard (Ast : Luaast.S) = struct
  type chunk = Ast.chunk

  module P = Luaparser_impl.Make(Ast)

  let chunks = P.chunks
  exception Error = P.Error
end
