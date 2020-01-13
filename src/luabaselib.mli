module Add (MakeParser : Luaparser.MAKER) (I : Luainterp.S) : sig
  include Luainterp.S
  module Parser : Luaparser.S with type chunk = Ast.chunk
  val do_lexbuf : sourcename:string -> state -> Lexing.lexbuf -> value list
  val dostring  : ?file:string -> state -> string -> value list
  val dofile    : state -> string -> value list
  val mk        : unit -> state  (* builds state and runs startup code *)
end with module Value = I.Value
