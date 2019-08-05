module type VALUE = Luavalue.S
module type USERDATA = Luavalue.USERDATA

module Lib = Lualib
module Parser = Luaparser
module type AST = Luaast.S

module type EVALUATOR = Luainterp.S
module type INTERP = sig
  include EVALUATOR
  module Parser : Luaparser.S with type chunk = Ast.chunk
  val do_lexbuf : sourcename:string -> state -> Lexing.lexbuf -> value list
  val dostring  : state -> string -> value list
  val dofile    : state -> string -> value list
  val mk        : unit -> state
end
module Run (I : INTERP) = Luarun.Make (I)
module MakeEval = Luainterp.Make
module MakeInterp = Luabaselib.Add

module Empty = Lualib.Empty

let scanner map buf = Luascanner.token buf map
