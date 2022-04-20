module type S = sig
  module Value : Luavalue.S
  type value = Value.value
  type name = string

type varargs = bool
type funcargs = name list * varargs
type location = int (* character position *)
type stmt =
  | Stmt'       of location * stmt
  | Assign      of lval list * exp list
  | WhileDo     of exp * block
  | RepeatUntil of block * exp
  | If          of exp * block * (exp * block) list * block option
  | Return      of exp list
  | Callstmt    of call
  | Local       of name list * exp list
and block = stmt list
and lval =
  | Lvar   of name
  | Lindex of exp * exp
and exp =
  | Var   of name
  | Lit   of value
  | Binop of exp * op * exp
  | Unop  of op * exp
  | Index of exp * exp
  | Table of exp list * (name * exp) list
  | Call  of call
and call =
  | Funcall  of exp * exp list
  | Methcall of exp * name * exp list
and op = And | Or | Lt | Le | Gt | Ge | Eq | Ne | Concat
       | Plus | Minus | Times | Div | Mod | Not | Pow

type chunk =
  | Debug     of bool                   (* turn debugging on/off *)
  | Statement of stmt
  | Fundef    of location * lval       * funcargs * block
  | Methdef   of location * exp * name * funcargs * block

end

module Make (V : Luavalue.S) : S with module Value = V
