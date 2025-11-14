%parameter<Ast : Luaast.S>

%start chunks
%type <Ast.chunk list> chunks chunklist

%type <Ast.lval> var
%type <Ast.stmt list> statlist ret

%% /* beginning of rules section */

chunks  : chunklist ret EOF { List.rev (List.map (fun s -> Ast.Statement s) $2 @ $1) }

chunklist : /* empty */        { [] }
	  | chunklist stat sc      { Ast.Statement $2    :: $1 }
	  | chunklist function_    { $2                :: $1 }
          | chunklist DEBUG_PRAGMA { Ast.Debug ($2 <> 0) :: $1 }
	  ;

function_     : FUNCTION funcname body  { $2 $3 ($symbolstartofs) };

funcname  : var             { fun (args, ss) w -> Ast.Fundef (w, $1, args, ss) }
	  | varexp COLON NAME { fun (args, ss) w -> Ast.Methdef (w, $1, $3, args, ss) }
          ;

body :  LPAR parlist RPAR block END { ($2, $4 ) } ;

statlist : /* empty */         { [] }
	 | statlist stat sc    { $1 @ [$2] }
	 ;

sc	 : /* empty */ { () } | SEMI { () } ;

stat   : stat_ { Ast.Stmt' ($symbolstartofs, $1) }
stat_  : IF expr1 THEN block elsepart END { let (a, e) = $5 in Ast.If ($2, $4, a, e) }
  /*
       | CASE expr1 OF case_body END
       | GLOBMATCH expr1 OF glob_body END
  */
       | WHILE  expr1 DO block END  { Ast.WhileDo ($2, $4) }
       | REPEAT block UNTIL expr1   { Ast.RepeatUntil ($2, $4) }
       | varlist1 GETS exprlist1     { Ast.Assign ($1, $3) }
       | functioncall               { Ast.Callstmt $1 }
       | LOCAL localdeclist decinit { Ast.Local ($2, $3) }

elsepart : /* empty */    { ([], None) }
	 | ELSE block     { ([], Some $2) }
         | ELSEIF expr1 THEN block elsepart { let (a, e) = $5 in (($2, $4)::a, e) }
block    :  statlist ret { $1 @ $2 } ;

ret	: /* empty */          { [] }
        | RETURN exprlist sc   { [Ast.Return $2] }
	;

expr1	 : expr { $1 } ;
				
expr :  LPAR expr RPAR  { $2 }
     |  expr1 EQ  expr1	{ Ast.Binop ($1, Ast.Eq, $3) }
     |	expr1 LT expr1	{ Ast.Binop ($1, Ast.Lt, $3) }
     |	expr1 GT expr1	{ Ast.Binop ($1, Ast.Gt, $3) }
     |	expr1 NE  expr1	{ Ast.Binop ($1, Ast.Ne, $3) }
     |	expr1 LE  expr1	{ Ast.Binop ($1, Ast.Le, $3) }
     |	expr1 GE  expr1	{ Ast.Binop ($1, Ast.Ge, $3) }
     |	expr1 PLUS expr1 { Ast.Binop ($1, Ast.Plus,  $3) }
     |	expr1 MINUS expr1 { Ast.Binop ($1, Ast.Minus, $3) }
     |	expr1 STAR expr1 { Ast.Binop ($1, Ast.Times, $3) }
     |	expr1 SLASH expr1 { Ast.Binop ($1, Ast.Div,   $3) }
     |  expr1 PERCENT expr1 { Ast.Binop ($1, Ast.Mod, $3) }
     |	expr1 HAT expr1 { Ast.Binop ($1, Ast.Pow,   $3) }
     |	expr1 CONC expr1 { Ast.Binop ($1, Ast.Concat, $3) }
     |	MINUS expr1 %prec UNARY	{ Ast.Unop (Ast.Minus, $2) }
     | table                    { $1 }
     |  varexp          { $1 }
     |  NUMBER          { Ast.Lit (Ast.Value.LuaValueBase.Number $1) }
     |  STRING          { Ast.Lit (Ast.Value.LuaValueBase.String $1) }
     |	NIL		{ Ast.Lit (Ast.Value.LuaValueBase.Nil)       }
     |  functioncall    { Ast.Call $1 }
     |	NOT expr1	{ Ast.Unop (Ast.Not, $2) }
     |	expr1 AND expr1 { Ast.Binop ($1, Ast.And, $3) }
     |	expr1 OR  expr1 { Ast.Binop ($1, Ast.Or,  $3) }
     ;

table : LBRA fieldlist RBRA { let (l, b) = $2 in Ast.Table (l, b) } ;

functioncall : funcvalue funcParams { $1 $2 } ;

funcvalue    : varexp { fun args -> Ast.Funcall ($1, args) }
	     | varexp COLON NAME  { fun args -> Ast.Methcall ($1, $3, args) }
	     ;

funcParams :	LPAR exprlist RPAR { $2 } 
  	|	table  { [$1] }
	;

exprlist  :	/* empty */		{ [] }
	  |	exprlist1		{ $1 }
	  ;
		
exprlist1 :  expr	{ [$1] }
	  |  exprlist1 COMMA expr { $1 @ [$3] }
	  ;

parlist   :	/* empty */ { ([], false) }
          |  DOTS           { ([], true) }
          |	parlist1 opt_dots   { ($1, $2) }
	  ;
		
parlist1 :	par		  { [$1] }
	  |	parlist1 COMMA par  { $1 @ [$3] }
	  ;

opt_dots  : /* empty */   { false }
          | COMMA  DOTS     { true  }

par : NAME	{ $1 }
    ;
		
fieldlist  : lfieldlist semicolonpart { ($1, $2) }
	   | ffieldlist1 lastcomma    { ([], $1) }
	   ;

semicolonpart : /* empty */    { [] }
	      | SEMI ffieldlist { $2 }
	      ;

lastcomma  : /* empty */   { () }
	   | COMMA           { () }
	   ;

ffieldlist  : /* empty */ { [] }
            | ffieldlist1 lastcomma { $1 }
            ;   

ffieldlist1 : ffield			{ [$1] }
	   | ffieldlist1 COMMA ffield	{ $1 @ [$3] }
ffield      : NAME GETS expr1  { ($1, $3) } ;

lfieldlist  : /* empty */ { [] }
	    | lfieldlist1 lastcomma { $1 }
	    ;

lfieldlist1 : expr1  {[$1]}
	    | lfieldlist1 COMMA expr1 { $1 @ [$3] }
            ;

varlist1  :	var  {[$1]}
	  |	varlist1 COMMA var	{ $1 @ [$3] }
	  ;
		
var	  :	singlevar { Ast.Lvar $1 }
	  |	varexp LSQ expr1 RSQ  { Ast.Lindex ($1, $3) }
	  |	varexp DOT NAME       { Ast.Lindex ($1, Ast.Lit (Ast.Value.LuaValueBase.String $3)) }
	  ;
		
singlevar :	NAME   { $1 }
	  ;

varexp	: var { match $1 with Ast.Lvar x -> Ast.Var x | Ast.Lindex (t, k) -> Ast.Index (t, k) }
	;
	  
localdeclist  : NAME { [$1] }
     	  | localdeclist COMMA NAME { $1 @ [$3] }
	  ;
		
decinit	  : /* empty */  { [] }
	  | GETS exprlist1 { $2 }
	  ;
	  
%%

