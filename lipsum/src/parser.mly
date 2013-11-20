%{
module LP = Litprog
module P  = Parsing
module L  = Lexing
module T  = Tangle

let position p = 
        { T.file   = p.L.pos_fname
        ; T.line   = p.L.pos_lnum
        ; T.column = p.L.pos_cnum - p.L.pos_bol
        ; T.offset = p.L.pos_cnum
        }

%}

%start litprog
%type <Litprog.chunk list> litprog

%token EOF AT
%token <string> REF
%token <string> DEF
%token <Lexing.position * string> STR

%% /* rules below */

litprog     : /**/ chunks EOF               {List.rev $1}
            | STR  chunks EOF               {LP.Doc(snd $1) :: List.rev $2}
            ;

chunks      : chunks chunk                  {$2::$1}
            | /**/                          {[]}
            ;
            
chunk       : code                          {$1}
            | doc                           {$1}
            ;
            
doc         : AT STR ;                      {LP.Doc(snd $2)}

code        : DEF body ;                    {LP.Code($1, List.rev $2)}
            
body        : body STR                      {LP.Str(position (fst $2),snd $2)::$1}
            | body REF                      {LP.Ref($2)::$1}
            | /**/                          {[]}
            ;   
%%
