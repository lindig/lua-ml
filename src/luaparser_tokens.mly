%token EOF
%token COMMA LBRA RBRA LT GT COLON SEMI HAT STAR
%token SLASH PERCENT LPAR RPAR GETS PLUS MINUS DOT LSQ RSQ

%token WRONGTOKEN
%token NIL
%token IF THEN ELSE ELSEIF WHILE DO REPEAT UNTIL CASE GLOBMATCH OF END
%token RETURN
%token LOCAL
%token FUNCTION
%token DOTS
%token ARROW
%token <float> NUMBER
%token <string>  STRING
%token <string>  NAME

%token <int> DEBUG_PRAGMA

%token AND OR
%token EQ NE LE GE
%token CONC
%token UNARY NOT


%left AND OR
%left EQ NE GT LT LE GE
%left CONC
%left PLUS MINUS
%left STAR SLASH PERCENT
%left UNARY NOT
%right HAT

%%
