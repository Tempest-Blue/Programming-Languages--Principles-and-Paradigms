%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token EOF
%token <string> Id
%token TRUE
%token FALSE
%token LET
%token REC
%token EQ
%token IN
%token FUN
%token ARROW
%token IF
%token THEN
%token ELSE
%token PLUS 
%token MINUS
%token MUL
%token DIV
%token LT 
%token LE 
%token NE
%token AND
%token OR
%token LPAREN
%token RPAREN
%token LBRAC
%token RBRAC
%token SEMI
%token COLONCOLON

%left OR
%left AND 
%left EQ NE LT LE
%right COLONCOLON SEMI RBRAC
%left PLUS MINUS
%left MUL DIV
%left FUN
%nonassoc Id Num
%nonassoc PRE

%start exp 
%type <Nano.expr> exp

%%

exp:
	| Num 			             {Const $1}
	| Id 						 {Var $1}
	| TRUE 						 {True}
	| FALSE						 {False}
	| LET Id EQ exp IN exp 		 {Let($2,$4,$6)}
	| LET REC Id EQ exp IN exp   {Letrec($3,$5,$7)}
	| FUN Id ARROW exp 			 {Fun($2,$4)}
	| IF exp THEN exp ELSE exp	 {If($2,$4,$6)}
	| exp PLUS exp			 	 {Bin($1,Plus,$3)}
	| exp MINUS exp			 	 {Bin($1,Minus,$3)}
	| exp MUL exp				 {Bin($1,Mul,$3)}
	| exp DIV exp				 {Bin($1,Div,$3)}
	| exp LT exp 				 {Bin($1,Lt,$3)}
	| exp LE exp 				 {Bin($1,Le,$3)}
	| exp NE exp 				 {Bin($1,Ne,$3)}
	| exp EQ exp 				 {Bin($1,Eq,$3)}
	| exp AND exp 			 	 {Bin($1,And,$3)}
	| exp OR exp 				 {Bin($1,Or,$3)}
	| LPAREN exp RPAREN			 {$2}
	| exp exp %prec PRE 		 {App($1,$2)}
	| exp SEMI exp 				 {Bin($1,Cons,$3)}
	| exp COLONCOLON exp 		 {Bin($1,Cons,$3)}
	| LBRAC RBRAC				 {NilExpr}
	| LBRAC exp 				 {$2}
	| exp RBRAC					 {Bin($1,Cons,NilExpr)}