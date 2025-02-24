%{ open Ast %}

%token SEMI COLASN COMMA LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE BAR PRIME
%token NEG NOT
%token PLUS MINUS TIMES DIVIDE POW
%token EQ NEQ LT GT LEQ GEQ AND OR 
%token ASN DASN
%token BOOL INT FLOAT CHAR STRING INTARR FLOATARR VOID
%token FUNC EQUA
%token IF ELSE NOELSE FOR WHILE RETURN 
%token METER SEC KGRAM AMP CMETER HERTZ GRAM NEWTON 


/* literals */
%token <string> ID /* identifier for variable and function names */
%token <string> UNIT 
%token <int> INT_LITERAL
%token <string> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOL_LITERAL
%token EOF

/* precedence */
%nonassoc NOELSE
%nonassoc ELSE


%right ASN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left POW
%right NOT NEG

%nonassoc LPAREN LBRACK LBRACE
%nonassoc RPAREN RBRACK RBRACE


%start program
%type <Ast.program> program

%%

program:
   decls EOF { $1 }

decls:
   /* nothing */   { {globals=[]; udecls=[]; fdecls=[];} }
   | decls var_decl {{
                  globals = $2 :: $1.globals;
                  udecls = $1.udecls;
                  fdecls = $1.fdecls;
  					}}
   | decls unit_decl {{
                  globals = $1.globals;
                  udecls = $2 :: $1.udecls;
                  fdecls = $1.fdecls;
  					}}
   | decls func_decl {{
                  globals = $1.globals;
                  udecls = $1.udecls;
                  fdecls = $2 ::$1.fdecls;
  					}}       


/**************** unit declarations *******************/
   /* |'{mm} = 0.001 '{m}|; */
unit_decl:
   BAR UNIT ASN FLOAT_LITERAL UNIT BAR SEMI { ($2, $5, $4) } 


/*************** variable declarations ********************/
var_decl: 
   unit_typ UNIT ID SEMI { ($1, $2, $3) }
  |typ ID SEMI { ($1, "1", $2) }

typ:
   INT     { Int   }
  | FLOAT  { Float }  
  | STRING { String}
  | BOOL   { Bool  }
  | VOID   { Void  }
  | INTARR { IntArr }
  | FLOATARR { FloatArr }

unit_typ:
   FLOAT    { Float }  
 | FLOATARR { FloatArr }


/**************** func_decl *******************/
func_decl:
   unit_typ UNIT FUNC ID LPAREN opt_formals RPAREN LBRACE stmt_list RBRACE{{
      return_type       = $1;
      return_unit       = $2; 
		func_identifier   = $4;
		func_formals      = $6;
		func_stmts        = List.rev $9;
   }}
|  typ FUNC ID LPAREN opt_formals RPAREN LBRACE stmt_list RBRACE{{
      return_type       = $1;
      return_unit       = "1";
		func_identifier   = $3;
		func_formals      = $5;
		func_stmts        = List.rev $8;
   }}


opt_formals:
   /* nothing */ {[]}
   | formals_list {List.rev $1 }


formals_list:
    typ ID { [($1, "1", $2)] }
   | unit_typ UNIT ID { [($1, $2, $3)] }
   | formals_list COMMA unit_typ UNIT ID { ($3, $4, $5) :: $1 } 
   | formals_list COMMA typ ID { ($3, "1", $4) :: $1 } 



stmt_list:
   { [] }
   | stmt_list stmt { $2 :: $1 }

stmt:
   expr SEMI {Expr $1}
   | typ ID ASN expr SEMI                  { DAssign($1, "1", $2, $4) } 
   | unit_typ UNIT ID ASN expr SEMI                  { DAssign($1, $2, $3, $5) } 
   | RETURN expr_opt SEMI 				      {Return $2}
   | LBRACE stmt_list RBRACE              { Block(List.rev $2) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  	| IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9)   }
   | WHILE LPAREN expr RPAREN stmt           { While($3, $5)   }
   
expr_opt:
    /* nothing */ { Noexpr }
  	| expr          { $1 }

expr:
   INT_LITERAL 									{ IntLit($1) }
   | FLOAT_LITERAL 								{ FloatLit($1) }
	| STRING_LITERAL				            { StringLit($1) }
	| BOOL_LITERAL 									{ BoolLit($1) }
	| ID 														{ Id($1) }
	| expr PLUS expr 								{ Binop($1, Add, $3) }
	| expr MINUS expr 							{ Binop($1, Sub, $3) }
	| expr TIMES expr 							{ Binop($1, Mult, $3) }
	| expr DIVIDE expr 							{ Binop($1, Div, $3) }
	| expr POW expr 								{ Binop($1, Pow, $3) }
	| expr EQ expr 									{ Binop($1, Equal, $3) }
	| expr NEQ expr 								{ Binop($1, Neq, $3) }
	| expr LT expr 									{ Binop($1, Less, $3) }
	| expr LEQ expr									{ Binop($1, Leq, $3) }
	| expr GT expr 									{ Binop($1, Greater, $3) }
	| expr GEQ expr 								{ Binop($1, Geq, $3) }
	| expr AND expr 								{ Binop($1, And, $3) }
	| expr OR expr 									{ Binop($1, Or, $3) }
	| MINUS expr %prec NEG 							{ Unop(Neg, $2) }
	| NOT expr 										{ Unop(Not, $2) }
   | expr ASN expr 								{ Assign($1, $3) }
	| LPAREN expr RPAREN 							{ $2 }
   | ID LPAREN args RPAREN             { FunctionCall($1, $3) } 
   | LBRACK opt_lst RBRACK                   { Array($2)}
   | expr LBRACK expr RBRACK            { ArrayAccess($1, $3)}


opt_lst:
   {[]}
   | lst { List.rev $1 }

lst: 
     expr { [$1]}
   | lst COMMA expr { $3 :: $1 }


args:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
