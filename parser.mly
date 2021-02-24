%{ open Ast %}

/* punctuations, unary operators, binary arithmetic, binary relational + logical, assignment 
   data type, declaration, statement, library function */
%token SEMI COLASN COMMA LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE BAR PRIME
%token NEG NOT
%token PLUS MINUS TIMES DIVIDE POW 
%token EQ NEQ LT GT LEQ GEQ TRUE FALSE AND OR 
%token ASN 
%token BOOL INT FLOAT CHAR STRING INTARR FLOATARR
%token FUNC EQUA
%token IF ELSE NOELSE FOR RETURN 
%token TYPEOF PRINT INT2FLOAT FLOAT2INT CEIL FLOOR

%token <string> ID
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOL_LITERAL
%token EOF

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


stmt:
	  expr SEMI 							  {Expr $1}
	| RETURN expr SEMI 						  {Return $2}
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  	| IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                      	                      { For($3, $5, $7, $9)   }
												

typ:
    INT    { Int   }
  | FLOAT  { Float }  
  | CHAR   { Char  }
  | STRING { String}  
  | BOOL   { Bool  }



unit:
	



expr:
	  INT_LITERAL 									{ Lit(IntLit($1)) }
	| FLOAT_LITERAL 								{ Lit(FloatLit($1)) }
	| CHAR_LITERAL 									{ Lit(CharLit($1)) }
	| STRING_LITERAL 								{ Lit(StringLit($1)) }
	| BOOL_LITERAL 									{ Lit(BoolLit($1)) }
	| TRUE 													{ BoolLit(true) }
	| FALSE 												{ BoolLit(false) }
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
	| MINUS expr %prec NEG 					{ Unop(Neg, $2) }
	| NOT expr 											{ Unop(Not, $2) }
	| ID ASSIGN expr 								{ Assign($1, $3) }
	| LPAREN expr RPAREN 						{ $2 }
	| ID LPAREN actuals_opt RPAREN 	{ Call($1, $3) }
	| 

















