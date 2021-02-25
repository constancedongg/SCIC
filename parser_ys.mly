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
%token METER SEC KGRAM AMP CMETER HERTZ GRAM NEWTON NOUNIT

/* literals */
%token <string> ID /* identifier for variable and function names */
%token <string> UID 
%token <string> UONE // ?
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
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


/* program: program declaration */
program: decls EOF { $1 }


// /* declarations: variable, function */
// decls:
//     /* nothing */               { ([], []) }
//   | decls var_decl { (($2 :: fst $1), snd $1) }
//   | decls func_decl { (fst $1, ($2 :: snd $1)) }


/* declarations: variable, function, unit */
decls:
    /* nothing */   { {vars=[]; units=[]; funcs=[]; equas=[];} }
  | decls var_decl  {{
						vars = $2 :: $1.vars;
						units = $1.units;
						funcs = $1.funcs;
						equas = $1.equas;
					}}
  | decls unit_decl {{
	  					vars = $1.vars;
						units = $2 :: $1.units;
						funcs = $1.funcs;
						equas = $1.equas;
  					}}
  | decls func_decl {{
						vars = $1.vars;
						units = $1.units;
						funcs = $2 :: $1.funcs;
						equas = $1.equas;
					}}
  | decls equa_decl {{
						vars = $1.vars;
						units = $1.units;
						funcs = $1.funcs;
						equas = $2 :: $1.equas;
  					}}


/* variable declaration: <type> <unit> <variable_name>; */
var_decl: 
	 typ unit ID SEMI 	{ ($1, $2, $3) }
	| typ ID SEMI 		{ ($1, NOUNIT, $2) } // ?



/***** function declaration *****/

/* <type> <unit> <function_name> (<arg1>, <arg2>, ...) { <statements> } */
func_decl: 
	 typ unit FUNC ID formals_block_unit stmt_block 
	{{
		return_type       = $1;
		return_unit       = $2;
		func_identifier = $4;
		func_formals    = List.rev $5;
		func_stmts = List.rev $6;
	}}
	| typ FUNC ID formals_block_no_unit stmt_block // split to two cases
	{{
		return_type       = $1;
		return_unit		  = NOUNIT;
		func_identifier = $3;
		func_formals    = List.rev $4;
		func_stmts = List.rev $5;
	}}

/***** equation declaration *****/
// need further discussion
equa_decl:	
	 EQUA ID formals_block_unit LBRACE stmt RBRACE 
	{{
		equa_identifier = $2;
		equa_formals = List.rev $3;
		equa_stmt = $5; 
	}} // equa_stmt should be more restricted than a single stmt
	| EQUA ID formals_block_no_unit LBRACE stmt RBRACE 
	{{
		equa_identifier = $2;
		equa_formals = List.rev $3;
		equa_stmt = $5; 
	}}







/* formals Block */
formals_block_unit: LPAREN opt_formals_unit RPAREN { $2 }

formals_block_no_unit: LPAREN opt_formals_no_unit RPAREN { $2 }


/* opt_formals: optional formals list in a function declaration.
 				either all with units or none with unit */
opt_formals_unit: /* nothing */ {[]}
			| formals_list_unit { List.rev $1 }

opt_formals_no_unit:
			/* nothing */ {[]}
			| formals_list_no_unit { List.rev $1 }



/* formal_list: a list of optional formal names in a function declaration */
formals_list_unit: 
		 typ unit ID             { [($1, $2, $3)] }
        | formals_list_unit COMMA typ unit ID  { ($3, $4, $5) :: $1 }


formals_list_no_unit:
		 typ ID             { [($1, NOUNIT, $2)] }
		| formals_list_no_unit COMMA typ ID 	  { ($3, NOUNIT, $4)} /* no unit - how to enforce none has unit? */



/* stmt_block: a statement block contained within a function, class, loop, or conditional.
delimited by an indent and dedent block introduced by the indentation parser in coral.ml */
stmt_block: 
   LBRACE stmt_list RBRACE { Block(List.rev $2) }


/* stmt_list: a list of statements in the global scope or a function body. modified from Micro C. */
stmt_list: 
   { [] }
  | stmt_list stmt { $2 :: $1 }


stmt:
	  expr SEMI 							  {Expr $1}
	| RETURN expr_opt SEMI 				      {Return $2}
	| LBRACE stmt_list RBRACE 				  {Block(List.rev $2)   }
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  	| IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                      	                      { For($3, $5, $7, $9)   }

expr_opt:
    /* nothing */ { Noexpr }
  	| expr          { $1 }




/*****  unit declaration *****/
/* | '{ new uid } = cexpr * '{ uid expression } | */
unit_decl:
	 BAR PRIME LBRACE UID RBRACE ASN cexpr TIMES unit BAR {{
		uid = $4;
		c_expr = $7;
		u_expr = $9; }}


/* built-in units are also valid uid's */
bi_unit:
	 METER		{ Meter } /* base */
	| SEC    	{ Second }
	| KGRAM     { Kilogram }
	| AMP      	{ Ampere }
	| HERTZ  	{ Hertz } /* derived */
	| CMETER    { Centimeter }
	| GRAM      { Gram }
	| NEWTON    { Newton }

/* <unit> in <type> <unit> <id> for a variable */
unit:
	 PRIME LBRACE bi_unit RBRACE { $3 }
	| PRIME LBRACE uexpr RBRACE { $3 } 


/* unit expression (without constant etc. ) */
uexpr:
	 UONE 							{ $1 }  // ??  |'{a} = '{1}|
	| UID 							{ Uid($1) } // |'{cm} = 0.01 * '{m} |
	| bi_unit						{ Uid($1) }
	| uexpr TIMES uexpr 			{ Binop($1, UMult, $3) }
	| uexpr DIVIDE uexpr 			{ Binop($1, UDiv, $3) }
	| uexpr POW INT_LITERAL 		{ Binop($1, UPow, $3) } // float int parsing
	| uexpr POW FLOAT_LITERAL 		{ Binop($1, UPow, $3) } // limit: not support neg? m^2
	| LPAREN uexpr RPAREN 			{ $2 }


/* constant calculation before uexpr */
cexpr:
	 cexpr TIMES  cexpr 	{ Binop($1, Mul, $3) }
	| cexpr DIVIDE cexpr  	{ Binop($1, Div, $3) }
	| cexpr POW cexpr 		{ Binop($1, Pow, $3) }
	| LPAREN cexpr RPAREN 	{ $2 }
	| MINUS expr %prec NEG 	{ Unop(Neg, $2) }
	| INT_LITERAL 		 	{ Lit(IntLit($1)) }
	| FLOAT_LITERAL 	 	{ Lit(FloatLit($1)) } 



/* supported types */
typ:
   INT     { Int   }
  | FLOAT  { Float }  
  | CHAR   { Char  }
  | STRING { String}  
  | BOOL   { Bool  }


/* expressions */
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
	| MINUS expr %prec NEG 							{ Unop(Neg, $2) }
	| NOT expr 										{ Unop(Not, $2) }
	| ID ASN expr 								{ Assign($1, $3) }
	| LPAREN expr RPAREN 							{ $2 }
	| ID LPAREN actuals_opt RPAREN 					{ Call($1, $3) }



/* actuals_opt: an optional expression in a function call */
actuals_opt: 
    { [] }
  | actuals_list { List.rev $1 }


/* identifier list: seems useless */

actuals_list:
   ID                { [$1] }  //   a 
  | INT_LITERAL       { [$1] }  //   1
  | FLOAT_LITERAL     { [$1] }  // 1.0
  | actuals_list COMMA INT_LITERAL   { $3 :: $1 }
  | actuals_list COMMA FLOAT_LITERAL { $3 :: $1 }
  | actuals_list COMMA ID  { $3 :: $1 } 


  /* in equation call*/