%{ open Ast %}

/* punctuations, unary operators, binary arithmetic, binary relational + logical, assignment 
   data type, declaration, statement, library function */
%token SEMI COLASN COMMA LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE BAR PRIME
%token NEG NOT
%token PLUS MINUS TIMES DIVIDE POW
%token EQ NEQ LT GT LEQ GEQ AND OR 
// TRUE FALSE 
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
// %nonassoc NOUNIT


%right ASN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left POW
%right NOT NEG

// 
%nonassoc LPAREN LBRACK LBRACE
%nonassoc RPAREN RBRACK RBRACE


%start program
%type <Ast.program> program

%%

program:
   decls EOF { $1 }

decls:
   /* nothing */   { {globals=[]; udecls=[]; fdecls=[];} }
   // | decls var_decl {(($2 :: fst $1), snd $1)}
   // | decls func_decl {(fst $1, ($2 :: snd $1))}
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
   // | decls equa_decl {{
	// 					vars = $1.vars;
	// 					units = $1.units;
	// 					funcs = $1.funcs;
	// 					equas = $2 :: $1.equas;
  	// 				}}           



/**************** unit_decl *******************/
   /* |'{mm} = 0.001 '{m}|; */
unit_decl:
   BAR UNIT ASN FLOAT_LITERAL UNIT BAR SEMI { ($2, $5, $4) } 
   // | BAR UNIT EQ INT_LITERAL UNIT BAR SEMI {($2, $4, $5)} 
/***********************************/

/*************** var_decl ********************/
var_decl: 
   /* <type> <unit> <variable_name>; int '{m} x; */
   // typ unit ID SEMI {($1, $2, $3)}
   /* <type> <variable_name>; int x; */
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
// lst_type:
//     typ LBRACK RBRACK { ArrayType($1) }

// =========== uexpr ============

// unit_opt:
//   /* no unit */ { }
// | UNIT         { $1 }

// uexpr:
//    UNIT              { Unit($1) }
//    // | /* nothing */   { Nounit }

// uexpr_opt:
//    /* nothing */  {Nounit}
//  | uexpr          {$1}
    

// bi_unit:
// 	 METER		{ Meter } /* base */
// 	| SEC    	{ Second }
// 	| CMETER     { Centimeter }
	// | AMP      	{ Ampere }
	// | HERTZ  	{ Hertz } /* derived */
	// | CMETER    { Centimeter }
	// | GRAM      { Gram }
	// | NEWTON    { Newton }

// uexpr:
// 	 UONE 							{ $1 }  // ??  |'{a} = '{1}|
// 	| UID 							{ Uid($1) } // |'{cm} = 0.01 * '{m} |
//    | bi_unit						{ Uid($1) }
// 	| uexpr TIMES uexpr 			{ Binop($1, UMult, $3) }
// 	| uexpr DIVIDE uexpr 			{ Binop($1, UDiv, $3) }
// 	| uexpr POW INT_LITERAL 		{ Binop($1, UPow, $3) } // float int parsing
// 	| uexpr POW FLOAT_LITERAL 		{ Binop($1, UPow, $3) } // limit: not support neg? m^2
// 	| LPAREN uexpr RPAREN 			{ $2 }
   

/***********************************/


/**************** func_decl *******************/
func_decl:
   /* <type> <unit> func <function_name> (args) {statement}*/
   // typ unit FUNC ID formals_block stmt_block {{
   //    return_type       = $1;
	// 	return_unit       = $2;
	// 	func_identifier   = $4;
	// 	func_formals      = List.rev $5;
	// 	func_stmts        = List.rev $6
   // }}
   unit_typ UNIT FUNC ID LPAREN opt_formals RPAREN LBRACE stmt_list RBRACE{{
      return_type       = $1;
      return_unit       = $2; 
		func_identifier   = $4;
		func_formals      = List.rev $6;
		func_stmts        = List.rev $9;
   }}
|  typ FUNC ID LPAREN opt_formals RPAREN LBRACE stmt_list RBRACE{{
      return_type       = $1;
      return_unit       = "1";
		func_identifier   = $3;
		func_formals      = List.rev $5;
		func_stmts        = List.rev $8;
   }}

/***** args *****/
// formals_block:
//    /* ...(args)... */ 
//    LPAREN opt_formals RPAREN { $2 }

opt_formals:
   /* nothing */ {[]}
   /* ...(<type> <units> <args_name>, ...)...*/
   /* ...(<type> <args_name>, ...)...*/ 
   /* ...(<type> <units> <args_name>, <type> <args_name>)...*/
   | formals_list {List.rev $1 }


formals_list:
    typ ID { [($1, "1", $2)] }
   | unit_typ UNIT ID { [($1, $2, $3)] }
   | formals_list COMMA unit_typ UNIT ID { ($3, $4, $5) :: $1 } 
   | formals_list COMMA typ ID { ($3, "1", $4) :: $1 } 

/***** statement *****/

stmt_list:
   // cannot be empty, at lease return;
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
	// | ID ASN expr 								{ Assign($1, $3) }
   | expr ASN expr 								{ Assign($1, $3) }
	| LPAREN expr RPAREN 							{ $2 }
   /* function call */ /* equation call */
   | ID LPAREN args RPAREN             { FunctionCall($1, $3) } 
   | LBRACK opt_lst RBRACK                   { Array($2)}
   | expr LBRACK expr RBRACK            { ArrayAccess($1, $3)}

   /* | '{m} = 10*12/10*'{mm} | */ 
   // | BAR PRIME LBRACE UID RBRACE ASN cexpr TIMES unit BAR SEMI {UnitAssign($4, $7, $9)}
   // /* int x = 10/2 */
   // | typ ID ASN expr                   { Init_Assign($1,$2,$4) }
   // /* int '{m} = 10 */
   // | typ unit ID ASN expr              {Init_Assign_Unit($1, $2, $3, $5)}
   // /* List operation */
   // /* init */
   // /* int[] x = [] */
   // /* int[] unit x = [1,2,3,6,6] */
   // | lst_type unit ID ASN lst_block          {ListInitUnit($1, $2, $3, $5)}
   // | lst_type ID ASN lst_block               {ListInit($1, $2, $4)}
   // /* assign */
   // /* x[1] = 10 */
   // | ID LBRACK INT_LITERAL RBRACK ASN prime  { ListEleAssign($1, $3, $6) }
   // /* access */
   // /* y = x[1] */
   // | ID LBRACK INT_LITERAL RBRACK {ListAccess($1, $3)}




// lst_block:
//   LBRACK opt_lst RBRACK {$2}

opt_lst:
   {[]}
   | lst { List.rev $1 }

lst: 
     expr { [$1]}
   | lst COMMA expr { $3 :: $1 }

// lst:
//    prime {[$1]}
//    | lst COMMA prime {$3 :: $1}

// prime:
//       INT_LITERAL 									{ IntLit($1) }
//    | FLOAT_LITERAL 								{ FloatLit($1) }
// 	| CHAR_LITERAL 									{ CharLit($1) }
// 	| STRING_LITERAL 								{ StringLit($1) }
// 	| BOOL_LITERAL 									{ BoolLit($1) }
   
// primeNwithid:
//    ID {Id($1)}
//    | INT_LITERAL 									{ Lit(IntLit($1)) }
//    | FLOAT_LITERAL 								{ Lit(FloatLit($1)) }

// primewithid:
//    ID {Id($1)}
//    | INT_LITERAL 									{ Lit(IntLit($1)) }
//    | FLOAT_LITERAL 								{ Lit(FloatLit($1)) }
//    | CHAR_LITERAL 									{ Lit(CharLit($1)) }
// 	| STRING_LITERAL 								{ Lit(StringLit($1)) }
// 	| BOOL_LITERAL 									{ Lit(BoolLit($1)) }
// 	| TRUE 													{ BoolLit(true) }
// 	| FALSE 												{ BoolLit(false) } 

// cexpr:
// 	 cexpr TIMES  cexpr 	{ Binop($1, Mul, $3) }
// 	| cexpr DIVIDE cexpr  	{ Binop($1, Div, $3) }
// 	| cexpr POW cexpr 		{ Binop($1, Pow, $3) }
// 	| LPAREN cexpr RPAREN 	{ $2 }
// 	| MINUS expr %prec NEG 	{ Unop(Neg, $2) }
// 	| INT_LITERAL 		 	{ Lit(IntLit($1)) }
// 	| FLOAT_LITERAL 	 	{ Lit(FloatLit($1)) }


args:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

// THIS IS NEEDED, change the ast
// args:
//    {[]}
//    | args_lst { List.rev $1}
//    | equa_args_lst { List.rev $1}

// args_lst:
//    primewithid { [$1] }
//    | args_lst COMMA primewithid {$3::$1}

// equa_args_lst:
//    equa_arg { [$1] }
//    | equa_args_lst COMMA equa_arg {$3::$1}

// equa_arg:
//    ID ASN ID {EquaArg($1, $3)}

/***********************************/

/**************** equal_decl *******************/
// equa_decl:
//  /* equa <equation_name> (args) {statement}*/
//    EQUA ID formals_block LBRACE equa_stmt RBRACE 
//    {{
//       equa_identifier = $2;
//       equa_formals = List.rev $3;
//       equa_stmt = $5; 
//    }}

// equa_stmt:
//    { [] }
//    /* x*y/c-1 = ms+o-y+1 */
//    | equa_expr ASN equa_expr { equa($1, $3)}

// equa_expr:
//    INT_LITERAL 									      { IntLit($1) }
//    | FLOAT_LITERAL 								      { FloatLit($1) } 
//    | ID 														{ Id($1) }
// 	| equa_expr PLUS equa_expr 							{ Binop($1, Add, $3) }
// 	| equa_expr MINUS equa_expr 							{ Binop($1, Sub, $3) }
// 	| equa_expr TIMES equa_expr 							{ Binop($1, Mult, $3) }
// 	| equa_expr DIVIDE equa_expr 							{ Binop($1, Div, $3) }
// 	| equa_expr POW equa_expr 								{ Binop($1, Pow, $3) }
/***********************************/