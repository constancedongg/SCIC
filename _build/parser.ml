type token =
  | SEMI
  | COLASN
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | BAR
  | PRIME
  | NEG
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | POW
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | AND
  | OR
  | ASN
  | DASN
  | BOOL
  | INT
  | FLOAT
  | CHAR
  | STRING
  | INTARR
  | FLOATARR
  | VOID
  | FUNC
  | EQUA
  | IF
  | ELSE
  | NOELSE
  | FOR
  | RETURN
  | METER
  | SEC
  | KGRAM
  | AMP
  | CMETER
  | HERTZ
  | GRAM
  | NEWTON
  | ID of (string)
  | UID of (string)
  | UONE of (string)
  | INT_LITERAL of (int)
  | FLOAT_LITERAL of (string)
  | STRING_LITERAL of (string)
  | BOOL_LITERAL of (bool)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 67 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* COLASN *);
  259 (* COMMA *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* LBRACK *);
  263 (* RBRACK *);
  264 (* LBRACE *);
  265 (* RBRACE *);
  266 (* BAR *);
  267 (* PRIME *);
  268 (* NEG *);
  269 (* NOT *);
  270 (* PLUS *);
  271 (* MINUS *);
  272 (* TIMES *);
  273 (* DIVIDE *);
  274 (* POW *);
  275 (* EQ *);
  276 (* NEQ *);
  277 (* LT *);
  278 (* GT *);
  279 (* LEQ *);
  280 (* GEQ *);
  281 (* AND *);
  282 (* OR *);
  283 (* ASN *);
  284 (* DASN *);
  285 (* BOOL *);
  286 (* INT *);
  287 (* FLOAT *);
  288 (* CHAR *);
  289 (* STRING *);
  290 (* INTARR *);
  291 (* FLOATARR *);
  292 (* VOID *);
  293 (* FUNC *);
  294 (* EQUA *);
  295 (* IF *);
  296 (* ELSE *);
  297 (* NOELSE *);
  298 (* FOR *);
  299 (* RETURN *);
  300 (* METER *);
  301 (* SEC *);
  302 (* KGRAM *);
  303 (* AMP *);
  304 (* CMETER *);
  305 (* HERTZ *);
  306 (* GRAM *);
  307 (* NEWTON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  308 (* ID *);
  309 (* UID *);
  310 (* UONE *);
  311 (* INT_LITERAL *);
  312 (* FLOAT_LITERAL *);
  313 (* STRING_LITERAL *);
  314 (* BOOL_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\005\000\005\000\005\000\
\005\000\005\000\004\000\006\000\006\000\008\000\008\000\007\000\
\007\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\011\000\011\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\012\000\012\000\013\000\013\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\009\000\000\000\001\000\002\000\004\000\000\000\
\002\000\002\000\005\000\003\000\003\000\005\000\007\000\009\000\
\000\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\002\000\003\000\003\000\
\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\054\000\000\000\009\000\006\000\007\000\008\000\
\010\000\001\000\003\000\004\000\000\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\016\000\000\000\000\000\015\000\000\000\016\000\011\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\028\000\029\000\
\030\000\000\000\017\000\000\000\000\000\000\000\046\000\045\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\048\000\021\000\000\000\
\000\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\000\000\023\000\
\000\000\000\000\024\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\042\000\020\000\027\000\021\000\
\043\000\044\000\052\000\076\000\077\000"

let yysindex = "\016\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\227\254\231\254\044\255\056\255\
\000\000\192\255\019\255\068\255\073\255\000\000\069\255\192\255\
\000\000\028\255\039\255\000\000\001\255\000\000\000\000\001\255\
\001\255\081\255\088\255\001\255\014\255\000\000\000\000\000\000\
\000\000\047\255\000\000\002\000\198\000\075\255\000\000\000\000\
\001\255\001\255\233\000\092\255\001\255\001\255\074\255\000\000\
\001\255\001\255\001\255\001\255\001\255\001\255\001\255\001\255\
\001\255\001\255\001\255\001\255\001\255\000\000\000\000\220\000\
\106\255\000\000\233\000\105\255\109\255\233\000\001\255\118\255\
\118\255\095\255\095\255\000\000\001\001\001\001\251\254\251\254\
\251\254\251\254\246\000\042\000\144\255\001\255\000\000\001\255\
\028\000\076\255\054\000\233\000\000\000\144\255\001\255\000\000\
\116\255\144\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\117\255\000\000\000\000\120\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\127\255\189\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\127\255\021\255\000\000\124\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\046\255\000\000\132\255\041\255\000\000\080\000\
\106\000\215\255\241\255\000\000\159\255\182\000\114\000\140\000\
\148\000\174\000\061\255\097\255\000\000\000\000\000\000\000\000\
\000\000\111\255\000\000\058\255\000\000\000\000\133\255\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\085\000\000\000\109\000\000\000\
\169\255\227\255\208\255\000\000\000\000"

let yytablesize = 537
let yytable = "\045\000\
\010\000\073\000\047\000\048\000\029\000\098\000\051\000\014\000\
\057\000\058\000\059\000\060\000\061\000\032\000\104\000\033\000\
\001\000\053\000\107\000\072\000\051\000\026\000\015\000\075\000\
\078\000\026\000\016\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\054\000\047\000\029\000\047\000\017\000\047\000\030\000\031\000\
\052\000\097\000\052\000\032\000\037\000\033\000\105\000\038\000\
\039\000\040\000\041\000\018\000\053\000\043\000\053\000\043\000\
\099\000\043\000\100\000\005\000\006\000\007\000\022\000\008\000\
\023\000\051\000\009\000\024\000\025\000\034\000\029\000\028\000\
\035\000\036\000\030\000\071\000\049\000\043\000\043\000\032\000\
\013\000\033\000\037\000\050\000\074\000\038\000\039\000\040\000\
\041\000\044\000\055\000\044\000\079\000\044\000\019\000\005\000\
\006\000\007\000\094\000\008\000\026\000\095\000\009\000\096\000\
\061\000\034\000\022\000\102\000\035\000\036\000\022\000\022\000\
\106\000\012\000\044\000\022\000\013\000\022\000\037\000\025\000\
\050\000\038\000\039\000\040\000\041\000\059\000\060\000\061\000\
\051\000\025\000\046\000\022\000\022\000\022\000\000\000\022\000\
\000\000\000\000\022\000\029\000\000\000\022\000\000\000\030\000\
\022\000\022\000\000\000\000\000\032\000\000\000\033\000\037\000\
\000\000\037\000\022\000\037\000\000\000\022\000\022\000\022\000\
\022\000\000\000\000\000\000\000\005\000\006\000\007\000\000\000\
\008\000\037\000\037\000\009\000\000\000\000\000\034\000\037\000\
\037\000\035\000\036\000\000\000\000\000\031\000\000\000\031\000\
\000\000\031\000\000\000\037\000\000\000\000\000\038\000\039\000\
\040\000\041\000\031\000\031\000\031\000\031\000\031\000\031\000\
\031\000\031\000\031\000\031\000\031\000\031\000\031\000\034\000\
\000\000\034\000\000\000\034\000\005\000\006\000\007\000\000\000\
\008\000\000\000\000\000\009\000\034\000\034\000\034\000\034\000\
\000\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\035\000\000\000\035\000\000\000\035\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\035\000\035\000\
\035\000\035\000\056\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\035\000\000\000\000\000\000\000\000\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\101\000\005\000\006\000\007\000\
\000\000\008\000\000\000\000\000\009\000\000\000\000\000\000\000\
\000\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\103\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\032\000\000\000\032\000\000\000\032\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\032\000\000\000\
\000\000\000\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\032\000\033\000\000\000\033\000\000\000\033\000\000\000\
\000\000\000\000\039\000\000\000\039\000\000\000\039\000\033\000\
\033\000\000\000\000\000\000\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\039\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\041\000\000\000\041\000\000\000\
\041\000\000\000\000\000\000\000\040\000\000\000\040\000\000\000\
\040\000\000\000\000\000\000\000\000\000\000\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\042\000\000\000\
\042\000\000\000\042\000\000\000\000\000\000\000\038\000\000\000\
\038\000\000\000\038\000\000\000\000\000\000\000\000\000\000\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\038\000\038\000\070\000\000\000\000\000\000\000\038\000\038\000\
\000\000\000\000\000\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\093\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\057\000\058\000\
\059\000\060\000\061\000\000\000\000\000\064\000\065\000\066\000\
\067\000"

let yycheck = "\029\000\
\000\000\050\000\032\000\033\000\004\001\093\000\036\000\037\001\
\014\001\015\001\016\001\017\001\018\001\013\001\102\000\015\001\
\001\000\004\001\106\000\049\000\050\000\001\001\052\001\053\000\
\054\000\005\001\052\001\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\027\001\001\001\004\001\003\001\001\001\005\001\008\001\009\001\
\003\001\079\000\005\001\013\001\052\001\015\001\103\000\055\001\
\056\001\057\001\058\001\004\001\003\001\001\001\005\001\003\001\
\094\000\005\001\096\000\029\001\030\001\031\001\052\001\033\001\
\005\001\103\000\036\001\003\001\008\001\039\001\004\001\052\001\
\042\001\043\001\008\001\009\001\004\001\025\001\026\001\013\001\
\004\000\015\001\052\001\004\001\001\001\055\001\056\001\057\001\
\058\001\001\001\052\001\003\001\027\001\005\001\018\000\029\001\
\030\001\031\001\001\001\033\001\024\000\005\001\036\001\003\001\
\018\001\039\001\004\001\040\001\042\001\043\001\008\001\009\001\
\005\001\005\001\026\001\013\001\005\001\015\001\052\001\001\001\
\005\001\055\001\056\001\057\001\058\001\016\001\017\001\018\001\
\005\001\005\001\030\000\029\001\030\001\031\001\255\255\033\001\
\255\255\255\255\036\001\004\001\255\255\039\001\255\255\008\001\
\042\001\043\001\255\255\255\255\013\001\255\255\015\001\001\001\
\255\255\003\001\052\001\005\001\255\255\055\001\056\001\057\001\
\058\001\255\255\255\255\255\255\029\001\030\001\031\001\255\255\
\033\001\019\001\020\001\036\001\255\255\255\255\039\001\025\001\
\026\001\042\001\043\001\255\255\255\255\001\001\255\255\003\001\
\255\255\005\001\255\255\052\001\255\255\255\255\055\001\056\001\
\057\001\058\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\001\001\
\255\255\003\001\255\255\005\001\029\001\030\001\031\001\255\255\
\033\001\255\255\255\255\036\001\014\001\015\001\016\001\017\001\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\001\001\255\255\003\001\255\255\005\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\001\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\255\255\255\255\255\255\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\001\001\029\001\030\001\031\001\
\255\255\033\001\255\255\255\255\036\001\255\255\255\255\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\001\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\001\001\255\255\003\001\255\255\005\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\014\001\015\001\255\255\
\255\255\255\255\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\001\001\255\255\003\001\255\255\005\001\255\255\
\255\255\255\255\001\001\255\255\003\001\255\255\005\001\014\001\
\015\001\255\255\255\255\255\255\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\001\001\255\255\003\001\255\255\
\005\001\255\255\255\255\255\255\001\001\255\255\003\001\255\255\
\005\001\255\255\255\255\255\255\255\255\255\255\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\001\001\255\255\
\003\001\255\255\005\001\255\255\255\255\255\255\001\001\255\255\
\003\001\255\255\005\001\255\255\255\255\255\255\255\255\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\019\001\020\001\005\001\255\255\255\255\255\255\025\001\026\001\
\255\255\255\255\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\005\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\021\001\022\001\023\001\
\024\001"

let yynames_const = "\
  SEMI\000\
  COLASN\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  LBRACK\000\
  RBRACK\000\
  LBRACE\000\
  RBRACE\000\
  BAR\000\
  PRIME\000\
  NEG\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  POW\000\
  EQ\000\
  NEQ\000\
  LT\000\
  GT\000\
  LEQ\000\
  GEQ\000\
  AND\000\
  OR\000\
  ASN\000\
  DASN\000\
  BOOL\000\
  INT\000\
  FLOAT\000\
  CHAR\000\
  STRING\000\
  INTARR\000\
  FLOATARR\000\
  VOID\000\
  FUNC\000\
  EQUA\000\
  IF\000\
  ELSE\000\
  NOELSE\000\
  FOR\000\
  RETURN\000\
  METER\000\
  SEC\000\
  KGRAM\000\
  AMP\000\
  CMETER\000\
  HERTZ\000\
  GRAM\000\
  NEWTON\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  UID\000\
  UONE\000\
  INT_LITERAL\000\
  FLOAT_LITERAL\000\
  STRING_LITERAL\000\
  BOOL_LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 54 "parser.mly"
             ( _1 )
# 420 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                   ( ([], [])  )
# 426 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 58 "parser.mly"
                    (((_2 :: fst _1), snd _1))
# 434 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 65 "parser.mly"
                     ((fst _1, (_2 :: snd _1)))
# 442 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 78 "parser.mly"
               ( (_1, _2) )
# 450 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
           ( Int   )
# 456 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
           ( Float )
# 462 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
           ( String)
# 468 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
           ( Bool  )
# 474 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
           ( Void  )
# 480 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'opt_formals) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 133 "parser.mly"
                                                                ({
      return_type       = _1;
		func_identifier   = _3;
		func_formals      = List.rev _5;
		func_stmts        = List.rev _8;
   })
# 495 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 146 "parser.mly"
                 ([])
# 501 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 150 "parser.mly"
                  (List.rev _1 )
# 508 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 155 "parser.mly"
           ( [(_1, _2)] )
# 516 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 157 "parser.mly"
                               ( (_3, _4) :: _1 )
# 525 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 163 "parser.mly"
   ( [] )
# 531 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 164 "parser.mly"
                    ( _2 :: _1 )
# 539 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 167 "parser.mly"
             (Expr _1)
# 546 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
                                           ( DAssign(_1, _2, _4) )
# 555 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 169 "parser.mly"
                                    (Return _2)
# 562 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 170 "parser.mly"
                                          ( Block(List.rev _2) )
# 569 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 171 "parser.mly"
                                           ( If(_3, _5, Block([])) )
# 577 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 172 "parser.mly"
                                             ( If(_3, _5, _7)        )
# 586 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 173 "parser.mly"
                                                             ( For(_3, _5, _7, _9)   )
# 596 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "parser.mly"
                  ( Noexpr )
# 602 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
                   ( _1 )
# 609 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 181 "parser.mly"
                        ( IntLit(_1) )
# 616 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 182 "parser.mly"
                           ( FloatLit(_1) )
# 623 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 183 "parser.mly"
                                 ( StringLit(_1) )
# 630 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 184 "parser.mly"
                         ( BoolLit(_1) )
# 637 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 185 "parser.mly"
                    ( Id(_1) )
# 644 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 186 "parser.mly"
                          ( Binop(_1, Add, _3) )
# 652 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "parser.mly"
                          ( Binop(_1, Sub, _3) )
# 660 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "parser.mly"
                          ( Binop(_1, Mult, _3) )
# 668 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 189 "parser.mly"
                           ( Binop(_1, Div, _3) )
# 676 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 190 "parser.mly"
                         ( Binop(_1, Pow, _3) )
# 684 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 191 "parser.mly"
                         ( Binop(_1, Equal, _3) )
# 692 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "parser.mly"
                         ( Binop(_1, Neq, _3) )
# 700 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 193 "parser.mly"
                         ( Binop(_1, Less, _3) )
# 708 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 194 "parser.mly"
                         ( Binop(_1, Leq, _3) )
# 716 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 195 "parser.mly"
                         ( Binop(_1, Greater, _3) )
# 724 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 196 "parser.mly"
                         ( Binop(_1, Geq, _3) )
# 732 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 197 "parser.mly"
                         ( Binop(_1, And, _3) )
# 740 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 198 "parser.mly"
                         ( Binop(_1, Or, _3) )
# 748 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 199 "parser.mly"
                               ( Unop(Neg, _2) )
# 755 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 200 "parser.mly"
                      ( Unop(Not, _2) )
# 762 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 201 "parser.mly"
                       ( Assign(_1, _3) )
# 770 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 202 "parser.mly"
                             ( _2 )
# 777 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 204 "parser.mly"
                                       (FunctionCall(_1, _3))
# 785 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 271 "parser.mly"
                  ( [] )
# 791 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 272 "parser.mly"
               ( List.rev _1 )
# 798 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 275 "parser.mly"
                            ( [_1] )
# 805 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 276 "parser.mly"
                         ( _3 :: _1 )
# 813 "parser.ml"
               : 'args_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
