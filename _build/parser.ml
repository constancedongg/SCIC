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
\007\000\009\000\009\000\009\000\009\000\009\000\009\000\011\000\
\011\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\012\000\012\000\013\000\013\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\009\000\000\000\001\000\002\000\004\000\000\000\
\002\000\002\000\003\000\003\000\005\000\007\000\009\000\000\000\
\001\000\001\000\001\000\001\000\001\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\004\000\003\000\003\000\
\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\054\000\000\000\009\000\006\000\007\000\008\000\
\010\000\001\000\003\000\004\000\000\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\016\000\000\000\000\000\015\000\000\000\016\000\011\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\027\000\028\000\
\029\000\000\000\017\000\000\000\000\000\000\000\045\000\044\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\048\000\020\000\000\000\
\000\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\000\000\
\000\000\023\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\042\000\020\000\027\000\021\000\
\043\000\044\000\052\000\076\000\077\000"

let yysindex = "\005\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\221\254\217\254\026\255\051\255\
\000\000\235\254\249\254\058\255\068\255\000\000\069\255\235\254\
\000\000\024\255\039\255\000\000\175\255\000\000\000\000\175\255\
\175\255\076\255\081\255\175\255\014\255\000\000\000\000\000\000\
\000\000\040\255\000\000\041\000\200\000\075\255\000\000\000\000\
\175\255\175\255\235\000\092\255\175\255\175\255\080\255\000\000\
\175\255\175\255\175\255\175\255\175\255\175\255\175\255\175\255\
\175\255\175\255\175\255\175\255\175\255\000\000\000\000\222\000\
\109\255\000\000\235\000\107\255\110\255\235\000\175\255\105\255\
\105\255\098\255\098\255\000\000\248\000\248\000\084\255\084\255\
\084\255\084\255\179\000\055\000\144\255\175\255\000\000\175\255\
\235\000\085\255\067\000\235\000\144\255\175\255\000\000\123\255\
\144\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\124\255\000\000\000\000\129\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\134\255\220\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\134\255\021\255\000\000\131\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\255\000\000\132\255\041\255\000\000\093\000\
\119\000\246\255\024\000\000\000\190\255\187\000\254\255\127\000\
\153\000\161\000\061\255\048\255\000\000\000\000\000\000\000\000\
\055\255\111\255\000\000\056\255\000\000\133\255\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\085\000\000\000\109\000\000\000\
\174\255\227\255\211\255\000\000\000\000"

let yytablesize = 528
let yytable = "\045\000\
\010\000\014\000\047\000\048\000\073\000\001\000\051\000\005\000\
\006\000\007\000\098\000\008\000\016\000\052\000\009\000\052\000\
\015\000\053\000\103\000\072\000\051\000\025\000\106\000\075\000\
\078\000\025\000\017\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\054\000\047\000\029\000\047\000\022\000\047\000\030\000\031\000\
\043\000\097\000\043\000\032\000\043\000\033\000\018\000\046\000\
\104\000\046\000\053\000\046\000\053\000\042\000\023\000\042\000\
\099\000\042\000\100\000\005\000\006\000\007\000\024\000\008\000\
\051\000\043\000\009\000\028\000\025\000\034\000\029\000\049\000\
\035\000\036\000\030\000\071\000\050\000\042\000\042\000\032\000\
\013\000\033\000\037\000\055\000\074\000\038\000\039\000\040\000\
\041\000\057\000\058\000\059\000\060\000\061\000\019\000\005\000\
\006\000\007\000\079\000\008\000\026\000\094\000\009\000\095\000\
\096\000\034\000\021\000\061\000\035\000\036\000\021\000\021\000\
\059\000\060\000\061\000\021\000\101\000\021\000\037\000\105\000\
\012\000\038\000\039\000\040\000\041\000\013\000\024\000\050\000\
\051\000\024\000\046\000\021\000\021\000\021\000\000\000\021\000\
\000\000\000\000\021\000\029\000\000\000\021\000\000\000\030\000\
\021\000\021\000\000\000\000\000\032\000\000\000\033\000\000\000\
\000\000\000\000\021\000\000\000\000\000\021\000\021\000\021\000\
\021\000\000\000\000\000\000\000\005\000\006\000\007\000\000\000\
\008\000\000\000\029\000\009\000\000\000\000\000\034\000\000\000\
\000\000\035\000\036\000\032\000\000\000\033\000\036\000\000\000\
\036\000\000\000\036\000\037\000\000\000\000\000\038\000\039\000\
\040\000\041\000\000\000\005\000\006\000\007\000\000\000\008\000\
\036\000\036\000\009\000\000\000\000\000\000\000\036\000\036\000\
\000\000\000\000\000\000\000\000\030\000\000\000\030\000\000\000\
\030\000\000\000\037\000\000\000\000\000\038\000\039\000\040\000\
\041\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\033\000\000\000\
\033\000\000\000\033\000\000\000\000\000\000\000\038\000\000\000\
\038\000\000\000\038\000\033\000\033\000\033\000\033\000\000\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\034\000\000\000\034\000\000\000\034\000\005\000\006\000\007\000\
\000\000\008\000\000\000\000\000\009\000\034\000\034\000\034\000\
\034\000\056\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\000\000\000\000\000\000\000\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\102\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\031\000\000\000\031\000\
\000\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\031\000\031\000\000\000\000\000\000\000\031\000\
\031\000\031\000\031\000\031\000\031\000\031\000\031\000\032\000\
\000\000\032\000\000\000\032\000\000\000\000\000\000\000\040\000\
\000\000\040\000\000\000\040\000\032\000\032\000\000\000\000\000\
\000\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\039\000\000\000\039\000\000\000\039\000\000\000\000\000\
\000\000\041\000\000\000\041\000\000\000\041\000\000\000\000\000\
\000\000\000\000\000\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\039\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\037\000\000\000\037\000\000\000\037\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\000\000\070\000\037\000\037\000\000\000\
\000\000\000\000\000\000\037\000\037\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\093\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\057\000\058\000\059\000\
\060\000\061\000\000\000\000\000\064\000\065\000\066\000\067\000"

let yycheck = "\029\000\
\000\000\037\001\032\000\033\000\050\000\001\000\036\000\029\001\
\030\001\031\001\093\000\033\001\052\001\003\001\036\001\005\001\
\052\001\004\001\101\000\049\000\050\000\001\001\105\000\053\000\
\054\000\005\001\001\001\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\027\001\001\001\004\001\003\001\052\001\005\001\008\001\009\001\
\001\001\079\000\003\001\013\001\005\001\015\001\004\001\001\001\
\102\000\003\001\003\001\005\001\005\001\001\001\005\001\003\001\
\094\000\005\001\096\000\029\001\030\001\031\001\003\001\033\001\
\102\000\026\001\036\001\052\001\008\001\039\001\004\001\004\001\
\042\001\043\001\008\001\009\001\004\001\025\001\026\001\013\001\
\004\000\015\001\052\001\052\001\001\001\055\001\056\001\057\001\
\058\001\014\001\015\001\016\001\017\001\018\001\018\000\029\001\
\030\001\031\001\027\001\033\001\024\000\001\001\036\001\005\001\
\003\001\039\001\004\001\018\001\042\001\043\001\008\001\009\001\
\016\001\017\001\018\001\013\001\040\001\015\001\052\001\005\001\
\005\001\055\001\056\001\057\001\058\001\005\001\001\001\005\001\
\005\001\005\001\030\000\029\001\030\001\031\001\255\255\033\001\
\255\255\255\255\036\001\004\001\255\255\039\001\255\255\008\001\
\042\001\043\001\255\255\255\255\013\001\255\255\015\001\255\255\
\255\255\255\255\052\001\255\255\255\255\055\001\056\001\057\001\
\058\001\255\255\255\255\255\255\029\001\030\001\031\001\255\255\
\033\001\255\255\004\001\036\001\255\255\255\255\039\001\255\255\
\255\255\042\001\043\001\013\001\255\255\015\001\001\001\255\255\
\003\001\255\255\005\001\052\001\255\255\255\255\055\001\056\001\
\057\001\058\001\255\255\029\001\030\001\031\001\255\255\033\001\
\019\001\020\001\036\001\255\255\255\255\255\255\025\001\026\001\
\255\255\255\255\255\255\255\255\001\001\255\255\003\001\255\255\
\005\001\255\255\052\001\255\255\255\255\055\001\056\001\057\001\
\058\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\001\001\255\255\
\003\001\255\255\005\001\255\255\255\255\255\255\001\001\255\255\
\003\001\255\255\005\001\014\001\015\001\016\001\017\001\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\001\001\255\255\003\001\255\255\005\001\029\001\030\001\031\001\
\255\255\033\001\255\255\255\255\036\001\014\001\015\001\016\001\
\017\001\001\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\255\255\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\001\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\001\001\255\255\003\001\
\255\255\005\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\014\001\015\001\255\255\255\255\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\001\001\
\255\255\003\001\255\255\005\001\255\255\255\255\255\255\001\001\
\255\255\003\001\255\255\005\001\014\001\015\001\255\255\255\255\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\001\001\255\255\003\001\255\255\005\001\255\255\255\255\
\255\255\001\001\255\255\003\001\255\255\005\001\255\255\255\255\
\255\255\255\255\255\255\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\001\001\255\255\003\001\255\255\005\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\255\255\005\001\019\001\020\001\255\255\
\255\255\255\255\255\255\025\001\026\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\005\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\014\001\015\001\016\001\
\017\001\018\001\255\255\255\255\021\001\022\001\023\001\024\001"

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
# 416 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                   ( ([], [])  )
# 422 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 58 "parser.mly"
                    (((_2 :: fst _1), snd _1))
# 430 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 65 "parser.mly"
                     ((fst _1, (_2 :: snd _1)))
# 438 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 78 "parser.mly"
               ( (_1, _2) )
# 446 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
           ( Int   )
# 452 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
           ( Float )
# 458 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
           ( String)
# 464 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
           ( Bool  )
# 470 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
           ( Void  )
# 476 "parser.ml"
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
# 491 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 146 "parser.mly"
                 ([])
# 497 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 150 "parser.mly"
                  (List.rev _1 )
# 504 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 155 "parser.mly"
           ( [(_1, _2)] )
# 512 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 157 "parser.mly"
                               ( (_3, _4) :: _1 )
# 521 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 163 "parser.mly"
   ( [] )
# 527 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 164 "parser.mly"
                    ( _2 :: _1 )
# 535 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 167 "parser.mly"
             (Expr _1)
# 542 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 168 "parser.mly"
                                    (Return _2)
# 549 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 169 "parser.mly"
                                          ( Block(List.rev _2) )
# 556 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 170 "parser.mly"
                                           ( If(_3, _5, Block([])) )
# 564 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 171 "parser.mly"
                                             ( If(_3, _5, _7)        )
# 573 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 173 "parser.mly"
                                             ( For(_3, _5, _7, _9)   )
# 583 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 176 "parser.mly"
                  ( Noexpr )
# 589 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 177 "parser.mly"
                   ( _1 )
# 596 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 180 "parser.mly"
                        ( IntLit(_1) )
# 603 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 181 "parser.mly"
                           ( FloatLit(_1) )
# 610 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 182 "parser.mly"
                                 ( StringLit(_1) )
# 617 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 183 "parser.mly"
                         ( BoolLit(_1) )
# 624 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 184 "parser.mly"
                    ( Id(_1) )
# 631 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 185 "parser.mly"
                          ( Binop(_1, Add, _3) )
# 639 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 186 "parser.mly"
                          ( Binop(_1, Sub, _3) )
# 647 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "parser.mly"
                          ( Binop(_1, Mult, _3) )
# 655 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "parser.mly"
                           ( Binop(_1, Div, _3) )
# 663 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 189 "parser.mly"
                         ( Binop(_1, Pow, _3) )
# 671 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 190 "parser.mly"
                         ( Binop(_1, Equal, _3) )
# 679 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 191 "parser.mly"
                         ( Binop(_1, Neq, _3) )
# 687 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "parser.mly"
                         ( Binop(_1, Less, _3) )
# 695 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 193 "parser.mly"
                         ( Binop(_1, Leq, _3) )
# 703 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 194 "parser.mly"
                         ( Binop(_1, Greater, _3) )
# 711 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 195 "parser.mly"
                         ( Binop(_1, Geq, _3) )
# 719 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 196 "parser.mly"
                         ( Binop(_1, And, _3) )
# 727 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 197 "parser.mly"
                         ( Binop(_1, Or, _3) )
# 735 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 198 "parser.mly"
                               ( Unop(Neg, _2) )
# 742 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 199 "parser.mly"
                      ( Unop(Not, _2) )
# 749 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 200 "parser.mly"
                                      ( DAssign(_1, _2, _4) )
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 201 "parser.mly"
                       ( Assign(_1, _3) )
# 766 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 202 "parser.mly"
                             ( _2 )
# 773 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 204 "parser.mly"
                                       (FunctionCall(_1, _3))
# 781 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 271 "parser.mly"
                  ( [] )
# 787 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 272 "parser.mly"
               ( List.rev _1 )
# 794 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 275 "parser.mly"
                            ( [_1] )
# 801 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 276 "parser.mly"
                         ( _3 :: _1 )
# 809 "parser.ml"
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
