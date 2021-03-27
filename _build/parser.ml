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
  | CHAR_LITERAL of (char)
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
  284 (* BOOL *);
  285 (* INT *);
  286 (* FLOAT *);
  287 (* CHAR *);
  288 (* STRING *);
  289 (* INTARR *);
  290 (* FLOATARR *);
  291 (* VOID *);
  292 (* FUNC *);
  293 (* EQUA *);
  294 (* IF *);
  295 (* ELSE *);
  296 (* NOELSE *);
  297 (* FOR *);
  298 (* RETURN *);
  299 (* METER *);
  300 (* SEC *);
  301 (* KGRAM *);
  302 (* AMP *);
  303 (* CMETER *);
  304 (* HERTZ *);
  305 (* GRAM *);
  306 (* NEWTON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  307 (* ID *);
  308 (* UID *);
  309 (* UONE *);
  310 (* INT_LITERAL *);
  311 (* FLOAT_LITERAL *);
  312 (* CHAR_LITERAL *);
  313 (* STRING_LITERAL *);
  314 (* BOOL_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\005\000\005\000\005\000\
\005\000\005\000\005\000\004\000\006\000\006\000\008\000\008\000\
\007\000\007\000\009\000\009\000\009\000\009\000\009\000\009\000\
\011\000\011\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\012\000\012\000\013\000\013\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\009\000\000\000\001\000\002\000\004\000\
\000\000\002\000\002\000\003\000\003\000\005\000\007\000\009\000\
\000\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\003\000\
\003\000\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\055\000\000\000\010\000\006\000\007\000\008\000\
\009\000\011\000\001\000\003\000\004\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\000\000\000\000\015\000\000\000\
\000\000\017\000\000\000\000\000\016\000\000\000\017\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\028\000\
\029\000\030\000\031\000\018\000\000\000\000\000\000\000\047\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\021\000\000\000\
\000\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\050\000\000\000\000\000\
\000\000\000\000\000\000\000\000\023\000\000\000\000\000\024\000"

let yydgoto = "\002\000\
\003\000\004\000\012\000\013\000\014\000\021\000\028\000\022\000\
\044\000\045\000\053\000\076\000\077\000"

let yysindex = "\013\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\228\254\223\254\021\255\
\022\255\000\000\142\255\004\255\056\255\061\255\000\000\060\255\
\142\255\000\000\018\255\002\255\000\000\068\255\000\000\000\000\
\068\255\068\255\073\255\082\255\068\255\015\255\000\000\000\000\
\000\000\000\000\000\000\000\000\216\255\130\000\037\255\000\000\
\000\000\068\255\068\255\165\000\089\255\068\255\068\255\000\000\
\068\255\068\255\068\255\068\255\068\255\068\255\068\255\068\255\
\068\255\068\255\068\255\068\255\068\255\000\000\000\000\152\000\
\097\255\000\000\165\000\098\255\099\255\165\000\120\255\120\255\
\100\255\100\255\000\000\189\000\189\000\182\255\182\255\182\255\
\182\255\178\000\204\255\093\255\068\255\000\000\068\255\072\255\
\242\255\165\000\093\255\068\255\000\000\102\255\093\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\112\255\000\000\000\000\115\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\126\255\138\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\126\255\008\255\000\000\116\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\044\255\000\000\124\255\127\255\020\000\046\000\
\164\255\190\255\000\000\114\000\117\000\250\255\054\000\080\000\
\088\000\079\255\084\255\000\000\000\000\000\000\000\000\058\255\
\000\000\071\255\000\000\128\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\029\000\000\000\109\000\000\000\
\169\255\226\255\207\255\000\000\000\000"

let yytablesize = 469
let yytable = "\046\000\
\011\000\073\000\048\000\049\000\096\000\030\000\052\000\015\000\
\026\000\031\000\032\000\101\000\026\000\001\000\033\000\104\000\
\034\000\017\000\054\000\072\000\052\000\018\000\016\000\075\000\
\078\000\019\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\035\000\
\030\000\055\000\036\000\037\000\031\000\071\000\053\000\020\000\
\053\000\033\000\102\000\034\000\038\000\027\000\023\000\039\000\
\040\000\041\000\042\000\043\000\024\000\022\000\097\000\025\000\
\098\000\022\000\022\000\026\000\029\000\052\000\022\000\030\000\
\022\000\054\000\035\000\054\000\050\000\036\000\037\000\044\000\
\033\000\044\000\034\000\044\000\045\000\051\000\045\000\038\000\
\045\000\074\000\039\000\040\000\041\000\042\000\043\000\022\000\
\030\000\093\000\022\000\022\000\031\000\095\000\094\000\044\000\
\044\000\033\000\103\000\034\000\022\000\045\000\099\000\022\000\
\022\000\022\000\022\000\022\000\013\000\061\000\038\000\014\000\
\051\000\039\000\040\000\041\000\042\000\043\000\025\000\048\000\
\052\000\048\000\035\000\048\000\025\000\036\000\037\000\059\000\
\060\000\061\000\032\000\047\000\032\000\000\000\032\000\038\000\
\000\000\000\000\039\000\040\000\041\000\042\000\043\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\032\000\032\000\032\000\035\000\000\000\035\000\000\000\
\035\000\005\000\006\000\007\000\008\000\009\000\000\000\000\000\
\010\000\035\000\035\000\035\000\035\000\000\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\036\000\000\000\
\036\000\000\000\036\000\057\000\058\000\059\000\060\000\061\000\
\000\000\000\000\000\000\036\000\036\000\036\000\036\000\000\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\056\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\100\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\040\000\000\000\040\000\000\000\040\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\040\000\033\000\000\000\033\000\000\000\
\033\000\000\000\000\000\000\000\005\000\006\000\007\000\008\000\
\009\000\033\000\033\000\010\000\000\000\000\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\034\000\000\000\
\034\000\000\000\034\000\000\000\000\000\000\000\042\000\000\000\
\042\000\000\000\042\000\034\000\034\000\000\000\000\000\000\000\
\034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\041\000\000\000\041\000\000\000\041\000\000\000\000\000\000\000\
\043\000\000\000\043\000\000\000\043\000\000\000\000\000\000\000\
\000\000\000\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\043\000\038\000\000\000\038\000\039\000\038\000\039\000\
\000\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\038\000\038\000\070\000\039\000\
\039\000\000\000\038\000\038\000\000\000\039\000\039\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\092\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\057\000\058\000\059\000\060\000\061\000\000\000\
\000\000\064\000\065\000\066\000\067\000"

let yycheck = "\030\000\
\000\000\051\000\033\000\034\000\092\000\004\001\037\000\036\001\
\001\001\008\001\009\001\099\000\005\001\001\000\013\001\103\000\
\015\001\051\001\004\001\050\000\051\000\001\001\051\001\054\000\
\055\000\004\001\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\038\001\
\004\001\027\001\041\001\042\001\008\001\009\001\003\001\019\000\
\005\001\013\001\100\000\015\001\051\001\025\000\051\001\054\001\
\055\001\056\001\057\001\058\001\005\001\004\001\093\000\003\001\
\095\000\008\001\009\001\008\001\051\001\100\000\013\001\004\001\
\015\001\003\001\038\001\005\001\004\001\041\001\042\001\001\001\
\013\001\003\001\015\001\005\001\001\001\004\001\003\001\051\001\
\005\001\001\001\054\001\055\001\056\001\057\001\058\001\038\001\
\004\001\001\001\041\001\042\001\008\001\003\001\005\001\025\001\
\026\001\013\001\005\001\015\001\051\001\026\001\039\001\054\001\
\055\001\056\001\057\001\058\001\005\001\018\001\051\001\005\001\
\005\001\054\001\055\001\056\001\057\001\058\001\001\001\001\001\
\005\001\003\001\038\001\005\001\005\001\041\001\042\001\016\001\
\017\001\018\001\001\001\031\000\003\001\255\255\005\001\051\001\
\255\255\255\255\054\001\055\001\056\001\057\001\058\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\001\001\255\255\003\001\255\255\
\005\001\028\001\029\001\030\001\031\001\032\001\255\255\255\255\
\035\001\014\001\015\001\016\001\017\001\255\255\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\001\001\255\255\
\003\001\255\255\005\001\014\001\015\001\016\001\017\001\018\001\
\255\255\255\255\255\255\014\001\015\001\016\001\017\001\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\001\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\001\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\001\001\255\255\003\001\255\255\005\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\001\001\255\255\003\001\255\255\
\005\001\255\255\255\255\255\255\028\001\029\001\030\001\031\001\
\032\001\014\001\015\001\035\001\255\255\255\255\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\001\001\255\255\
\003\001\255\255\005\001\255\255\255\255\255\255\001\001\255\255\
\003\001\255\255\005\001\014\001\015\001\255\255\255\255\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\001\001\255\255\003\001\255\255\005\001\255\255\255\255\255\255\
\001\001\255\255\003\001\255\255\005\001\255\255\255\255\255\255\
\255\255\255\255\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\001\001\255\255\003\001\001\001\005\001\003\001\
\255\255\005\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\019\001\020\001\005\001\019\001\
\020\001\255\255\025\001\026\001\255\255\025\001\026\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\005\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\014\001\015\001\016\001\017\001\018\001\255\255\
\255\255\021\001\022\001\023\001\024\001"

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
  CHAR_LITERAL\000\
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
# 399 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                   ( ([], [])  )
# 405 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 58 "parser.mly"
                    (((_2 :: fst _1), snd _1))
# 413 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 65 "parser.mly"
                     ((fst _1, (_2 :: snd _1)))
# 421 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 78 "parser.mly"
               ( (_1, _2) )
# 429 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
           ( Int   )
# 435 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
           ( Float )
# 441 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
           ( Char  )
# 447 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
           ( String)
# 453 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
           ( Bool  )
# 459 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
           ( Void  )
# 465 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'opt_formals) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 134 "parser.mly"
                                                                ({
      return_type       = _1;
		func_identifier   = _3;
		func_formals      = List.rev _5;
		func_stmts        = List.rev _8;
   })
# 480 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
                 ([])
# 486 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 151 "parser.mly"
                  (List.rev _1 )
# 493 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 156 "parser.mly"
           ( [(_1, _2)] )
# 501 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 158 "parser.mly"
                               ( (_3, _4) :: _1 )
# 510 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 164 "parser.mly"
   ( [] )
# 516 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 165 "parser.mly"
                    ( _2 :: _1 )
# 524 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
             (Expr _1)
# 531 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 169 "parser.mly"
                                    (Return _2)
# 538 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 170 "parser.mly"
                                          ( Block(List.rev _2) )
# 545 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 171 "parser.mly"
                                           ( If(_3, _5, Block([])) )
# 553 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 172 "parser.mly"
                                             ( If(_3, _5, _7)        )
# 562 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 174 "parser.mly"
                                             ( For(_3, _5, _7, _9)   )
# 572 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "parser.mly"
                  ( Noexpr )
# 578 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
                   ( _1 )
# 585 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 181 "parser.mly"
                        ( IntLit(_1) )
# 592 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 182 "parser.mly"
                           ( FloatLit(_1) )
# 599 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 183 "parser.mly"
                         ( CharLit(_1) )
# 606 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 184 "parser.mly"
                          ( StringLit(_1) )
# 613 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 185 "parser.mly"
                         ( BoolLit(_1) )
# 620 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 186 "parser.mly"
                    ( Id(_1) )
# 627 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "parser.mly"
                          ( Binop(_1, Add, _3) )
# 635 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "parser.mly"
                          ( Binop(_1, Sub, _3) )
# 643 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 189 "parser.mly"
                          ( Binop(_1, Mult, _3) )
# 651 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 190 "parser.mly"
                           ( Binop(_1, Div, _3) )
# 659 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 191 "parser.mly"
                         ( Binop(_1, Pow, _3) )
# 667 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "parser.mly"
                         ( Binop(_1, Equal, _3) )
# 675 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 193 "parser.mly"
                         ( Binop(_1, Neq, _3) )
# 683 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 194 "parser.mly"
                         ( Binop(_1, Less, _3) )
# 691 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 195 "parser.mly"
                         ( Binop(_1, Leq, _3) )
# 699 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 196 "parser.mly"
                         ( Binop(_1, Greater, _3) )
# 707 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 197 "parser.mly"
                         ( Binop(_1, Geq, _3) )
# 715 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 198 "parser.mly"
                         ( Binop(_1, And, _3) )
# 723 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 199 "parser.mly"
                         ( Binop(_1, Or, _3) )
# 731 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 200 "parser.mly"
                               ( Unop(Neg, _2) )
# 738 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 201 "parser.mly"
                      ( Unop(Not, _2) )
# 745 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 202 "parser.mly"
                       ( Assign(_1, _3) )
# 753 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 203 "parser.mly"
                             ( _2 )
# 760 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 205 "parser.mly"
                                       (FunctionCall(_1, _3))
# 768 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 272 "parser.mly"
                  ( [] )
# 774 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 273 "parser.mly"
               ( List.rev _1 )
# 781 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 276 "parser.mly"
                            ( [_1] )
# 788 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 277 "parser.mly"
                         ( _3 :: _1 )
# 796 "parser.ml"
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
