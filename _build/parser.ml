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
  | STRING_LITERAL of (string)
  | BOOL_LITERAL of (bool)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 66 "parser.ml"
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
  312 (* STRING_LITERAL *);
  313 (* BOOL_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\005\000\005\000\005\000\
\005\000\005\000\005\000\004\000\006\000\006\000\008\000\008\000\
\007\000\007\000\009\000\009\000\009\000\009\000\009\000\009\000\
\011\000\011\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\012\000\012\000\013\000\013\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\009\000\000\000\001\000\002\000\004\000\
\000\000\002\000\002\000\003\000\003\000\005\000\007\000\009\000\
\000\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\002\000\003\000\003\000\
\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\054\000\000\000\010\000\006\000\007\000\008\000\
\009\000\011\000\001\000\003\000\004\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\000\000\000\000\015\000\000\000\
\000\000\017\000\000\000\000\000\016\000\000\000\017\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\028\000\
\029\000\030\000\018\000\000\000\000\000\000\000\046\000\045\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\048\000\021\000\000\000\000\000\
\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\049\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\000\000\000\000\024\000"

let yydgoto = "\002\000\
\003\000\004\000\012\000\013\000\014\000\021\000\028\000\022\000\
\043\000\044\000\052\000\075\000\076\000"

let yysindex = "\007\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\226\254\227\254\040\255\
\043\255\000\000\137\255\002\255\049\255\064\255\000\000\060\255\
\137\255\000\000\024\255\001\255\000\000\102\255\000\000\000\000\
\102\255\102\255\072\255\093\255\102\255\254\254\000\000\000\000\
\000\000\000\000\000\000\202\255\126\000\036\255\000\000\000\000\
\102\255\102\255\161\000\109\255\102\255\102\255\000\000\102\255\
\102\255\102\255\102\255\102\255\102\255\102\255\102\255\102\255\
\102\255\102\255\102\255\102\255\000\000\000\000\148\000\115\255\
\000\000\161\000\077\255\117\255\161\000\084\255\084\255\087\255\
\087\255\000\000\185\000\185\000\177\255\177\255\177\255\177\255\
\174\000\216\255\081\255\102\255\000\000\102\255\079\255\228\255\
\161\000\081\255\102\255\000\000\116\255\081\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\119\255\000\000\000\000\120\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\126\255\125\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\126\255\058\255\000\000\124\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\083\255\000\000\128\255\012\255\254\255\034\000\159\255\
\185\255\000\000\110\000\113\000\042\000\068\000\076\000\102\000\
\078\255\045\255\000\000\000\000\000\000\000\000\057\255\000\000\
\104\255\000\000\129\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\249\255\000\000\100\000\000\000\
\238\255\226\255\217\255\000\000\000\000"

let yytablesize = 465
let yytable = "\045\000\
\011\000\053\000\047\000\048\000\030\000\015\000\051\000\001\000\
\031\000\032\000\072\000\020\000\047\000\033\000\047\000\034\000\
\047\000\027\000\071\000\051\000\016\000\017\000\074\000\077\000\
\054\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\035\000\030\000\
\018\000\036\000\037\000\031\000\070\000\044\000\019\000\044\000\
\033\000\044\000\034\000\038\000\023\000\024\000\039\000\040\000\
\041\000\042\000\026\000\101\000\022\000\096\000\026\000\097\000\
\022\000\022\000\025\000\026\000\051\000\022\000\044\000\022\000\
\095\000\035\000\029\000\049\000\036\000\037\000\043\000\100\000\
\043\000\093\000\043\000\103\000\030\000\052\000\038\000\052\000\
\031\000\039\000\040\000\041\000\042\000\033\000\022\000\034\000\
\050\000\022\000\022\000\058\000\059\000\060\000\043\000\043\000\
\060\000\030\000\053\000\022\000\053\000\073\000\022\000\022\000\
\022\000\022\000\033\000\092\000\034\000\098\000\035\000\094\000\
\102\000\036\000\037\000\013\000\014\000\031\000\025\000\031\000\
\050\000\031\000\046\000\038\000\051\000\025\000\039\000\040\000\
\041\000\042\000\031\000\031\000\031\000\031\000\031\000\031\000\
\031\000\031\000\031\000\031\000\031\000\031\000\031\000\000\000\
\038\000\000\000\000\000\039\000\040\000\041\000\042\000\034\000\
\000\000\034\000\000\000\034\000\005\000\006\000\007\000\008\000\
\009\000\000\000\000\000\010\000\034\000\034\000\034\000\034\000\
\000\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\035\000\000\000\035\000\000\000\035\000\056\000\057\000\
\058\000\059\000\060\000\000\000\000\000\000\000\035\000\035\000\
\035\000\035\000\055\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\035\000\000\000\000\000\000\000\000\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\099\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\032\000\000\000\
\032\000\000\000\032\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\032\000\032\000\000\000\000\000\000\000\
\032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\000\000\000\000\000\000\000\000\005\000\006\000\007\000\008\000\
\009\000\000\000\033\000\010\000\033\000\000\000\033\000\000\000\
\000\000\000\000\039\000\000\000\039\000\000\000\039\000\033\000\
\033\000\000\000\000\000\000\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\039\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\041\000\000\000\041\000\000\000\
\041\000\000\000\000\000\000\000\040\000\000\000\040\000\000\000\
\040\000\000\000\000\000\000\000\000\000\000\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\042\000\000\000\
\042\000\000\000\042\000\000\000\000\000\000\000\037\000\000\000\
\037\000\038\000\037\000\038\000\000\000\038\000\000\000\000\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\037\000\037\000\069\000\038\000\038\000\000\000\037\000\037\000\
\000\000\038\000\038\000\056\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\091\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\056\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\056\000\057\000\
\058\000\059\000\060\000\000\000\000\000\063\000\064\000\065\000\
\066\000"

let yycheck = "\030\000\
\000\000\004\001\033\000\034\000\004\001\036\001\037\000\001\000\
\008\001\009\001\050\000\019\000\001\001\013\001\003\001\015\001\
\005\001\025\000\049\000\050\000\051\001\051\001\053\000\054\000\
\027\001\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\038\001\004\001\
\001\001\041\001\042\001\008\001\009\001\001\001\004\001\003\001\
\013\001\005\001\015\001\051\001\051\001\005\001\054\001\055\001\
\056\001\057\001\001\001\099\000\004\001\092\000\005\001\094\000\
\008\001\009\001\003\001\008\001\099\000\013\001\026\001\015\001\
\091\000\038\001\051\001\004\001\041\001\042\001\001\001\098\000\
\003\001\005\001\005\001\102\000\004\001\003\001\051\001\005\001\
\008\001\054\001\055\001\056\001\057\001\013\001\038\001\015\001\
\004\001\041\001\042\001\016\001\017\001\018\001\025\001\026\001\
\018\001\004\001\003\001\051\001\005\001\001\001\054\001\055\001\
\056\001\057\001\013\001\001\001\015\001\039\001\038\001\003\001\
\005\001\041\001\042\001\005\001\005\001\001\001\001\001\003\001\
\005\001\005\001\031\000\051\001\005\001\005\001\054\001\055\001\
\056\001\057\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\255\255\
\051\001\255\255\255\255\054\001\055\001\056\001\057\001\001\001\
\255\255\003\001\255\255\005\001\028\001\029\001\030\001\031\001\
\032\001\255\255\255\255\035\001\014\001\015\001\016\001\017\001\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\001\001\255\255\003\001\255\255\005\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\001\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\255\255\255\255\255\255\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\001\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\001\001\255\255\
\003\001\255\255\005\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\014\001\015\001\255\255\255\255\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\255\255\255\255\255\255\255\255\028\001\029\001\030\001\031\001\
\032\001\255\255\001\001\035\001\003\001\255\255\005\001\255\255\
\255\255\255\255\001\001\255\255\003\001\255\255\005\001\014\001\
\015\001\255\255\255\255\255\255\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\001\001\255\255\003\001\255\255\
\005\001\255\255\255\255\255\255\001\001\255\255\003\001\255\255\
\005\001\255\255\255\255\255\255\255\255\255\255\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\001\001\255\255\
\003\001\255\255\005\001\255\255\255\255\255\255\001\001\255\255\
\003\001\001\001\005\001\003\001\255\255\005\001\255\255\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\019\001\020\001\005\001\019\001\020\001\255\255\025\001\026\001\
\255\255\025\001\026\001\014\001\015\001\016\001\017\001\018\001\
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
# 53 "parser.mly"
             ( _1 )
# 396 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
                   ( ([], [])  )
# 402 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 57 "parser.mly"
                    (((_2 :: fst _1), snd _1))
# 410 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 64 "parser.mly"
                     ((fst _1, (_2 :: snd _1)))
# 418 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 77 "parser.mly"
               ( (_1, _2) )
# 426 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
           ( Int   )
# 432 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
           ( Float )
# 438 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
           ( Char  )
# 444 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
           ( String)
# 450 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
           ( Bool  )
# 456 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
           ( Void  )
# 462 "parser.ml"
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
# 477 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 146 "parser.mly"
                 ([])
# 483 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 150 "parser.mly"
                  (List.rev _1 )
# 490 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 155 "parser.mly"
           ( [(_1, _2)] )
# 498 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 157 "parser.mly"
                               ( (_3, _4) :: _1 )
# 507 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 163 "parser.mly"
   ( [] )
# 513 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 164 "parser.mly"
                    ( _2 :: _1 )
# 521 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 167 "parser.mly"
             (Expr _1)
# 528 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 168 "parser.mly"
                                    (Return _2)
# 535 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 169 "parser.mly"
                                          ( Block(List.rev _2) )
# 542 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 170 "parser.mly"
                                           ( If(_3, _5, Block([])) )
# 550 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 171 "parser.mly"
                                             ( If(_3, _5, _7)        )
# 559 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 173 "parser.mly"
                                             ( For(_3, _5, _7, _9)   )
# 569 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 176 "parser.mly"
                  ( Noexpr )
# 575 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 177 "parser.mly"
                   ( _1 )
# 582 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 180 "parser.mly"
                        ( IntLit(_1) )
# 589 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 181 "parser.mly"
                           ( FloatLit(_1) )
# 596 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 182 "parser.mly"
                                 ( StringLit(_1) )
# 603 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 183 "parser.mly"
                         ( BoolLit(_1) )
# 610 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 184 "parser.mly"
                    ( Id(_1) )
# 617 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 185 "parser.mly"
                          ( Binop(_1, Add, _3) )
# 625 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 186 "parser.mly"
                          ( Binop(_1, Sub, _3) )
# 633 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "parser.mly"
                          ( Binop(_1, Mult, _3) )
# 641 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "parser.mly"
                           ( Binop(_1, Div, _3) )
# 649 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 189 "parser.mly"
                         ( Binop(_1, Pow, _3) )
# 657 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 190 "parser.mly"
                         ( Binop(_1, Equal, _3) )
# 665 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 191 "parser.mly"
                         ( Binop(_1, Neq, _3) )
# 673 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "parser.mly"
                         ( Binop(_1, Less, _3) )
# 681 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 193 "parser.mly"
                         ( Binop(_1, Leq, _3) )
# 689 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 194 "parser.mly"
                         ( Binop(_1, Greater, _3) )
# 697 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 195 "parser.mly"
                         ( Binop(_1, Geq, _3) )
# 705 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 196 "parser.mly"
                         ( Binop(_1, And, _3) )
# 713 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 197 "parser.mly"
                         ( Binop(_1, Or, _3) )
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 198 "parser.mly"
                               ( Unop(Neg, _2) )
# 728 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 199 "parser.mly"
                      ( Unop(Not, _2) )
# 735 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 200 "parser.mly"
                       ( Assign(_1, _3) )
# 743 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 201 "parser.mly"
                             ( _2 )
# 750 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 203 "parser.mly"
                                       (FunctionCall(_1, _3))
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 270 "parser.mly"
                  ( [] )
# 764 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 271 "parser.mly"
               ( List.rev _1 )
# 771 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 274 "parser.mly"
                            ( [_1] )
# 778 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 275 "parser.mly"
                         ( _3 :: _1 )
# 786 "parser.ml"
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
