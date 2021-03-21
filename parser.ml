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
\005\000\005\000\005\000\004\000\006\000\008\000\008\000\009\000\
\009\000\007\000\007\000\010\000\010\000\010\000\010\000\010\000\
\010\000\012\000\012\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\007\000\003\000\000\000\001\000\002\000\
\004\000\000\000\002\000\002\000\003\000\003\000\005\000\007\000\
\009\000\000\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\003\000\
\003\000\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\055\000\000\000\010\000\006\000\007\000\008\000\
\009\000\011\000\001\000\003\000\004\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\000\000\000\000\000\000\018\000\
\016\000\013\000\000\000\000\000\000\000\000\000\018\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\029\000\
\030\000\031\000\032\000\019\000\000\000\017\000\000\000\000\000\
\047\000\046\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\022\000\000\000\
\000\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\036\000\037\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\050\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\025\000"

let yydgoto = "\002\000\
\003\000\004\000\012\000\013\000\014\000\020\000\028\000\022\000\
\023\000\044\000\045\000\054\000\076\000\077\000"

let yysindex = "\008\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\225\254\223\254\023\255\
\044\255\000\000\098\255\043\255\003\255\056\255\062\255\000\000\
\000\000\000\000\098\255\002\255\019\255\067\255\000\000\000\000\
\067\255\067\255\069\255\072\255\067\255\015\255\000\000\000\000\
\000\000\000\000\000\000\000\000\156\255\000\000\094\000\037\255\
\000\000\000\000\067\255\067\255\246\255\065\255\067\255\067\255\
\000\000\067\255\067\255\067\255\067\255\067\255\067\255\067\255\
\067\255\067\255\067\255\067\255\067\255\000\000\000\000\107\000\
\076\255\000\000\246\255\085\255\093\255\246\255\253\254\253\254\
\000\000\000\000\143\000\143\000\152\255\152\255\152\255\152\255\
\132\000\120\000\094\255\067\255\000\000\067\255\060\255\173\255\
\246\255\094\255\067\255\000\000\099\255\094\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\103\255\000\000\000\000\000\000\106\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\102\255\139\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\102\255\007\255\000\000\107\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\081\255\000\000\114\255\136\255\199\255\225\255\
\000\000\000\000\078\000\081\000\233\255\018\000\044\000\052\000\
\080\255\205\255\000\000\000\000\000\000\000\000\059\255\000\000\
\084\255\000\000\056\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\028\000\000\000\089\000\000\000\
\000\000\181\255\226\255\206\255\000\000\000\000"

let yytablesize = 423
let yytable = "\047\000\
\011\000\073\000\049\000\050\000\015\000\030\000\053\000\027\000\
\001\000\031\000\032\000\027\000\060\000\061\000\033\000\095\000\
\034\000\017\000\055\000\016\000\072\000\053\000\100\000\018\000\
\075\000\078\000\103\000\079\000\080\000\081\000\082\000\083\000\
\084\000\085\000\086\000\087\000\088\000\089\000\090\000\035\000\
\030\000\056\000\036\000\037\000\031\000\071\000\021\000\019\000\
\101\000\033\000\024\000\034\000\038\000\025\000\029\000\039\000\
\040\000\041\000\042\000\043\000\026\000\096\000\023\000\097\000\
\027\000\074\000\023\000\023\000\053\000\046\000\030\000\023\000\
\051\000\023\000\035\000\052\000\092\000\036\000\037\000\033\000\
\044\000\034\000\044\000\053\000\044\000\053\000\054\000\038\000\
\054\000\093\000\039\000\040\000\041\000\042\000\043\000\094\000\
\023\000\030\000\098\000\023\000\023\000\031\000\026\000\102\000\
\044\000\044\000\033\000\014\000\034\000\023\000\015\000\051\000\
\023\000\023\000\023\000\023\000\023\000\038\000\052\000\048\000\
\039\000\040\000\041\000\042\000\043\000\005\000\006\000\007\000\
\008\000\009\000\000\000\035\000\010\000\000\000\036\000\037\000\
\048\000\000\000\048\000\033\000\048\000\033\000\000\000\033\000\
\038\000\000\000\000\000\039\000\040\000\041\000\042\000\043\000\
\033\000\033\000\033\000\033\000\057\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\058\000\059\000\060\000\
\061\000\058\000\059\000\060\000\061\000\099\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\000\000\000\000\
\000\000\000\000\058\000\059\000\060\000\061\000\000\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\034\000\
\000\000\034\000\000\000\034\000\000\000\045\000\000\000\045\000\
\000\000\045\000\000\000\000\000\034\000\034\000\000\000\000\000\
\000\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\035\000\000\000\035\000\000\000\035\000\045\000\000\000\
\000\000\040\000\000\000\040\000\000\000\040\000\035\000\035\000\
\000\000\000\000\000\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\035\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\058\000\059\000\060\000\061\000\000\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\000\000\000\000\042\000\000\000\042\000\000\000\042\000\000\000\
\000\000\000\000\000\000\000\000\005\000\006\000\007\000\008\000\
\009\000\000\000\000\000\010\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\041\000\000\000\041\000\000\000\
\041\000\000\000\000\000\000\000\043\000\000\000\043\000\000\000\
\043\000\000\000\000\000\000\000\000\000\000\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\038\000\000\000\
\038\000\039\000\038\000\039\000\000\000\039\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\038\000\038\000\070\000\039\000\039\000\000\000\038\000\038\000\
\000\000\039\000\039\000\058\000\059\000\060\000\061\000\091\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\058\000\059\000\060\000\061\000\000\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\058\000\059\000\060\000\
\061\000\000\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\058\000\059\000\060\000\061\000\000\000\062\000\063\000\
\064\000\065\000\066\000\067\000\058\000\059\000\060\000\061\000\
\000\000\000\000\000\000\064\000\065\000\066\000\067\000"

let yycheck = "\030\000\
\000\000\052\000\033\000\034\000\036\001\004\001\037\000\001\001\
\001\000\008\001\009\001\005\001\016\001\017\001\013\001\091\000\
\015\001\051\001\004\001\051\001\051\000\052\000\098\000\001\001\
\055\000\056\000\102\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\038\001\
\004\001\027\001\041\001\042\001\008\001\009\001\019\000\004\001\
\099\000\013\001\008\001\015\001\051\001\051\001\027\000\054\001\
\055\001\056\001\057\001\058\001\005\001\092\000\004\001\094\000\
\003\001\001\001\008\001\009\001\099\000\051\001\004\001\013\001\
\004\001\015\001\038\001\004\001\001\001\041\001\042\001\013\001\
\001\001\015\001\003\001\003\001\005\001\005\001\003\001\051\001\
\005\001\005\001\054\001\055\001\056\001\057\001\058\001\003\001\
\038\001\004\001\039\001\041\001\042\001\008\001\001\001\005\001\
\025\001\026\001\013\001\005\001\015\001\051\001\005\001\005\001\
\054\001\055\001\056\001\057\001\058\001\051\001\005\001\031\000\
\054\001\055\001\056\001\057\001\058\001\028\001\029\001\030\001\
\031\001\032\001\255\255\038\001\035\001\255\255\041\001\042\001\
\001\001\255\255\003\001\001\001\005\001\003\001\255\255\005\001\
\051\001\255\255\255\255\054\001\055\001\056\001\057\001\058\001\
\014\001\015\001\016\001\017\001\001\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\014\001\015\001\016\001\
\017\001\014\001\015\001\016\001\017\001\001\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\255\255\255\255\
\255\255\255\255\014\001\015\001\016\001\017\001\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\001\001\
\255\255\003\001\255\255\005\001\255\255\001\001\255\255\003\001\
\255\255\005\001\255\255\255\255\014\001\015\001\255\255\255\255\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\001\001\255\255\003\001\255\255\005\001\026\001\255\255\
\255\255\001\001\255\255\003\001\255\255\005\001\014\001\015\001\
\255\255\255\255\255\255\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\014\001\015\001\016\001\017\001\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\255\255\255\255\001\001\255\255\003\001\255\255\005\001\255\255\
\255\255\255\255\255\255\255\255\028\001\029\001\030\001\031\001\
\032\001\255\255\255\255\035\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\001\001\255\255\003\001\255\255\
\005\001\255\255\255\255\255\255\001\001\255\255\003\001\255\255\
\005\001\255\255\255\255\255\255\255\255\255\255\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\001\001\255\255\
\003\001\001\001\005\001\003\001\255\255\005\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\019\001\020\001\005\001\019\001\020\001\255\255\025\001\026\001\
\255\255\025\001\026\001\014\001\015\001\016\001\017\001\005\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\014\001\015\001\016\001\017\001\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\014\001\015\001\016\001\
\017\001\255\255\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\014\001\015\001\016\001\017\001\255\255\019\001\020\001\
\021\001\022\001\023\001\024\001\014\001\015\001\016\001\017\001\
\255\255\255\255\255\255\021\001\022\001\023\001\024\001"

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
# 387 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                   ( ([], [])  )
# 393 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 58 "parser.mly"
                    (((_2 :: fst _1), snd _1))
# 401 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 65 "parser.mly"
                     ((fst _1, (_2 :: snd _1)))
# 409 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 78 "parser.mly"
               ( (_1, _2) )
# 417 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
           ( Int   )
# 423 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
           ( Float )
# 429 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
           ( Char  )
# 435 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
           ( String)
# 441 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
           ( Bool  )
# 447 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
           ( Void  )
# 453 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'formals_block) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 134 "parser.mly"
                                                    ({
      return_type       = _1;
		func_identifier   = _3;
		func_formals      = List.rev _4;
		func_stmts        = List.rev _6;
   })
# 468 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'opt_formals) in
    Obj.repr(
# 144 "parser.mly"
                             ( _2 )
# 475 "parser.ml"
               : 'formals_block))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
                 ([])
# 481 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 151 "parser.mly"
                  (List.rev _1 )
# 488 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 156 "parser.mly"
           ( [(_1, _2)] )
# 496 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 158 "parser.mly"
                               ( (_3, _4) :: _1 )
# 505 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 164 "parser.mly"
   ( [] )
# 511 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 165 "parser.mly"
                    ( _2 :: _1 )
# 519 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
             (Expr _1)
# 526 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 169 "parser.mly"
                                    (Return _2)
# 533 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 170 "parser.mly"
                                          ( Block(List.rev _2) )
# 540 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 171 "parser.mly"
                                           ( If(_3, _5, Block([])) )
# 548 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 172 "parser.mly"
                                             ( If(_3, _5, _7)        )
# 557 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 174 "parser.mly"
                                             ( For(_3, _5, _7, _9)   )
# 567 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "parser.mly"
                  ( Noexpr )
# 573 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
                   ( _1 )
# 580 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 181 "parser.mly"
                        ( IntLit(_1) )
# 587 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 182 "parser.mly"
                           ( FloatLit(_1) )
# 594 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 183 "parser.mly"
                         ( CharLit(_1) )
# 601 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 184 "parser.mly"
                          ( StringLit(_1) )
# 608 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 185 "parser.mly"
                         ( BoolLit(_1) )
# 615 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 186 "parser.mly"
                    ( Id(_1) )
# 622 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "parser.mly"
                          ( Binop(_1, Add, _3) )
# 630 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "parser.mly"
                          ( Binop(_1, Sub, _3) )
# 638 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 189 "parser.mly"
                          ( Binop(_1, Mult, _3) )
# 646 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 190 "parser.mly"
                           ( Binop(_1, Div, _3) )
# 654 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "parser.mly"
                         ( Binop(_1, Equal, _3) )
# 662 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 193 "parser.mly"
                         ( Binop(_1, Neq, _3) )
# 670 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 194 "parser.mly"
                         ( Binop(_1, Less, _3) )
# 678 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 195 "parser.mly"
                         ( Binop(_1, Leq, _3) )
# 686 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 196 "parser.mly"
                         ( Binop(_1, Greater, _3) )
# 694 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 197 "parser.mly"
                         ( Binop(_1, Geq, _3) )
# 702 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 198 "parser.mly"
                         ( Binop(_1, And, _3) )
# 710 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 199 "parser.mly"
                         ( Binop(_1, Or, _3) )
# 718 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 200 "parser.mly"
                               ( Unop(Neg, _2) )
# 725 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 201 "parser.mly"
                      ( Unop(Not, _2) )
# 732 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 202 "parser.mly"
                       ( Assign(_1, _3) )
# 740 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 203 "parser.mly"
                             ( _2 )
# 747 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 205 "parser.mly"
                                       (FunctionCall(_1, _3))
# 755 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 272 "parser.mly"
                  ( [] )
# 761 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 273 "parser.mly"
               ( List.rev _1 )
# 768 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 276 "parser.mly"
                            ( [_1] )
# 775 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 277 "parser.mly"
                         ( _3 :: _1 )
# 783 "parser.ml"
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
