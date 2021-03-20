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
  291 (* FUNC *);
  292 (* EQUA *);
  293 (* IF *);
  294 (* ELSE *);
  295 (* NOELSE *);
  296 (* FOR *);
  297 (* RETURN *);
  298 (* METER *);
  299 (* SEC *);
  300 (* KGRAM *);
  301 (* AMP *);
  302 (* CMETER *);
  303 (* HERTZ *);
  304 (* GRAM *);
  305 (* NEWTON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  306 (* ID *);
  307 (* UID *);
  308 (* UONE *);
  309 (* INT_LITERAL *);
  310 (* FLOAT_LITERAL *);
  311 (* CHAR_LITERAL *);
  312 (* STRING_LITERAL *);
  313 (* BOOL_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\005\000\005\000\005\000\
\005\000\005\000\004\000\006\000\008\000\008\000\009\000\009\000\
\007\000\010\000\010\000\011\000\011\000\011\000\011\000\011\000\
\013\000\013\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\014\000\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\005\000\003\000\000\000\001\000\002\000\004\000\
\003\000\000\000\002\000\002\000\003\000\005\000\007\000\009\000\
\000\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\003\000\
\003\000\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\055\000\000\000\010\000\006\000\007\000\008\000\
\009\000\001\000\003\000\004\000\000\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\018\000\011\000\
\015\000\012\000\000\000\000\000\000\000\000\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\028\000\029\000\
\030\000\031\000\019\000\000\000\016\000\000\000\047\000\046\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\049\000\000\000\000\000\021\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\037\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\050\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\000\000\000\000\024\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\013\000\019\000\024\000\021\000\
\022\000\028\000\043\000\044\000\052\000\074\000\075\000"

let yysindex = "\004\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\255\215\254\013\255\011\255\
\000\000\129\255\014\255\227\254\050\255\064\255\000\000\000\000\
\000\000\000\000\129\255\003\255\025\255\066\255\000\000\066\255\
\066\255\084\255\085\255\066\255\021\255\000\000\000\000\000\000\
\000\000\000\000\000\000\192\255\000\000\104\000\000\000\000\000\
\066\255\066\255\139\000\095\255\066\255\066\255\000\000\066\255\
\066\255\066\255\066\255\066\255\066\255\066\255\066\255\066\255\
\066\255\066\255\066\255\066\255\000\000\126\000\096\255\000\000\
\139\000\104\255\107\255\139\000\170\255\170\255\099\255\099\255\
\000\000\163\000\163\000\167\255\167\255\167\255\167\255\152\000\
\206\255\058\255\066\255\000\000\066\255\087\255\218\255\139\000\
\058\255\066\255\000\000\113\255\058\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\122\255\000\000\000\000\000\000\124\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\131\255\123\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\131\255\041\255\000\000\125\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\061\255\000\000\146\255\007\255\244\255\020\000\149\255\175\255\
\000\000\088\000\091\000\081\255\046\000\054\000\080\000\130\255\
\046\255\000\000\000\000\000\000\000\000\037\255\000\000\080\255\
\000\000\148\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\242\255\000\000\000\000\000\000\
\000\000\000\000\235\255\226\255\223\255\000\000\000\000"

let yytablesize = 443
let yytable = "\046\000\
\010\000\047\000\048\000\020\000\001\000\051\000\030\000\048\000\
\016\000\048\000\031\000\048\000\029\000\017\000\018\000\032\000\
\071\000\033\000\070\000\051\000\025\000\023\000\073\000\076\000\
\053\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\084\000\085\000\086\000\087\000\088\000\089\000\014\000\034\000\
\022\000\026\000\035\000\036\000\022\000\026\000\045\000\054\000\
\045\000\022\000\045\000\022\000\037\000\015\000\026\000\038\000\
\039\000\040\000\041\000\042\000\095\000\030\000\096\000\053\000\
\100\000\053\000\027\000\051\000\094\000\030\000\032\000\045\000\
\033\000\022\000\045\000\099\000\022\000\022\000\032\000\102\000\
\033\000\040\000\054\000\040\000\054\000\040\000\022\000\049\000\
\050\000\022\000\022\000\022\000\022\000\022\000\034\000\072\000\
\091\000\035\000\036\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\037\000\092\000\093\000\038\000\039\000\
\040\000\041\000\042\000\037\000\060\000\101\000\038\000\039\000\
\040\000\041\000\042\000\032\000\097\000\032\000\013\000\032\000\
\014\000\051\000\044\000\025\000\044\000\000\000\044\000\000\000\
\032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\032\000\032\000\032\000\032\000\035\000\052\000\035\000\
\025\000\035\000\044\000\044\000\005\000\006\000\007\000\008\000\
\009\000\000\000\035\000\035\000\035\000\035\000\000\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\035\000\036\000\
\000\000\036\000\000\000\036\000\056\000\057\000\058\000\059\000\
\060\000\058\000\059\000\060\000\036\000\036\000\036\000\036\000\
\055\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\000\000\000\000\000\000\000\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\098\000\056\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\033\000\000\000\033\000\000\000\
\033\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\033\000\000\000\000\000\000\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\000\000\034\000\000\000\
\034\000\000\000\000\000\000\000\005\000\006\000\007\000\008\000\
\009\000\034\000\034\000\000\000\000\000\000\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\042\000\000\000\
\042\000\000\000\042\000\000\000\000\000\000\000\041\000\000\000\
\041\000\000\000\041\000\000\000\000\000\000\000\000\000\000\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\043\000\000\000\043\000\000\000\043\000\000\000\000\000\000\000\
\038\000\000\000\038\000\039\000\038\000\039\000\000\000\039\000\
\000\000\000\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\043\000\038\000\038\000\069\000\039\000\039\000\000\000\
\038\000\038\000\000\000\039\000\039\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\090\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\056\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\056\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\056\000\057\000\058\000\059\000\060\000\000\000\000\000\063\000\
\064\000\065\000\066\000"

let yycheck = "\030\000\
\000\000\032\000\033\000\018\000\001\000\036\000\004\001\001\001\
\050\001\003\001\008\001\005\001\027\000\001\001\004\001\013\001\
\050\000\015\001\049\000\050\000\050\001\008\001\053\000\054\000\
\004\001\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\035\001\037\001\
\004\001\001\001\040\001\041\001\008\001\005\001\001\001\027\001\
\003\001\013\001\005\001\015\001\050\001\050\001\005\001\053\001\
\054\001\055\001\056\001\057\001\091\000\004\001\093\000\003\001\
\098\000\005\001\003\001\098\000\090\000\004\001\013\001\026\001\
\015\001\037\001\050\001\097\000\040\001\041\001\013\001\101\000\
\015\001\001\001\003\001\003\001\005\001\005\001\050\001\004\001\
\004\001\053\001\054\001\055\001\056\001\057\001\037\001\001\001\
\001\001\040\001\041\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\050\001\005\001\003\001\053\001\054\001\
\055\001\056\001\057\001\050\001\018\001\005\001\053\001\054\001\
\055\001\056\001\057\001\001\001\038\001\003\001\005\001\005\001\
\005\001\005\001\001\001\001\001\003\001\255\255\005\001\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\001\001\005\001\003\001\
\005\001\005\001\025\001\026\001\028\001\029\001\030\001\031\001\
\032\001\255\255\014\001\015\001\016\001\017\001\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\001\001\
\255\255\003\001\255\255\005\001\014\001\015\001\016\001\017\001\
\018\001\016\001\017\001\018\001\014\001\015\001\016\001\017\001\
\001\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\255\255\255\255\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\001\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\001\001\255\255\003\001\255\255\
\005\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\014\001\015\001\255\255\255\255\255\255\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\255\255\255\255\
\255\255\255\255\255\255\255\255\001\001\255\255\003\001\255\255\
\005\001\255\255\255\255\255\255\028\001\029\001\030\001\031\001\
\032\001\014\001\015\001\255\255\255\255\255\255\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\001\001\255\255\
\003\001\255\255\005\001\255\255\255\255\255\255\001\001\255\255\
\003\001\255\255\005\001\255\255\255\255\255\255\255\255\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\001\001\255\255\003\001\255\255\005\001\255\255\255\255\255\255\
\001\001\255\255\003\001\001\001\005\001\003\001\255\255\005\001\
\255\255\255\255\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\019\001\020\001\005\001\019\001\020\001\255\255\
\025\001\026\001\255\255\025\001\026\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\005\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\014\001\015\001\016\001\017\001\018\001\255\255\255\255\021\001\
\022\001\023\001\024\001"

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
# 390 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                   ( {vars=[]; units=[]; funcs=[]; equas=[];} )
# 396 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 58 "parser.mly"
                    ({
                  vars = _2 :: _1.vars;
						units = _1.units;
						funcs = _1.funcs;
						equas = _1.equas;
                  })
# 409 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 70 "parser.mly"
                     ({vars = _1.vars;
						units = _1.units;
						funcs = _2 :: _1.funcs;
						equas = _1.equas;})
# 420 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 86 "parser.mly"
               ( (_1, NOUNIT, _2) )
# 428 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
           ( Int   )
# 434 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
           ( Float )
# 440 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
           ( Char  )
# 446 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
           ( String)
# 452 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
           ( Bool  )
# 458 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'formals_block) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_block) in
    Obj.repr(
# 142 "parser.mly"
                                        ({
      return_type       = _1;
		func_identifier   = _3;
		func_formals      = List.rev _4;
		func_stmts        = List.rev _5;
   })
# 473 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'opt_formals) in
    Obj.repr(
# 152 "parser.mly"
                             ( _2 )
# 480 "parser.ml"
               : 'formals_block))
; (fun __caml_parser_env ->
    Obj.repr(
# 155 "parser.mly"
                 ([])
# 486 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 159 "parser.mly"
                  (List.rev _1 )
# 493 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 164 "parser.mly"
           ( [(_1, _2)] )
# 501 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 166 "parser.mly"
                               ( (_3, _4) :: _1 )
# 510 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 170 "parser.mly"
                           ( Block(List.rev _2) )
# 517 "parser.ml"
               : 'stmt_block))
; (fun __caml_parser_env ->
    Obj.repr(
# 174 "parser.mly"
   ( [] )
# 523 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 175 "parser.mly"
                    ( _2 :: _1 )
# 531 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
             (Expr _1)
# 538 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 179 "parser.mly"
                                    (Return _2)
# 545 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 180 "parser.mly"
                                           ( If(_3, _5, Block([])) )
# 553 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 181 "parser.mly"
                                             ( If(_3, _5, _7)        )
# 562 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 183 "parser.mly"
                                             ( For(_3, _5, _7, _9)   )
# 572 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 186 "parser.mly"
                  ( Noexpr )
# 578 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "parser.mly"
                   ( _1 )
# 585 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 190 "parser.mly"
                        ( IntLit(_1) )
# 592 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 191 "parser.mly"
                           ( FloatLit(_1) )
# 599 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 192 "parser.mly"
                         ( CharLit(_1) )
# 606 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 193 "parser.mly"
                          ( StringLit(_1) )
# 613 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 194 "parser.mly"
                         ( BoolLit(_1) )
# 620 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 195 "parser.mly"
                    ( Id(_1) )
# 627 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 196 "parser.mly"
                          ( Binop(_1, Add, _3) )
# 635 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 197 "parser.mly"
                          ( Binop(_1, Sub, _3) )
# 643 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 198 "parser.mly"
                          ( Binop(_1, Mult, _3) )
# 651 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 199 "parser.mly"
                           ( Binop(_1, Div, _3) )
# 659 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 200 "parser.mly"
                         ( Binop(_1, Pow, _3) )
# 667 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 201 "parser.mly"
                         ( Binop(_1, Equal, _3) )
# 675 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 202 "parser.mly"
                         ( Binop(_1, Neq, _3) )
# 683 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 203 "parser.mly"
                         ( Binop(_1, Less, _3) )
# 691 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 204 "parser.mly"
                         ( Binop(_1, Leq, _3) )
# 699 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 205 "parser.mly"
                         ( Binop(_1, Greater, _3) )
# 707 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 206 "parser.mly"
                         ( Binop(_1, Geq, _3) )
# 715 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 207 "parser.mly"
                         ( Binop(_1, And, _3) )
# 723 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 208 "parser.mly"
                         ( Binop(_1, Or, _3) )
# 731 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 209 "parser.mly"
                               ( Unop(Neg, _2) )
# 738 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 210 "parser.mly"
                      ( Unop(Not, _2) )
# 745 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 211 "parser.mly"
                       ( Assign(_1, _3) )
# 753 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 212 "parser.mly"
                             ( _2 )
# 760 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 214 "parser.mly"
                                       (FunctionCall(_1, _3))
# 768 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 281 "parser.mly"
                  ( [] )
# 774 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 282 "parser.mly"
               ( List.rev _1 )
# 781 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 285 "parser.mly"
                            ( [_1] )
# 788 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 286 "parser.mly"
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
