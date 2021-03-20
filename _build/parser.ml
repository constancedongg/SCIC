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
\009\000\007\000\010\000\010\000\011\000\011\000\011\000\011\000\
\011\000\013\000\013\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\014\000\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\005\000\003\000\000\000\001\000\002\000\
\004\000\003\000\000\000\002\000\002\000\003\000\005\000\007\000\
\009\000\000\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\003\000\003\000\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\056\000\000\000\010\000\006\000\007\000\008\000\
\009\000\011\000\001\000\003\000\004\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\000\000\000\000\000\000\019\000\
\012\000\016\000\013\000\000\000\000\000\000\000\000\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\029\000\
\030\000\031\000\032\000\020\000\000\000\017\000\000\000\048\000\
\047\000\000\000\000\000\000\000\000\000\000\000\000\000\021\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\050\000\000\000\000\000\
\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\038\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\051\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\025\000"

let yydgoto = "\002\000\
\003\000\004\000\012\000\013\000\014\000\020\000\025\000\022\000\
\023\000\029\000\044\000\045\000\053\000\075\000\076\000"

let yysindex = "\024\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\227\254\226\254\046\255\
\049\255\000\000\128\255\067\255\021\255\071\255\080\255\000\000\
\000\000\000\000\000\000\128\255\001\255\034\255\066\255\000\000\
\066\255\066\255\082\255\084\255\066\255\014\255\000\000\000\000\
\000\000\000\000\000\000\000\000\193\255\000\000\130\000\000\000\
\000\000\066\255\066\255\165\000\088\255\066\255\066\255\000\000\
\066\255\066\255\066\255\066\255\066\255\066\255\066\255\066\255\
\066\255\066\255\066\255\066\255\066\255\000\000\152\000\094\255\
\000\000\165\000\092\255\095\255\165\000\048\255\048\255\083\255\
\083\255\000\000\189\000\189\000\118\255\118\255\118\255\118\255\
\178\000\207\255\058\255\066\255\000\000\066\255\063\255\219\255\
\165\000\058\255\066\255\000\000\098\255\058\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\102\255\000\000\000\000\000\000\106\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\117\255\124\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\117\255\012\255\000\000\114\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\255\000\000\121\255\045\255\245\255\020\000\150\255\
\176\255\000\000\114\000\117\000\046\000\054\000\080\000\088\000\
\079\255\105\255\000\000\000\000\000\000\000\000\036\255\000\000\
\064\255\000\000\123\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\026\000\000\000\000\000\000\000\
\000\000\000\000\169\255\225\255\217\255\000\000\000\000"

let yytablesize = 469
let yytable = "\047\000\
\011\000\048\000\049\000\095\000\031\000\052\000\015\000\054\000\
\032\000\054\000\100\000\072\000\027\000\033\000\103\000\034\000\
\027\000\054\000\071\000\052\000\017\000\016\000\074\000\077\000\
\001\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\035\000\023\000\
\055\000\036\000\037\000\023\000\021\000\049\000\018\000\049\000\
\023\000\049\000\023\000\038\000\019\000\030\000\039\000\040\000\
\041\000\042\000\043\000\101\000\096\000\031\000\097\000\059\000\
\060\000\061\000\055\000\052\000\055\000\031\000\033\000\026\000\
\034\000\023\000\024\000\027\000\023\000\023\000\033\000\045\000\
\034\000\045\000\028\000\045\000\046\000\050\000\023\000\051\000\
\073\000\023\000\023\000\023\000\023\000\023\000\092\000\035\000\
\093\000\094\000\036\000\037\000\061\000\098\000\102\000\045\000\
\045\000\046\000\014\000\046\000\038\000\046\000\015\000\039\000\
\040\000\041\000\042\000\043\000\038\000\026\000\052\000\039\000\
\040\000\041\000\042\000\043\000\033\000\053\000\033\000\026\000\
\033\000\000\000\046\000\057\000\058\000\059\000\060\000\061\000\
\000\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\036\000\000\000\
\036\000\000\000\036\000\005\000\006\000\007\000\008\000\009\000\
\000\000\000\000\010\000\036\000\036\000\036\000\036\000\000\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\037\000\000\000\037\000\000\000\037\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\037\000\037\000\037\000\
\037\000\056\000\037\000\037\000\037\000\037\000\037\000\037\000\
\037\000\037\000\000\000\000\000\000\000\000\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\099\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\034\000\000\000\034\000\
\000\000\034\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\034\000\034\000\000\000\000\000\000\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\034\000\000\000\
\000\000\000\000\000\000\000\000\035\000\000\000\035\000\000\000\
\035\000\000\000\000\000\000\000\005\000\006\000\007\000\008\000\
\009\000\035\000\035\000\010\000\000\000\000\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\041\000\000\000\
\041\000\000\000\041\000\000\000\000\000\000\000\043\000\000\000\
\043\000\000\000\043\000\000\000\000\000\000\000\000\000\000\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
\042\000\000\000\042\000\000\000\042\000\000\000\000\000\000\000\
\044\000\000\000\044\000\000\000\044\000\000\000\000\000\000\000\
\000\000\000\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\039\000\000\000\039\000\040\000\039\000\040\000\
\000\000\040\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\039\000\070\000\040\000\
\040\000\000\000\039\000\039\000\000\000\040\000\040\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\091\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\057\000\058\000\059\000\060\000\061\000\000\000\
\000\000\064\000\065\000\066\000\067\000"

let yycheck = "\031\000\
\000\000\033\000\034\000\091\000\004\001\037\000\036\001\003\001\
\008\001\005\001\098\000\051\000\001\001\013\001\102\000\015\001\
\005\001\004\001\050\000\051\000\051\001\051\001\054\000\055\000\
\001\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\038\001\004\001\
\027\001\041\001\042\001\008\001\019\000\001\001\001\001\003\001\
\013\001\005\001\015\001\051\001\004\001\028\000\054\001\055\001\
\056\001\057\001\058\001\099\000\092\000\004\001\094\000\016\001\
\017\001\018\001\003\001\099\000\005\001\004\001\013\001\051\001\
\015\001\038\001\008\001\005\001\041\001\042\001\013\001\001\001\
\015\001\003\001\003\001\005\001\051\001\004\001\051\001\004\001\
\001\001\054\001\055\001\056\001\057\001\058\001\001\001\038\001\
\005\001\003\001\041\001\042\001\018\001\039\001\005\001\025\001\
\026\001\001\001\005\001\003\001\051\001\005\001\005\001\054\001\
\055\001\056\001\057\001\058\001\051\001\001\001\005\001\054\001\
\055\001\056\001\057\001\058\001\001\001\005\001\003\001\005\001\
\005\001\255\255\026\001\014\001\015\001\016\001\017\001\018\001\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\001\001\255\255\
\003\001\255\255\005\001\028\001\029\001\030\001\031\001\032\001\
\255\255\255\255\035\001\014\001\015\001\016\001\017\001\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\001\001\255\255\003\001\255\255\005\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\014\001\015\001\016\001\
\017\001\001\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\255\255\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\001\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\001\001\255\255\003\001\
\255\255\005\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\014\001\015\001\255\255\255\255\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\255\255\
\255\255\255\255\255\255\255\255\001\001\255\255\003\001\255\255\
\005\001\255\255\255\255\255\255\028\001\029\001\030\001\031\001\
\032\001\014\001\015\001\035\001\255\255\255\255\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\001\001\255\255\
\003\001\255\255\005\001\255\255\255\255\255\255\001\001\255\255\
\003\001\255\255\005\001\255\255\255\255\255\255\255\255\255\255\
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
                   ( {vars=[]; units=[]; funcs=[]; equas=[];} )
# 405 "parser.ml"
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
# 418 "parser.ml"
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
# 429 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 86 "parser.mly"
               ( (_1, NOUNIT, _2) )
# 437 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
           ( Int   )
# 443 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
           ( Float )
# 449 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
           ( Char  )
# 455 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
           ( String)
# 461 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
           ( Bool  )
# 467 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
           ( Void  )
# 473 "parser.ml"
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
# 488 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'opt_formals) in
    Obj.repr(
# 152 "parser.mly"
                             ( _2 )
# 495 "parser.ml"
               : 'formals_block))
; (fun __caml_parser_env ->
    Obj.repr(
# 155 "parser.mly"
                 ([])
# 501 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 159 "parser.mly"
                  (List.rev _1 )
# 508 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 164 "parser.mly"
           ( [(_1, _2)] )
# 516 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 166 "parser.mly"
                               ( (_3, _4) :: _1 )
# 525 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 170 "parser.mly"
                           ( Block(List.rev _2) )
# 532 "parser.ml"
               : 'stmt_block))
; (fun __caml_parser_env ->
    Obj.repr(
# 174 "parser.mly"
   ( [] )
# 538 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 175 "parser.mly"
                    ( _2 :: _1 )
# 546 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
             (Expr _1)
# 553 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 179 "parser.mly"
                                    (Return _2)
# 560 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 180 "parser.mly"
                                           ( If(_3, _5, Block([])) )
# 568 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 181 "parser.mly"
                                             ( If(_3, _5, _7)        )
# 577 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 183 "parser.mly"
                                             ( For(_3, _5, _7, _9)   )
# 587 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 186 "parser.mly"
                  ( Noexpr )
# 593 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "parser.mly"
                   ( _1 )
# 600 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 190 "parser.mly"
                        ( IntLit(_1) )
# 607 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 191 "parser.mly"
                           ( FloatLit(_1) )
# 614 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 192 "parser.mly"
                         ( CharLit(_1) )
# 621 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 193 "parser.mly"
                          ( StringLit(_1) )
# 628 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 194 "parser.mly"
                         ( BoolLit(_1) )
# 635 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 195 "parser.mly"
                    ( Id(_1) )
# 642 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 196 "parser.mly"
                          ( Binop(_1, Add, _3) )
# 650 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 197 "parser.mly"
                          ( Binop(_1, Sub, _3) )
# 658 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 198 "parser.mly"
                          ( Binop(_1, Mult, _3) )
# 666 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 199 "parser.mly"
                           ( Binop(_1, Div, _3) )
# 674 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 200 "parser.mly"
                         ( Binop(_1, Pow, _3) )
# 682 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 201 "parser.mly"
                         ( Binop(_1, Equal, _3) )
# 690 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 202 "parser.mly"
                         ( Binop(_1, Neq, _3) )
# 698 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 203 "parser.mly"
                         ( Binop(_1, Less, _3) )
# 706 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 204 "parser.mly"
                         ( Binop(_1, Leq, _3) )
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 205 "parser.mly"
                         ( Binop(_1, Greater, _3) )
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 206 "parser.mly"
                         ( Binop(_1, Geq, _3) )
# 730 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 207 "parser.mly"
                         ( Binop(_1, And, _3) )
# 738 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 208 "parser.mly"
                         ( Binop(_1, Or, _3) )
# 746 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 209 "parser.mly"
                               ( Unop(Neg, _2) )
# 753 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 210 "parser.mly"
                      ( Unop(Not, _2) )
# 760 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 211 "parser.mly"
                       ( Assign(_1, _3) )
# 768 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 212 "parser.mly"
                             ( _2 )
# 775 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 214 "parser.mly"
                                       (FunctionCall(_1, _3))
# 783 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 281 "parser.mly"
                  ( [] )
# 789 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 282 "parser.mly"
               ( List.rev _1 )
# 796 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 285 "parser.mly"
                            ( [_1] )
# 803 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 286 "parser.mly"
                         ( _3 :: _1 )
# 811 "parser.ml"
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
