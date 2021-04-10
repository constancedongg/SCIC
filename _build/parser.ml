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
  | UNIT of (string)
  | INT_LITERAL of (int)
  | FLOAT_LITERAL of (string)
  | STRING_LITERAL of (string)
  | BOOL_LITERAL of (bool)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 65 "parser.ml"
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
  308 (* UNIT *);
  309 (* INT_LITERAL *);
  310 (* FLOAT_LITERAL *);
  311 (* STRING_LITERAL *);
  312 (* BOOL_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\005\000\005\000\005\000\
\005\000\005\000\005\000\004\000\004\000\006\000\006\000\008\000\
\008\000\008\000\007\000\007\000\009\000\009\000\009\000\009\000\
\009\000\009\000\011\000\011\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\012\000\012\000\013\000\013\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\004\000\001\000\001\000\001\000\
\001\000\001\000\001\000\010\000\009\000\000\000\001\000\002\000\
\003\000\005\000\000\000\002\000\002\000\003\000\003\000\005\000\
\007\000\009\000\000\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\003\000\003\000\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\056\000\000\000\010\000\006\000\007\000\008\000\
\009\000\011\000\001\000\003\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\000\000\000\000\
\000\000\000\000\016\000\000\000\000\000\000\000\000\000\017\000\
\019\000\000\000\000\000\000\000\000\000\019\000\000\000\019\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\030\000\031\000\032\000\020\000\000\000\018\000\000\000\000\000\
\000\000\048\000\047\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\050\000\023\000\000\000\000\000\022\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\038\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\051\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\
\000\000\000\000\026\000"

let yydgoto = "\002\000\
\003\000\004\000\012\000\013\000\023\000\024\000\036\000\025\000\
\052\000\053\000\063\000\087\000\088\000"

let yysindex = "\015\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\222\254\249\254\225\254\
\062\255\019\255\078\255\112\255\067\255\000\000\228\254\083\255\
\093\255\112\255\000\000\046\255\092\255\112\255\100\255\000\000\
\000\000\057\255\122\255\004\255\069\255\000\000\070\255\000\000\
\000\000\070\255\070\255\127\255\128\255\070\255\023\255\000\000\
\000\000\000\000\000\000\000\000\226\255\000\000\039\255\141\000\
\060\255\000\000\000\000\070\255\070\255\176\000\133\255\070\255\
\070\255\000\000\070\255\070\255\070\255\070\255\070\255\070\255\
\070\255\070\255\070\255\070\255\070\255\070\255\070\255\000\000\
\000\000\000\000\163\000\137\255\000\000\176\000\140\255\150\255\
\176\000\045\255\045\255\136\255\136\255\000\000\200\000\200\000\
\175\255\175\255\175\255\175\255\189\000\240\255\114\255\070\255\
\000\000\070\255\118\255\252\255\176\000\114\255\070\255\000\000\
\154\255\114\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\156\255\000\000\000\000\000\000\000\000\
\158\255\156\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\163\255\157\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\163\255\009\255\000\000\161\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\255\000\000\180\255\
\048\255\023\000\049\000\183\255\209\255\000\000\125\000\128\000\
\057\000\083\000\091\000\117\000\081\255\086\255\000\000\000\000\
\000\000\000\000\095\255\000\000\073\255\000\000\182\255\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\011\000\168\000\079\000\000\000\
\025\000\217\255\201\255\000\000\000\000"

let yytablesize = 480
let yytable = "\056\000\
\011\000\015\000\058\000\059\000\018\000\084\000\062\000\039\000\
\054\000\028\000\054\000\040\000\041\000\028\000\014\000\001\000\
\042\000\016\000\043\000\019\000\083\000\062\000\027\000\028\000\
\086\000\089\000\064\000\090\000\091\000\092\000\093\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\034\000\044\000\039\000\017\000\045\000\046\000\040\000\080\000\
\049\000\065\000\049\000\042\000\049\000\043\000\047\000\113\000\
\048\000\049\000\050\000\051\000\069\000\070\000\071\000\039\000\
\108\000\020\000\109\000\040\000\082\000\021\000\026\000\062\000\
\042\000\039\000\043\000\055\000\044\000\055\000\022\000\045\000\
\046\000\045\000\042\000\045\000\043\000\045\000\046\000\029\000\
\046\000\047\000\046\000\048\000\049\000\050\000\051\000\030\000\
\032\000\044\000\024\000\033\000\045\000\046\000\024\000\024\000\
\035\000\045\000\045\000\024\000\037\000\024\000\047\000\046\000\
\048\000\049\000\050\000\051\000\055\000\039\000\057\000\054\000\
\047\000\040\000\048\000\049\000\050\000\051\000\042\000\107\000\
\043\000\038\000\060\000\061\000\024\000\085\000\112\000\024\000\
\024\000\104\000\115\000\005\000\006\000\007\000\008\000\009\000\
\105\000\024\000\010\000\024\000\024\000\024\000\024\000\044\000\
\106\000\071\000\045\000\046\000\110\000\033\000\114\000\033\000\
\014\000\033\000\015\000\027\000\047\000\052\000\048\000\049\000\
\050\000\051\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\036\000\
\053\000\036\000\027\000\036\000\067\000\068\000\069\000\070\000\
\071\000\031\000\000\000\000\000\036\000\036\000\036\000\036\000\
\000\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\037\000\000\000\037\000\000\000\037\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\037\000\037\000\
\037\000\037\000\066\000\037\000\037\000\037\000\037\000\037\000\
\037\000\037\000\037\000\000\000\000\000\000\000\000\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000\111\000\067\000\068\000\069\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\067\000\068\000\069\000\070\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\079\000\000\000\034\000\
\000\000\034\000\000\000\034\000\005\000\006\000\007\000\008\000\
\009\000\000\000\000\000\010\000\034\000\034\000\000\000\000\000\
\000\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\035\000\000\000\035\000\000\000\035\000\000\000\000\000\
\000\000\041\000\000\000\041\000\000\000\041\000\035\000\035\000\
\000\000\000\000\000\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\035\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\043\000\000\000\043\000\000\000\043\000\
\000\000\000\000\000\000\042\000\000\000\042\000\000\000\042\000\
\000\000\000\000\000\000\000\000\000\000\043\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\044\000\000\000\044\000\
\000\000\044\000\000\000\000\000\000\000\039\000\000\000\039\000\
\040\000\039\000\040\000\000\000\040\000\000\000\000\000\044\000\
\044\000\044\000\044\000\044\000\044\000\044\000\044\000\039\000\
\039\000\081\000\040\000\040\000\000\000\039\000\039\000\000\000\
\040\000\040\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000\078\000\079\000\103\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\067\000\068\000\069\000\070\000\071\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\079\000\067\000\068\000\069\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\079\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000\067\000\068\000\069\000\
\070\000\071\000\000\000\000\000\074\000\075\000\076\000\077\000"

let yycheck = "\039\000\
\000\000\036\001\042\000\043\000\036\001\061\000\046\000\004\001\
\003\001\001\001\005\001\008\001\009\001\005\001\004\000\001\000\
\013\001\052\001\015\001\051\001\060\000\061\000\051\001\052\001\
\064\000\065\000\004\001\067\000\068\000\069\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\030\000\038\001\004\001\051\001\041\001\042\001\008\001\009\001\
\001\001\027\001\003\001\013\001\005\001\015\001\051\001\111\000\
\053\001\054\001\055\001\056\001\016\001\017\001\018\001\004\001\
\104\000\004\001\106\000\008\001\009\001\051\001\004\001\111\000\
\013\001\004\001\015\001\003\001\038\001\005\001\001\001\041\001\
\042\001\001\001\013\001\003\001\015\001\005\001\001\001\005\001\
\003\001\051\001\005\001\053\001\054\001\055\001\056\001\003\001\
\051\001\038\001\004\001\008\001\041\001\042\001\008\001\009\001\
\005\001\025\001\026\001\013\001\052\001\015\001\051\001\026\001\
\053\001\054\001\055\001\056\001\038\000\004\001\040\000\051\001\
\051\001\008\001\053\001\054\001\055\001\056\001\013\001\103\000\
\015\001\008\001\004\001\004\001\038\001\001\001\110\000\041\001\
\042\001\001\001\114\000\028\001\029\001\030\001\031\001\032\001\
\005\001\051\001\035\001\053\001\054\001\055\001\056\001\038\001\
\003\001\018\001\041\001\042\001\039\001\001\001\005\001\003\001\
\005\001\005\001\005\001\001\001\051\001\005\001\053\001\054\001\
\055\001\056\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\001\001\
\005\001\003\001\005\001\005\001\014\001\015\001\016\001\017\001\
\018\001\026\000\255\255\255\255\014\001\015\001\016\001\017\001\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\001\001\255\255\003\001\255\255\005\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\001\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\255\255\255\255\255\255\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\001\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\255\255\001\001\
\255\255\003\001\255\255\005\001\028\001\029\001\030\001\031\001\
\032\001\255\255\255\255\035\001\014\001\015\001\255\255\255\255\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\001\001\255\255\003\001\255\255\005\001\255\255\255\255\
\255\255\001\001\255\255\003\001\255\255\005\001\014\001\015\001\
\255\255\255\255\255\255\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\001\001\255\255\003\001\255\255\005\001\
\255\255\255\255\255\255\001\001\255\255\003\001\255\255\005\001\
\255\255\255\255\255\255\255\255\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\001\001\255\255\003\001\
\255\255\005\001\255\255\255\255\255\255\001\001\255\255\003\001\
\001\001\005\001\003\001\255\255\005\001\255\255\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\019\001\
\020\001\005\001\019\001\020\001\255\255\025\001\026\001\255\255\
\025\001\026\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\005\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\014\001\015\001\016\001\
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
  UNIT\000\
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
# 51 "parser.mly"
             ( _1 )
# 401 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                   ( ([], [])  )
# 407 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 55 "parser.mly"
                    (((_2 :: fst _1), snd _1))
# 415 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 56 "parser.mly"
                     ((fst _1, (_2 :: snd _1)))
# 423 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 84 "parser.mly"
                    ( (_1, _2, _3) )
# 432 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
           ( Int   )
# 438 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
           ( Float )
# 444 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
           ( Char  )
# 450 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
           ( String)
# 456 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
           ( Bool  )
# 462 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
           ( Void  )
# 468 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'opt_formals) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 153 "parser.mly"
                                                                     ({
      return_type       = _1;
		func_identifier   = _4;
		func_formals      = List.rev _6;
		func_stmts        = List.rev _9;
   })
# 484 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'opt_formals) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 159 "parser.mly"
                                                                ({
      return_type       = _1;
		func_identifier   = _3;
		func_formals      = List.rev _5;
		func_stmts        = List.rev _8;
   })
# 499 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 172 "parser.mly"
                 ([])
# 505 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 176 "parser.mly"
                  (List.rev _1 )
# 512 "parser.ml"
               : 'opt_formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 180 "parser.mly"
           ( [(_1, "", _2)] )
# 520 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 181 "parser.mly"
                 ( [(_1, _2, _3)] )
# 529 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 183 "parser.mly"
                                    ( (_3, _4, _5) :: _1 )
# 539 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 189 "parser.mly"
   ( [] )
# 545 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 190 "parser.mly"
                    ( _2 :: _1 )
# 553 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 193 "parser.mly"
             (Expr _1)
# 560 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 194 "parser.mly"
                                    ( Return _2 )
# 567 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 195 "parser.mly"
                                          ( Block(List.rev _2) )
# 574 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 196 "parser.mly"
                                           ( If(_3, _5, Block([])) )
# 582 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 197 "parser.mly"
                                             ( If(_3, _5, _7)        )
# 591 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 199 "parser.mly"
                                             ( For(_3, _5, _7, _9)   )
# 601 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 202 "parser.mly"
                  ( Noexpr )
# 607 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 203 "parser.mly"
                   ( _1 )
# 614 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 206 "parser.mly"
                        ( IntLit(_1) )
# 621 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 207 "parser.mly"
                           ( FloatLit(_1) )
# 628 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 208 "parser.mly"
                                 ( StringLit(_1) )
# 635 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 209 "parser.mly"
                         ( BoolLit(_1) )
# 642 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 210 "parser.mly"
                    ( Id(_1) )
# 649 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 211 "parser.mly"
                          ( Binop(_1, Add, _3) )
# 657 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 212 "parser.mly"
                          ( Binop(_1, Sub, _3) )
# 665 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 213 "parser.mly"
                          ( Binop(_1, Mult, _3) )
# 673 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 214 "parser.mly"
                           ( Binop(_1, Div, _3) )
# 681 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 215 "parser.mly"
                         ( Binop(_1, Pow, _3) )
# 689 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 216 "parser.mly"
                         ( Binop(_1, Equal, _3) )
# 697 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 217 "parser.mly"
                         ( Binop(_1, Neq, _3) )
# 705 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 218 "parser.mly"
                         ( Binop(_1, Less, _3) )
# 713 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 219 "parser.mly"
                         ( Binop(_1, Leq, _3) )
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 220 "parser.mly"
                         ( Binop(_1, Greater, _3) )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 221 "parser.mly"
                         ( Binop(_1, Geq, _3) )
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 222 "parser.mly"
                         ( Binop(_1, And, _3) )
# 745 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 223 "parser.mly"
                         ( Binop(_1, Or, _3) )
# 753 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 224 "parser.mly"
                               ( Unop(Neg, _2) )
# 760 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 225 "parser.mly"
                      ( Unop(Not, _2) )
# 767 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 226 "parser.mly"
                       ( Assign(_1, _3) )
# 775 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 227 "parser.mly"
                             ( _2 )
# 782 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 229 "parser.mly"
                                       (FunctionCall(_1, _3))
# 790 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 296 "parser.mly"
                  ( [] )
# 796 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 297 "parser.mly"
               ( List.rev _1 )
# 803 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 300 "parser.mly"
                            ( [_1] )
# 810 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 301 "parser.mly"
                         ( _3 :: _1 )
# 818 "parser.ml"
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
