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
  | TRUE
  | FALSE
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
  | TYPEOF
  | PRINT
  | INT2FLOAT
  | FLOAT2INT
  | CEIL
  | FLOOR
  | METER
  | SEC
  | KGRAM
  | AMP
  | CMETER
  | HERTZ
  | GRAM
  | NEWTON
  | NOUNIT
  | SIZEOF
  | ID of (string)
  | UID of (string)
  | UONE of (string)
  | INT_LITERAL of (int)
  | FLOAT_LITERAL of (float)
  | CHAR_LITERAL of (char)
  | STRING_LITERAL of (string)
  | BOOL_LITERAL of (bool)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
