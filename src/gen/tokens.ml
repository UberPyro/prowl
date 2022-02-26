type token = 
  | FN | VAL | LET | IN | REC
  | OPEN | INCL | SPEC | INST | IMP
  | PUB | OPAQ | TYPE
  | NEW | DATA
  | MOD | SIG | BEGIN | END
  | TO | WITH | PAT | DOT

  | PLUS | MINUS | TIMES | DIV | ASSIGN

  | CMP | EQ | NEQ
  
  | AND | OR
  
  | CONS | SNOC | DOLLAR | AT | PERCENT | CARET
  | FOLDL | FOLDR | QMARK | EMARK | PIPE | AMPERSAND

  | LANGLE | RANGLE
  | LPAREN | RPAREN
  | LBRACKET | RBRACKET
  | LBRACE | RBRACE | PBRACE

  | COMMA | COLON | ARROW | BACKARROW

  | UNIT | NOP | QNOP | BLANK| EOF

  | ID of string | CAP_ID of string
  | MOD_ID of string | PMOD_ID of string | MACRO_ID of string
  | VARIANT of string | PVARIANT of string | STRING of string
  | METATYPE of string | LABEL of string

  | INTEGER of int
  | FLOAT of float
  | CHAR of char
