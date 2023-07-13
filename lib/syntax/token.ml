type token = Parse.token = 
| ZAP
| WITHIN
| VAR of (
# 24 "lib/syntax/parse.mly"
  (string)
# 17 "lib/syntax/parse.ml"
)
| UNIT
| UNION
| TENSOR
| SWAP
| SUB
| STRING of (
# 24 "lib/syntax/parse.mly"
  (string)
# 27 "lib/syntax/parse.ml"
)
| STAR
| STACK_VAR of (
# 24 "lib/syntax/parse.mly"
  (string)
# 33 "lib/syntax/parse.ml"
)
| SPECIFY
| SHOW
| RPAREN
| RBRACK
| RBRACE
| PONDER
| PLUS
| PIPE
| PICK
| PARSE
| NOP
| NEQ
| MUL
| MARK
| LT
| LPAREN
| LIN
| LET
| LE
| LBRACK
| LBRACE
| INT of (
# 25 "lib/syntax/parse.mly"
  (int)
# 59 "lib/syntax/parse.ml"
)
| INDUCE
| IN
| ID
| GUESS
| GT
| GEN
| GE
| FORK
| FAB
| EXISTS
| EXCH
| EQ
| EOF
| ELIM
| DUP
| DOT
| DOLLAR
| DIVMOD
| DIP
| DAG
| CROSS
| COSTACK_VAR of (
# 24 "lib/syntax/parse.mly"
  (string)
# 85 "lib/syntax/parse.ml"
)
| CONS
| COMMA
| CMP
| CAT
| CAP of (
# 24 "lib/syntax/parse.mly"
  (string)
# 94 "lib/syntax/parse.ml"
)
| BAR
| ASSIGN
| ARROW
| APPLY
| AP
| ADD
| AB
    [@@deriving show]