type token = 
| ZAP
| WITHIN
| VAR of (
# 31 "lib/syntax/parse.mly"
  (string)
# 17 "lib/syntax/parse.ml"
)
| UNIT
| UNION
| TENSOR
| SWAP
| SUB
| STRING of (
# 31 "lib/syntax/parse.mly"
  (string)
# 27 "lib/syntax/parse.ml"
)
| STAR
| STACK_VAR of (
# 31 "lib/syntax/parse.mly"
  (string)
# 33 "lib/syntax/parse.ml"
)
| SPECIFY
| SHOW
| RPAREN
| RL
| RBRACK
| RBRACE
| PT
| PONDER
| PLUS
| PIPE
| PICK
| PARSE
| NOP
| NEQ
| MUL
| MT
| MARK
| LT
| LPAREN
| LIN
| LET
| LE
| LBRACK
| LBRACE
| INT of (
# 32 "lib/syntax/parse.mly"
  (int)
# 62 "lib/syntax/parse.ml"
)
| INDUCE
| IN
| ID
| GUESS
| GT
| GEN
| GE
| FORK
| FN
| FAB
| EXISTS
| EXCH
| EQ
| EOF
| ELIM
| EACH
| DUP
| DOT
| DOLLAR
| DIVMOD
| DIP
| DAG
| CROSS
| COSTACK_VAR of (
# 31 "lib/syntax/parse.mly"
  (string)
# 90 "lib/syntax/parse.ml"
)
| CONS
| COMMA
| CMP
| CAT
| CAP of (
# 31 "lib/syntax/parse.mly"
  (string)
# 99 "lib/syntax/parse.ml"
)
| BAR
| ASSIGN
| ARROW
| APPLY
| AP
| ADD
| AB
[@@deriving show]
