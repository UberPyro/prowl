
(* The type of tokens. *)

type token = 
  | VARIANT of (string)
  | VAL
  | UNIT
  | TYPE
  | TO
  | TIMES
  | STRING of (string)
  | SPEC
  | SNOC
  | SIG
  | RPAREN
  | REC
  | RBRACKET
  | RBRACE
  | RANGLE
  | QNOP
  | QMARK
  | PVARIANT of (string)
  | PUB
  | PMOD_ID of (string)
  | PLUS
  | PIPE
  | PERCENT
  | PBRACE
  | OR
  | OPEN
  | OPAQ
  | NOP
  | NEW
  | NEQ
  | MOD_ID of (string)
  | MOD
  | MINUS
  | METATYPE of (string)
  | MACRO_ID of (string)
  | LPAREN
  | LET
  | LBRACKET
  | LBRACE
  | LANGLE
  | LABEL of (string)
  | INTEGER of (int)
  | INST
  | INCL
  | IN
  | IMP
  | ID of (string)
  | FOLDR
  | FOLDL
  | FN
  | FLOAT of (float)
  | EQ
  | EOF
  | END
  | EMARK
  | DOLLAR
  | DIV
  | DATA
  | CONS
  | COMMA
  | COLON
  | CMP
  | CHAR of (char)
  | CARET
  | CAP_ID of (string)
  | BEGIN
  | BACKARROW
  | AT
  | ASSIGN
  | AS
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.stmt)
