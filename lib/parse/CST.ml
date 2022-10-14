(* Generated by ocaml-tree-sitter. *)
(*
   prowl grammar

   entrypoint: source_file
*)

open! Sexplib.Conv
open Tree_sitter_run

type op3 = Token.t (* pattern [+\-][~!@#$%^&*\-=+\.?:<>|/\\]* *)
[@@deriving sexp_of]

type string_content1 = Token.t (* pattern "[^\\\\\"]+" *)
[@@deriving sexp_of]

type l = Token.t (* pattern \(|beg *)
[@@deriving sexp_of]

type op0 = Token.t (* pattern \|[~!@#$%^&*\-=+\.?:<>|/\\]* *)
[@@deriving sexp_of]

type id = Token.t (* pattern "[a-z](-?[A-Za-z0-9_'])*" *)
[@@deriving sexp_of]

type escape3 = Token.t (* pattern \\x[0-9A-Fa-f][0-9A-Fa-f] *)
[@@deriving sexp_of]

type character_content1 = Token.t (* pattern "[^\\\\']" *)
[@@deriving sexp_of]

type op1 = Token.t (* pattern [$=&][~!@#$%^&*\-=+\.?:<>|/\\]* *)
[@@deriving sexp_of]

type r = Token.t (* pattern \)|end *)
[@@deriving sexp_of]

type uop = Token.t (* pattern [~!?][~!@#$%^&*\-=+\.?:<>|/\\]* *)
[@@deriving sexp_of]

type escape4 = Token.t (* pattern \\o[0-3][0-7][0-7] *)
[@@deriving sexp_of]

type int_ = Token.t (* pattern 0|[1-9][0-9]* *)
[@@deriving sexp_of]

type op4 = Token.t (* pattern [*\/%][~!@#$%^&*\-=+\.?:<>|/\\]* *)
[@@deriving sexp_of]

type op2 = Token.t (* pattern [@:][~!@#$%^&*\-=+\.?:<>|/\\]* *)
[@@deriving sexp_of]

type escape1 = Token.t (* pattern "\\\\[\\\\\"'ntbr ]" *)
[@@deriving sexp_of]

type comment_contents =
  Token.t (* pattern ([^*\/]|\*[^\/]|\/[^*]|[ \t\n])*[^*\/] *)
[@@deriving sexp_of]

type escape2 = Token.t (* pattern \\[0-9][0-9][0-9] *)
[@@deriving sexp_of]

type op5 = Token.t (* pattern [\.\^#][~!@#$%^&*\-=+\.?:<>|/\\]* *)
[@@deriving sexp_of]

type comment = (
    Token.t (* "/*" *)
  * [ `Comm_content of comment_contents (*tok*) | `Comm of comment ]
  * Token.t (* "*/" *)
)
[@@deriving sexp_of]

type escape_sequence = [
    `Esc1 of escape1 (*tok*)
  | `Esc2 of escape2 (*tok*)
  | `Esc3 of escape3 (*tok*)
  | `Esc4 of escape4 (*tok*)
]
[@@deriving sexp_of]

type bop = [
    `Op0 of op0 (*tok*)
  | `Op1 of op1 (*tok*)
  | `Op2 of op2 (*tok*)
  | `Op3 of op3 (*tok*)
  | `Op4 of op4 (*tok*)
  | `Op5 of op5 (*tok*)
]
[@@deriving sexp_of]

type string_content = [
    `SPACE of Token.t (* " " *)
  | `LF of Token.t (* "\n" *)
  | `HT of Token.t (* "\t" *)
  | `Str_content1 of string_content1 (*tok*)
  | `Esc_seq of escape_sequence
]
[@@deriving sexp_of]

type character_content = [
    `Char_content1 of character_content1 (*tok*)
  | `Esc_seq of escape_sequence
]
[@@deriving sexp_of]

type expr = [
    `Rep1_word of word list (* one or more *)
  | `Expr_op5_expr of (expr * op5 (*tok*) * expr)
  | `Expr_uop of (expr * uop (*tok*))
  | `Expr_op4_expr of (expr * op4 (*tok*) * expr)
  | `Expr_op3_expr of (expr * op3 (*tok*) * expr)
  | `Expr_op2_expr of (expr * op2 (*tok*) * expr)
  | `Expr_op1_expr of (expr * op1 (*tok*) * expr)
  | `Let_opt_rec_rep1_id_EQ_expr_in_expr of (
        Token.t (* "let" *)
      * Token.t (* "rec" *) option
      * id (*tok*) list (* one or more *)
      * Token.t (* "=" *)
      * expr
      * Token.t (* "in" *)
      * expr
    )
  | `As_rep1_id_DASHGT_expr of (
        Token.t (* "as" *)
      * id (*tok*) list (* one or more *)
      * Token.t (* "->" *)
      * expr
    )
  | `Expr_op0_expr of (expr * op0 (*tok*) * expr)
]

and word = [
    `Int of int_ (*tok*)
  | `Char of (
        Token.t (* "'" *)
      * character_content option
      * Token.t (* "'" *)
    )
  | `LBRACK_expr_RBRACK of (Token.t (* "[" *) * expr * Token.t (* "]" *))
  | `L_expr_r of (l (*tok*) * expr * r (*tok*))
  | `LCURL_opt_COMMA_expr_rep_COMMA_expr_opt_COMMA_RCURL of (
        Token.t (* "{" *)
      * Token.t (* "," *) option
      * expr
      * (Token.t (* "," *) * expr) list (* zero or more *)
      * Token.t (* "," *) option
      * Token.t (* "}" *)
    )
  | `LBRACK_RBRACK of (Token.t (* "[" *) * Token.t (* "]" *))
  | `L_r of (l (*tok*) * r (*tok*))
  | `LCURL_RCURL of (Token.t (* "{" *) * Token.t (* "}" *))
  | `Str of (
        Token.t (* "\"" *)
      * string_content list (* zero or more *)
      * Token.t (* "\"" *)
    )
  | `Id of id (*tok*)
  | `L_bop_expr_r of (l (*tok*) * bop * expr * r (*tok*))
  | `L_expr_bop_r of (l (*tok*) * expr * bop * r (*tok*))
  | `L_bop_r of (l (*tok*) * bop * r (*tok*))
  | `L_uop_r of (l (*tok*) * uop (*tok*) * r (*tok*))
  | `LBRACK_bop_expr_RBRACK of (
        Token.t (* "[" *) * bop * expr * Token.t (* "]" *)
    )
  | `LBRACK_expr_bop_RBRACK of (
        Token.t (* "[" *) * expr * bop * Token.t (* "]" *)
    )
  | `LBRACK_bop_RBRACK of (Token.t (* "[" *) * bop * Token.t (* "]" *))
  | `LBRACK_uop_RBRACK of (
        Token.t (* "[" *) * uop (*tok*) * Token.t (* "]" *)
    )
]
[@@deriving sexp_of]

type source_file = expr option
[@@deriving sexp_of]

type string_ (* inlined *) = (
    Token.t (* "\"" *)
  * string_content list (* zero or more *)
  * Token.t (* "\"" *)
)
[@@deriving sexp_of]

type char (* inlined *) = (
    Token.t (* "'" *)
  * character_content option
  * Token.t (* "'" *)
)
[@@deriving sexp_of]

let dump_tree root =
  sexp_of_source_file root
  |> Print_sexp.to_stdout
