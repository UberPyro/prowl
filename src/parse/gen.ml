open Batteries
open Printf

open Ast
open Parse

let parse ch = 
  let lexbuf = Lexing.from_channel ch in
  try Parse.program Lex.token lexbuf with
  | _ ->
    let p = lexbuf.lex_curr_p in
    Printf.sprintf
      "Unexpected Token at [%d,%d]"
      p.pos_lnum (p.pos_cnum - p.pos_bol)
    |> failwith

let string_of_quant = function
  | Opt -> "?"
  | Plus -> "+"
  | Star -> "*"
  | _ -> "not implemented"

let string_of_greed = function
  | Gre -> ""
  | Rel -> "?"
  | Cut -> "+"

let c_str s = function
  | 0 -> s
  | n -> sprintf "%s %d" s n

let string_of_comb = function
  | StackComb lst -> 
    List.map fst lst
    |> List.map begin function
      | Dup n -> c_str "DUP" n
      | Zap n -> c_str "ZAP" n
      | Rot n -> c_str "ROT" n
      | Run n -> c_str "RUN" n
    end |> String.concat "; "
  | _ -> failwith "Not a stack combinator"

let string_of_bindop op = function
  | "" -> op
  | s -> sprintf "%sOP %s" op s

let string_of_token = function
  | WIDE_ARROW -> "=>"
  | VOID -> "<;>"
  | UNIT -> "<>"
  | TYPE -> "TYPE"
  | TIMES_BRACK -> "{ (regex)"
  | TIMES -> "*"
  | TILDE -> "~"
  | SYMBOL s -> sprintf "SYMBOL %s" s
  | STR s -> sprintf "STRING %s" s
  | SNOC -> ">-"
  | SIG -> "SIG"
  | SEMICOLON g -> sprintf ";%s" (string_of_greed g)
  | RPAREN -> ")"
  | RBRACK -> "]"
  | RBRACE g -> sprintf "}%s" (string_of_greed g)
  | RANGE -> ".."
  | QUANT (q, g) -> 
    sprintf "QUANT %s%s" (string_of_quant q) (string_of_greed g)
  | PUB -> "PUB"
  | PLUS -> "+"
  | OPEN -> "OPEN"
  | OPAQ -> "OPAQ"
  | NONCAP_BRACK -> "(?:"
  | NEQ -> "/="
  | MOD -> "MOD"
  | MIX -> "MIX"
  | MINUS -> "-"
  | LT -> "<"
  | LPAREN -> "("
  | LET s -> string_of_bindop "LET" s
  | LE -> "<="
  | LBRACK -> "["
  | LBRACE -> "{"
  | KLEISLI -> ">=>"
  | INV_BRACK -> "[^"
  | INTERSECT -> "&&"
  | INT i -> sprintf "INTEGER %d" i
  | INFIX s -> sprintf "INFIX %s" s
  | IMPL -> "IMPL"
  | ID s -> sprintf "ID %s" s
  | GT -> ">"
  | GE -> ">="
  | FLOAT f -> sprintf "FLOAT %f" f
  | EXP -> "**"
  | EQ -> "=="
  | EOF -> "EOF"
  | END -> "END"
  | EFFECT -> "--"
  | DOT -> "."
  | DO -> "DO"
  | DIVIDE -> "/"
  | DEF -> "DEF"
  | DATA -> "DATA"
  | CONS -> "-<"
  | COMMA -> ","
  | COMB e -> sprintf "COMB %s" (string_of_comb e)
  | COLON -> ":"
  | CHAR c -> sprintf "CHAR %c" c
  | CAT -> "&"
  | CAP s -> sprintf "CAP %s" s
  | BLANK -> "BLANK"
  | BIND -> ">>="
  | BACKARROW -> "<-"
  | ATOM_BRACK -> "(?>"
  | ASSIGN -> "="
  | AS s -> string_of_bindop "AS" s
  | ARROW -> "->"
  | APPEND -> "++"
  | AND s -> string_of_bindop "AND" s
  | ALT -> "|"
  | IMPL_LBRACK -> "[<"
  | IMPL_RBRACK -> ">]"

let lex ch = 
  let lexbuf = Lexing.from_channel ch in
  while 
    let l = string_of_token (Lex.token lexbuf) in
    print_endline l;
    l <> "EOF" do
    ()
  done
