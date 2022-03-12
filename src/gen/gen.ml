open Batteries

let ast ch = 
  let lexbuf = Lexing.from_channel ch in
  try Parse.program Lex.token lexbuf with
  | _ ->
    let p = lexbuf.lex_curr_p in
    Printf.sprintf
      "Invalid Token at [%d,%d]"
      p.pos_lnum p.pos_cnum
    |> failwith

let string_of_token : Parse.token -> string = function
  | WITH -> "WITH"
  | VAL -> "VAL"
  | USE -> "USE"
  | USCORE -> "USCORE"
  | UPDATE -> "UPDATE"
  | TYPE -> "TYPE"
  | TO -> "TO"
  | TIMES -> "TIMES"
  | SYMBOL s -> Printf.sprintf "SYMBOL <%s>" s
  | SUM s -> Printf.sprintf "SUM <%s>" s
  | STR s -> Printf.sprintf "STR <%s>" s
  | SPEC -> "SPEC"
  | SNOC -> "SNOC"
  | SIG -> "SIG"
  | RPAREN -> ")"
  | RBRACKET -> "]"
  | RBRACE -> "}"
  | RANGLE -> ">"
  | RANGE -> ".."
  | PUB -> "PUB"
  | PLUS -> "+"
  | PBRACE -> "'{"
  | OPEN -> "OPEN"
  | OPAQ -> "OPAQ"
  | NEW -> "NEW"
  | NEQ -> "/="
  | MIX -> "MIX"
  | MINUS -> "-"
  | METATYPE s -> Printf.sprintf "METATYPE <%s>" s
  | LPAREN -> "("
  | LET -> "LET"
  | LBRACKET -> "["
  | LBRACE -> "{"
  | LANGLE -> "<"
  | LABEL s -> Printf.sprintf "LABEL <%s>" s
  | INT i -> Printf.sprintf "INT <%d>" i
  | INST -> "INST"
  | IN -> "IN"
  | ID s -> Printf.sprintf "ID <%s>" s
  | FN -> "FN"
  | FLOAT f -> Printf.sprintf "FLOAT <%f>" f
  | EXP -> "EXP"
  | EQ -> "EQ"
  | EOF -> "EOF"
  | END -> "END"
  | DIV -> "DIV"
  | DCOLON -> "::"
  | DATA -> "DATA"
  | CONS -> "-<"
  | COMMA -> ","
  | COLON -> ":"
  | CMP -> "?="
  | CHAR c -> Printf.sprintf "CHAR <%c>" c
  | CAP_ID s -> Printf.sprintf "CID <%s>" s
  | BLANK -> "BLANK"
  | BEGIN -> "BEGIN"
  | ATOM s -> Printf.sprintf "ATOM <%s>" s
  | ASSIGN -> "="
  | AS -> "as"
  | APPEND -> "APPEND"
  | MOD -> "MOD"

let lex ch = 
  let lexbuf = Lexing.from_channel ch in
  while 
    let l = Lex.token lexbuf |> string_of_token in 
    print_endline l;
    l <> "EOF" do
    ()
  done
