let eol = ['\r' '\n'] | '\r' '\n'
let whitespace = ' '+

let digit = ['0'-'9']
let sig_digits = ['1'-'9'] digit*

let id_tail = ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let id = ['a'-'z'] id_tail

let op_char = [
  '~' '!' '@' '#' '$' '%' '^' '&' '*' '-' '+' '='
  ';' ':' ',' '.' '<' '>' '/' '?' '|' '\\'
]

let symbol = op_char+

let integer = '0' | sig_digits
let float = sig_digits '.' digit* | '.' digit+

let char_body = [^ '\'']
let string_body = ([^ '"'] | "\\\"")*

rule token = parse
  | "/*" {comment 0 lexbuf}
  | eof {EOF}
  | eol {new_line lexbuf; token lexbuf}
  | whitespace {token lexbuf}
  | '\t' {token lexbuf}

  | "fn" {FN}
  | '=' {ASSIGN}

  | (integer as i) {INT (int_of_string i)}
  | (float as f) {FLOAT (float_of_string f)}
  | '"' (string_body as s) '"' {STR (decode s)}
