{
  open! Batteries
  open Lexing

  open Parse
}

let eol = ['\r' '\n'] | '\r' '\n'
let whitespace = ' '+

let digit = ['0'-'9']
let sig_digits = ['1'-'9'] digit*

let id_char = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']
let id_tail = ('-'? id_char)*
let id = ['a'-'z'] id_tail
let cap_id = ['A'-'Z'] id_tail

let integer = '0' | sig_digits
let string_body = ([^ '"'] | "\\\"")*

rule token = parse
  | (id as s) whitespace "=" {ASSIGN s}

  | "/*"        {comment 0 lexbuf}
  | eof         {EOF}
  | eol         {new_line lexbuf; token lexbuf}
  | whitespace  {token lexbuf}
  | '\t'        {token lexbuf}

  | "let"       {LET}
  | "in"        {IN}
  | "ex"        {EX}
  | "."         {DOT}

  | "|"         {PIPE}
  | "^?"        {QUANT_MARK}
  | "^*"        {QUANT_STAR}
  | "^+"        {QUANT_PLUS}
  | "~"         {TILDE
  }
  | "pick"      {PICK}
  | "ponder"    {PONDER}
  | "FORK"      {FORK}
  | "par"       {PAR}
  | "end"       {END}

  | "||"        {PICK_OP}
  | "++"        {PONDER_OP}
  | "&&"        {FORK_OP}
  | "**"        {PAR_OP}

  | "+"         {PLUS}
  | "-"         {MINUS}
  | "*"         {STAR}
  | "?"         {QMARK}

  | "("         {LPAREN}
  | ")"         {RPAREN}
  | "["         {LBRACK}
  | "]"         {RBRACK}
  | "{"         {LBRACE}
  | "}"         {RBRACE}

  | ","         {COMMA}
  | ";"         {SEMICOLON}
  | "--"        {BAR}

  | (integer as i) {INT (int_of_string i)}
  | '"' (string_body as s) '"' {STR s}

  | id     as s {ID s}
  | cap_id as s {UVAR s}

and comment level = parse
  | "*/" {if level = 0 then token lexbuf
          else comment (level-1) lexbuf}
  | "/*" {comment (level+1) lexbuf}
  | _    {comment level lexbuf}
