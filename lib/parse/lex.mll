{ open Batteries
  open Lexing
  
  open Lex_proc
  open Parse     }

let eol = ['\r' '\n'] | '\r' '\n'
let whitespace = ' '+

let digit = ['0'-'9']
let sig_digits = ['1'-'9'] digit*

let id_tail = ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let id = ['a'-'z'] id_tail

let op_slow = ['=' '<' '<' '|' '&' '$']
let op_append = ['@' ':']
let op_plus = ['+' '-']
let op_times = ['*' '/' '%']
let op_exp = "**" | ['.' '#' '^']
let op_postfix = ['~' '!' '?']

let op_body = (
    op_slow | op_append | op_plus
  | op_times | op_exp | op_postfix
)*

let slow = op_slow op_body
let ap = op_append op_body
let plus = op_plus op_body
let times = op_times op_body
let exp = op_exp op_body
let post = op_postfix op_body

let integer = '0' | sig_digits
let float = sig_digits '.' digit* | '.' digit+

let char_body = [^ '\'']
let string_body = ([^ '"'] | "\\\"")*

rule token = parse
  | "/*" {comment 0 lexbuf}
  | eof {EOF}
  | eol {new_line lexbuf; token lexbuf}
  | whitespace {token lexbuf}
  | '\t' {advance lexbuf; token lexbuf}

  | "fn" {FN}
  | '=' {ASSIGN}
  | '|' {ALT}

  | "(" {LPAREN}
  | ")" {RPAREN}
  | "[" {LBRACK}
  | "]" {RBRACK}
  | "{" {LBRACE}
  | "}" {RBRACE}

  | (integer as i) {INT (int_of_string i)}
  | (float as f) {FLOAT (float_of_string f)}
  | '"' (string_body as s) '"' {STR (decode s)}
  | '\'' (char_body as s) '\'' {CHAR (decode_char s)}

  | id as s {ID s}
  | slow as s {SLOW s}
  | ap as s {AP s}
  | plus as s {PLUS s}
  | times as s {TIMES s}
  | exp as s {EXP s}
  | post as s {POST s}

and comment level = parse
  | "*/" {if level = 0 then token lexbuf
          else comment (level-1) lexbuf}
  | "/*" {comment (level+1) lexbuf}
  | _    {comment level lexbuf}
