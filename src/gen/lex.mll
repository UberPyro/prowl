{ open Tokens
  open Lex_proc }


let eol = '\r' | '\n' | '\r' '\n'
let whitespace = eol | [' ' '\t']

let digit = ['0'-'9']
let sig_digits = ['1'-'9'] digit*
let hex_digit = digit | ['A'-'F' 'a'-'f']

let hex = hex_digit
        | hex_digit hex_digit
        | hex_digit hex_digit hex_digit
        | hex_digit hex_digit hex_digit hex_digit

let id_tail = ['A'-'Z' 'a'-'z' '0'-'9' '_']*'\''*

let id = ['a'-'z'] id_tail
let cap_id = ['A'-'Z'] id_tail
let any_id = id | cap_id

let integer = '0' | sig_digits
let float = sig_digits '.' digit* | '.' digit+

let char_single = [^ '\'']
let char_ascii = '\\' hex

let string_body = ([^ '"'] | "\\\"")*

rule token = parse
  | integer as i {INTEGER (int_of_string i)}
  | float as f {FLOAT (float_of_string f)}
  | "()" {UNIT}
  | "{}" {NOP}
  | "<>" {QNOP}
  | "/#" {comment 0 lexbuf}
  | "'" (char_single as c) "'" {CHAR c}
  | "'" '\\' (_ as c) "'" {CHAR c}
  | "'" (char_ascii as c) "'" {CHAR (decode_char c)}
  | '"' (string_body as s) '"' {STRING (decode2 s)}
  | "/#" {comment 0 lexbuf}
  
  | ':' (id as s) {VARIANT s}
  | ':' (cap_id as s) {PVARIANT s}
  | "--" (any_id as s) {METATYPE s}
  | '~' (any_id as s) {LABEL s}
  | (any_id as s) '.' {MOD_ID s}
  | (any_id as s) ':' {PMOD_ID s}
  | (any_id as s) "::" {MACRO_ID s}
  | id as s {ID s}
  | cap_id as s {CAP_ID s}

  | "fn" {FN}
  | "val" {VAL}
  | "let" {LET}
  | "in" {IN}
  | "rec" {REC}
  | "open" {OPEN}
  | "incl" {INCL}
  | "spec" {SPEC}
  | "imp" {IMP}
  | "inst" {INST}
  | "pub" {PUB}
  | "opaq" {OPAQ}
  | "type" {TYPE}
  | "new" {NEW}
  | "data" {DATA}
  | "mod" {MOD}
  | "sig" {SIG}
  | "begin" {BEGIN}
  | "end" {END}
  | "to" {TO}
  | "with" {WITH}
  | "pat" {PAT}

  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {TIMES}
  | "/" {DIV}
  | "=" {ASSIGN}

  | "?=" {CMP}
  | "==" {EQ}
  | "/=" {NEQ}

  | "&&" {AND}
  | "||" {OR}

  | ":$" {CONS}
  | "$:" {SNOC}
  | "$" {DOLLAR}
  | "@" {AT}
  | "%" {PERCENT}
  | "^" {CARET}
  | "-^" {FOLDL}
  | "^-" {FOLDR}
  | "?" {QMARK}
  | "!" {EMARK}
  | "|" {PIPE}
  | "&" {AMPERSAND}

  | "<" {LANGLE}
  | ">" {RANGLE}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "[" {LBRACKET}
  | "]" {RBRACKET}
  | "{" {LBRACE}
  | "}" {RBRACE}
  | ":{" {PBRACE}

  | "," {COMMA}
  | ":" {COLON}
  | "." {DOT}
  | "->" {ARROW}
  | "<-" {BACKARROW}

  | eof {EOF}
  | whitespace {token lexbuf}

and comment level = parse
  | "#/" {
    if level = 0
    then token lexbuf
    else comment (level-1) lexbuf
  }
  | "/#" {comment (level+1) lexbuf}
  | _ {comment level lexbuf}
