{
  open! Batteries
  open Lexing

  open Parse
}

let eol = ['\r' '\n'] | '\r' '\n'
let whitespace = ' '+

let id_char = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']
let id_tail = ('-'? id_char)*
let id = ['a'-'z'] id_tail
let cap_id = ['A'-'Z'] id_tail

rule token = parse
  | "/*"        {comment 0 lexbuf}
  | eof         {EOF}
  | eol         {new_line lexbuf; token lexbuf}
  | whitespace  {token lexbuf}
  | '\t'        {token lexbuf}

  | "="         {ASSIGN}
  | "in"        {IN}
  | "::"        {WITHIN}
  | "->"        {ARROW}
  | ":"         {SPECIFY}
  | "--"        {BAR}
  | "."         {DOT}
  | "$"         {DOLLAR}

  | "["         {LBRACK}
  | "]"         {RBRACK}
  | "{"         {LBRACE}
  | "}"         {RBRACE}

  | "|"         {PIPE}

  | "fn"        {FN}
  | "pt"        {PT}
  | "mt"        {MT}
  | "rl"        {RL}
  | "*"         {AND}
  | "+"         {XOR}
  | ";"         {SEMI}

  | '"' (id as s) {COSTACK_VAR s}
  | "'" (id as s) {STACK_VAR s}
  | id as s     {VAR s}
  | cap_id as s {CAP s}

and comment level = parse
  | "*/" {if level = 0 then token lexbuf
          else comment (level-1) lexbuf}
  | "/*" {comment (level+1) lexbuf}
  | _    {comment level lexbuf}
