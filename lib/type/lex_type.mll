{ open Parse_type }

let eol = ['\r' '\n'] | '\r' '\n'
let whitespace = [' ' '\t']+

let costack = ['0'-'9']+
let stack = ['a'-'z'] ['0'-'9']*
let var = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let mono = ['a'-'z'] ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
  | "/*" {comment 0 lexbuf}
  | eof {EOF}
  | eol {token lexbuf}
  | whitespace {token lexbuf}

  | "sp" {SP}
  | ':' {HASTYPE}
  | '|' {ALT}

  | "[" {LBRACK}
  | "]" {RBRACK}
  | "{" {LBRACE}
  | "}" {RBRACE}

  | costack as c {COSTACK c}
  | stack as s {STACK s}
  | var as v {VAR v}
  | mono as m {MONO m}

and comment level = parse
  | "*/" {if level = 0 then token lexbuf
          else comment (level-1) lexbuf}
  | "/*" {comment (level+1) lexbuf}
  | _    {comment level lexbuf}
