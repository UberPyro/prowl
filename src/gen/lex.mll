{ open Lex_proc }

let eol = '\r' | '\n' | '\r' '\n'
let whitespace = eol | [' ']

let digit = ['0'-'9']
let sig_digits = ['1'-'9'] digit*
let hex_digit = digit | ['A'-'F' 'a'-'f']

let hex = hex_digit
        | hex_digit hex_digit
        | hex_digit hex_digit hex_digit
        | hex_digit hex_digit hex_digit hex_digit

let id_tail = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
let id = ['a'-'z'] id_tail
let cap_id = ['A'-'Z'] id_tail
let any_id = id | cap_id

let sym = ';' | [
        '~' '!' '@' '#' '$' '%' '^' '&' '*' '-' '=' '+'
        '.' '/' '?' ':' '|' '\\'
]+

let integer = '0' | sig_digits
let float = sig_digits '.' digit* | '.' digit+

let string_body = ([^ '"'] | "\\\"")*

rule token = parse
  | "fn"    {FN}
  | "val"   {VAL}
  | "let"   {LET}
  | "in"    {IN}
  | "open"  {OPEN}
  | "mix"   {MIX}
  | "use"   {USE}
  | "inst"  {INST}
  | "pub"   {PUB}
  | "opaq"  {OPAQ}
  | "type"  {TYPE}
  | "new"   {NEW}
  | "data"  {DATA}
  | "mod"   {MOD}
  | "sig"   {SIG}
  | "begin" {BEGIN}
  | "end"   {END}
  | "to"    {TO}
  | "as"    {AS}
  | "with"  {WITH}

  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {TIMES}
  | "/" {DIV}
  
  | "**"  {EXP}
  | ".."  {RANGE}
  | "-<"  {CONS}
  | ">-"  {SNOC}
  | "++"  {APPEND}


  | "="   {ASSIGN}
  | ":="  {UPDATE}

  | "?="  {CMP}
  | "=="  {EQ}
  | "/="  {NEQ}

  | ":{"  {PBRACE}
  | "}:"  {ABRACE}
  | "}::" {PABRACE}
  
  | "<" {LANGLE}
  | ">" {RANGLE}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "[" {LBRACKET}
  | "]" {RBRACKET}
  | "{" {LBRACE}
  | "}" {RBRACE}

  | integer as i                {INT (int_of_string i)}
  | float as f                  {FLOAT (float_of_string f)}
  | "/#"                        {comment 0 lexbuf}
  | '"' (string_body as s) '"'  {STR (decode2 s)}
  
  | cap_id as s         {SUM s} 
  | "'" (any_id as s)   {ATOM s}
  | '~' (cap_id as s)   {METATYPE s}
  | '~' (id as s)       {LABEL s}
  
  | (id as s) ':'       {ACCESS}
  | (id as s) "::"      {PACCESS}
  | (cap_id as s) ':'   {MACRO}

  | id as s     {ID s}
  | '_'         {USCORE}
  | '_' any_id  {BLANK}

  | eof         {EOF}
  | whitespace  {token lexbuf}

and comment level = parse
  | "#/" {if level = 0 then token lexbuf
          else comment (level-1) lexbuf}
  | "/#" {comment (level+1) lexbuf}
  | _ {comment level lexbuf}
