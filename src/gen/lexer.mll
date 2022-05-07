{ open Batteries
  open Lexing
  
  open Lex_proc
  open Parse    }

let digit = ['0'-'9']
let sig_digits = ['1'-'9'] digit*

let hex_digit = digit | ['A'-'F' 'a'-'f']

let hex = 
    hex_digit
  | hex_digit hex_digit
  | hex_digit hex_digit hex_digit
  | hex_digit hex_digit hex_digit hex_digit

let id_tail = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'' '-' '.']*
let id = ['a'-'z'] id_tail
let cap_id = ['A'-'Z'] id_tail

let op_char = [
  '~' '!' '@' '#' '$' '%' '^' '&' '*' '-' '=' '+'
  '.' '?' ':' '|' '/' '\\'
]

let symbol = op_char+
let infix = (op_char | ['<' '>'])+

let integer = '0' | sig_digits
let float = sig_digits '.' digit* | '.' digit+

let char_body = [^ '\'']
let string_body = ([^ '"'] | "\\\"")*

let stack_comb = (['^' '_' '%' '$'] integer?)+

let suffix = ['?' '+' '*']
let greed = ['?' '+']?

rule token = parse
  | "as"  (infix as s) {AS s}
  | "let" (infix as s) {LET s}
  | "and" (infix as s) {AND s}

  | stack_comb as s {set_regex(); STACK_COMB (parse_comb s)}
  | ((suffix greed) as s) {is_cat (SYMBOL s) (parse_suffix s)}

  | "def"   {DEF}
  | "open"  {OPEN}
  | "mix"   {MIX}
  | "use"   {USE}
  | "impl"  {IMPL}
  | "pub"   {PUB}
  | "opaq"  {OPAQ}
  | "type"  {TYPE}
  | "alias" {ALIAS}
  | "class" {CLASS}
  | "sig"   {SIG}
  | "do"    {DO}

  | "+"  {PLUS}
  | "-"  {MINUS}
  | "*"  {TIMES}
  | "/"  {DIVIDE}

  | "**" {EXP}
  | ".." {RANGE}
  | ">-" {SNOC}
  | "-<" {CONS}

  | "++"   {APPEND}
  | ">>="  {BIND}

  | "|"    {ALT}
  | ">>-"  {CAT}

  | '_'         {USCORE}
  | '_' id_tail {BLANK}

  | "?=" {CMP}
  | "==" {EQ}
  | "/=" {NEQ}
  | "<"  {LT}
  | "<=" {LE}
  | ">"  {GT}
  | ">=" {GE}

  | ":"  {COLON}
  | "~"  {TILDE}
  | "="  {ASSIGN}
  | "->" {ARROW}

  | ","  {set_cat(); COMMA}
  | ";"  {set_cat(); SEMICOLON}

  | "(?:" {set_cat(); NONCAP_BRACK}
  | "(?>" {set_cat(); ATOM_BRACK}

  | "(" {set_cat(); LPAREN}
  | "[" {set_cat(); LBRACK}
  | "{" {set_cat(); is_cat LBRACE TIMES_BRACK}
  | ")" {set_regex(); RPAREN}
  | "]" {set_regex(); RBRACK}
  | "}" {set_regex(); RBRACE}
  | "do"  {set_cat(); DO}
  | "mod" {set_cat(); MOD}
  | "end" {set_regex(); END}

  | (integer as i) {set_regex(); INT (int_of_string i)}
  | (float   as f) {set_regex(); FLOAT (float_of_string f)}
  | '"' (string_body as s) '"' {set_regex(); STR (decode s, t)}
  | '\'' (char_body as s) '\'' {set_regex(); CHAR (s.[0], t)}
  | '\'' ("\\x" hex) as s '\'' {set_regex(); CHAR ((decode s).[0], t)}
  | '\'' '\\' (_ as c) '\''    {set_regex(); CHAR (c, t)}
  | "<>" {set_regex(); UNIT}

  | cap_id as s {CAP s}
  | symbol as s {SYMBOL s}
  | infix as s  {INFIX s}

  | "/*" {comment 0 lexbuf}
  | eof  {EOF}
  | ' '  {set_cat(); token lexbuf}
  | '\t' {set_cat(); advance lexbuf; token lexbuf}

and comment level = parse
  | "*/" {if level = 0 then token lexbuf
          else comment (level-1) lexbuf}
  | "/*" {comment (level+1) lexbuf}
  | _ {comment level lexbuf}
