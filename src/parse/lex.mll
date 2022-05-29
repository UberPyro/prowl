{ open Batteries
  open Lexing
  
  open Lex_proc
  open Parse    }

let eol = ['\r' '\n'] | '\r' '\n'
let whitespace = ' '+

let digit = ['0'-'9']
let sig_digits = ['1'-'9'] digit*

let hex_digit = digit | ['A'-'F' 'a'-'f']

let hex = 
    hex_digit
  | hex_digit hex_digit
  | hex_digit hex_digit hex_digit
  | hex_digit hex_digit hex_digit hex_digit

let id_char = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']
let id_tail = ('-'? id_char)*
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

let comb = (['^' '_' '%' '$'] integer?)+

let suffix = ['?' '+' '*']
let greed = ['?' '+']?

rule token = parse
  | "/*"        {comment 0 lexbuf}
  | eof         {EOF}
  | eol         {new_line lexbuf; set_cat(); token lexbuf}
  | whitespace  {set_cat(); token lexbuf}
  | '\t'        {set_cat(); advance lexbuf; token lexbuf}

  | "as"  (infix? as s) {AS s}
  | "let" (infix? as s) {LET s}
  | "and" (infix? as s) {AND s}

  | '_' ['A'-'Z' 'a'-'z'] id_tail {BLANK}
  | comb as s {set_regex(); COMB (parse_comb s)}  (* modify to exclude _? *)
  | (((suffix as s) (greed as g)) as z) {
    if !mode == Cat then begin match z with
        | "*" -> TIMES
        | "+" -> PLUS
        | "++" -> APPEND
        | s -> SYMBOL s end
    else QUANT (parse_quant s g)}

  | "def"   {DEF}
  | "spec"  {SPEC}
  | "open"  {OPEN}
  | "mix"   {MIX}
  | "impl"  {IMPL}
  | "priv"  {PRIV}
  | "opaq"  {OPAQ}
  | "type"  {TYPE}
  | "sig"   {SIG}
  | "data"  {DATA}

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
  | ">=>"  {KLEISLI}
  | "|"    {ALT}
  | "|?"   {ALT_REL}
  | "|+"   {ALT_CUT}
  | "&"    {CAT}
  | "&&"   {INTERSECT}

  | "==" {EQ}
  | "/=" {NEQ}
  | "<"  {LT}
  | "<=" {LE}
  | ">"  {GT}
  | ">=" {GE}

  | ":"  {COLON}
  | "="  {ASSIGN}
  | "--" {EFFECT}
  | "->" {ARROW}
  | "~"  {TILDE}
  | "=>" {WIDE_ARROW}
  | "<-" {BACKARROW}
  | "."  {DOT}
  | ","  {set_cat(); COMMA}
  | ";" (greed as g) {set_cat(); SEMICOLON (parse_greed g)}

  | "(?:" {set_cat(); NONCAP_BRACK}
  | "(?>" {set_cat(); ATOM_BRACK}
  | "[^"  {set_cat(); INV_BRACK}
  
  | "[<"  {set_cat(); IMPL_LBRACK}
  | ">]"  {IMPL_RBRACK}

  | "(" {set_cat(); LPAREN}
  | "[" {set_cat(); LBRACK}
  | "{" {if !mode == Cat then LBRACE else TIMES_BRACK}
  | ")" {set_regex(); RPAREN}
  | "]" {set_regex(); RBRACK}
  | "}" (greed as g) {set_regex(); RBRACE (parse_greed g)}
  | "do"  {set_cat(); DO}
  | "mod" {set_cat(); MOD}
  | "end" {set_regex(); END}

  | (integer as i) {set_regex(); INT (int_of_string i)}
  | (float   as f) {set_regex(); FLOAT (float_of_string f)}
  | '"' (string_body as s) '"' {set_regex(); STR (decode s)}
  | '\'' (char_body as s) '\'' {set_regex(); CHAR s}
  | '\'' ("\\x" hex) as s '\'' {set_regex(); CHAR (decode s).[0]}
  | '\'' '\\' (_ as c) '\''    {set_regex(); CHAR c}
  | "<>"  {set_regex(); UNIT}
  | "<;>" {set_regex(); VOID}

  | id     as s {set_regex(); ID s}
  | cap_id as s {CAP s}
  | symbol as s {SYMBOL s}
  | infix  as s {INFIX s}

and comment level = parse
  | "*/" {if level = 0 then token lexbuf
          else comment (level-1) lexbuf}
  | "/*" {comment (level+1) lexbuf}
  | _    {comment level lexbuf}
