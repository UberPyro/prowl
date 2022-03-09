%{
  open Batteries

  module M = struct
    'a t = 'a * Parsing.position
  end
  
  module T = Ast(M)
%}

%token
  FN VAL LET IN OPEN MIX USE INST
  PUB OPAQ TYPE NEW DATA
  MOD SIG BEGIN END WITH
  TO AS

  PLUS MINUS TIMES DIV
  EXPR CONS SNOC APPEND
  RANGE CMP EQ NEQ
  ASSIGN UPDATE

  LANGLE   RANGLE
  LPAREN   RPAREN
  LBRACKET RBRACKET
  LBRACE   RBRACE

  PBRACE ABRACE PABRACE
  USCORE BLANK EOF

%token<string>
  STR SUM ATOM
  LABEL METATYPE ID
  ACCESS PACCESS MACRO

%token<int> INT
%token<float> FLOAT
%token<char> CHAR

%right ARROW

%left CMP
%left EQ NEQ
%left SNOC
%right CONS
%left PLUS MINUS
%left TIMES DIV

%nonassoc ACCESS PACCESS MACRO

%start<T.t> program

%%

program: 
  | stmt EOF {`stmt $1}
  | expr EOF {`expr $1}

spec: spec_t {$1, $loc}
%inline spec_t: 
  | SPEC id MINUS ty {`sp ($2, $4)}
  | TYPE id ASSIGN ty {`ty ($2, $4)}
  | TYPE id {`abst_ty $2}
  | DATA id ASSIGN data {`data ($2, $4)}

ty: ty_t {$1, $loc}
%inline ty_t: 
  | pair(terminated(
      separated_nonempty_list(COMMA, type_constr), 
      pair(ASSIGN, RANGLE) )) {$1}
  | ty_body {([], $1)}

%inline type_constr: pair(id, nonempty_list(id)) {$1, $loc}

ty_body: ty_body_t {$1, $loc}
%inline ty_body_t: 
  | id {`id $1, $loc}
  | ty_body constr_arrow ty_body {`fn ($1, $3)}
  | nonempty_list(ty_term) {`sq $1}

ty_term: ty_term_t {$1, $loc}
ty_term_t: 
  | LPAREN RPAREN {`unit}
  | METATYPE {`metatype $1}
  | to_like(WITH, ty) {`with_ $1}

  | arr_like(ty_body) {`arr $1}
  | grouped(ty_body) {$1}
  | quoted(ty_body) {`quoted $1}
  | mod_like(SIG, spec) {`sig_ $1}

data: data_t {$1, $loc}
data_t: 
  | arr_like(pair(opt_unit(ty), SUM)) {`sum $1}
  | record_like(ID, ty) {`prod $1}




%inline constr_arrow: pair(ASSIGN, RANGLE) %prec ARROW {$1}

%inline opt_unit(t): ioption(t) 
  {match $1 with Some s -> s | None -> [], TUnit}
