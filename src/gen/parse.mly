%{
  open Batteries

  open Parse_proc

  module M = struct
    type 'a m = 'a * Lexing.position
  end
  
  module T = Ast.T(M)
%}

%token
  FN VAL LET IN OPEN MIX USE INST
  PUB OPAQ TYPE SPEC NEW DATA
  MOD SIG BEGIN END WITH TO AS

  PLUS MINUS TIMES DIV
  EXP CONS SNOC APPEND
  RANGE CMP EQ NEQ AND
  OR ASSIGN UPDATE

  LANGLE   RANGLE
  LPAREN   RPAREN
  LBRACKET RBRACKET
  LBRACE   RBRACE

  PBRACE COLON DCOLON
  COMMA USCORE BLANK EOF

%token<string>
  STR SUM ATOM
  LABEL METATYPE
  ID CAP_ID SYMBOL

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

%right COLON DCOLON

%start<T.t> program

%%

program: 
  | s EOF {`s $1}
  | e EOF {`e $1}

spec: spec_t {$1, $loc}
%inline spec_t: 
  | SPEC ID MINUS ty    {`sp ($2, $4)}
  | TYPE ID ASSIGN ty   {`ty ($2, $4)}
  | TYPE ID             {`abst_ty $2}
  | DATA ID ASSIGN data {`data ($2, $4)}

ty: ty_t {$1, $loc}
%inline ty_t: 
  | separated_pair(
      separated_nonempty_list(COMMA, type_constr), 
      pair(ASSIGN, RANGLE), 
      ty_body)  {$1}
  | ty_body     {([], $1)}

%inline type_constr: pair(ID, nonempty_list(ID)) {$1, $loc}

ty_body: ty_body_t {$1, $loc}
%inline ty_body_t: 
  | ID                            {`id $1}
  | ty_body constr_arrow ty_body  {`fn ($1, $3)}
  | ty_body access ty_body        {`accop ($1, $2, $3)}
  | nonempty_list(ty_term)        {`sq $1}

ty_term:
  | LPAREN RPAREN     {`unit}
  | METATYPE          {`metatype $1}
  | to_like(WITH, ty) {`with_ $1}

  | arr_like(ty_body)   {`arr $1}
  | grouped(ty_body)    {$1}
  | quoted(ty_body)     {`quoted $1}
  | mod_like(SIG, spec) {`sig_ $1}

data: data_t {$1, $loc}
%inline data_t: 
  | arr_like(pair(opt_unit(ty), SUM)) {`sum $1}
  | record_like(LBRACE, ty)           {`prod $1}
  | record_like(PBRACE, ty)           {`rows $1}

%inline constr_arrow: pair(ASSIGN, RANGLE) %prec ARROW {$1}
%inline abst_arrow:   pair(MINUS, RANGLE)  %prec ARROW {$1}

%inline opt_unit(t): ioption(t) 
  {match $1 with Some s -> s | None -> [], TUnit}

s: s_t {$1, $loc}
%inline s_t: 
  | ioption(pub) ioption(use_mods) FN p ASSIGN e
    {`fn (filter_id [$1; $2], $4, $6)}
  | ioption(pub) ioption(use_mods) VAL p ASSIGN e
    {`val_ (filter_id [$1; $2], $4, $6)}
  | OPEN e {`open_ $2}
  | MIX ioption(inst) e {`incl (filter_id [$2], $3)}
  | USE e {`use $2}
  | ioption(access_mods) ioption(use) ioption(new_)
    TYPE ID ASSIGN ty {`ty (filter_id [$1; $2; $3], $5, $7)}
  | TYPE ID {`abst_ty $2}
  | ioption(access_mods) DATA ID ASSIGN data
    {`data (filter_id [$1], $3, $5)}
  | mod_like(BEGIN, s) {`begin_ $1}

%inline any_id:
  | ID      {$1}
  | CAP_ID  {$1}

%inline named_arg: pair(LABEL, grouped(e)) {$1}

%inline new_: NEW   {`new_}
%inline inst: INST  {`inst}
%inline use: USE    {`use}
%inline pub: PUB    {`pub}

%inline use_mods: 
  | USE   {`use}
  | INST  {`inst}

%inline access_mods: 
  | PUB   {`pub}
  | OPAQ  {`opaq}

e: e_t {$1, $loc}
%inline e_t: 
  | e access e          {`accop ($1, $2, $3)}
  | e bop e             {`bop ($1, $2, $3)}
  | nonempty_list(term) {`sq $1}

term: 
  | ID                {`id $1}
  | SYMBOL            {`id $1}
  | USCORE            {`id "_"}
  | to_like(TO, ty)   {`to_ $1}
  | to_like(AS, e)    {`as_ $1}
  | to_like(WITH, ty) {`with_ $1}
  | named_arg         {`named $1}
  | METATYPE          {`metatype $1}
  | SUM               {`sum $1}
  | ATOM              {`atom $1}
  
  | to_like(UPDATE, record_like(LBRACE, e)) {`rec_upd $1}

  | STR   {`str $1}
  | CHAR  {`char $1}
  | INT   {`int $1}
  | FLOAT {`flo $1}

  | LPAREN RPAREN {`unit}
  | LBRACE RBRACE {`fun_ []}
  | LANGLE RANGLE {`quoted (`fun_ [])}

  | grouped(bop)          {`sect $1}
  | grouped(pair(bop, e)) {let b, e = $1 in `sect_left (b, e)}
  | grouped(pair(e, bop)) {let e, b = $1 in `sect_right (e, b)}
  
  | delimited(LET, s, IN)   {`let_ $1}
  | mod_like(MOD, s)        {`mod_ $1}
  | record_like(RBRACE, e)  {`prod $1}
  | record_like(PBRACE, e)  {`rows $1}
  | tuple_like(e)           {`tup $1}

  | arr_like(e)     {`arr $1}
  | func_like(p, e) {`fun_ $1}
  | quoted(e)       {`quoted $1}
  | grouped(e)      {$1}

%inline func_like(key, value): delimited(
    LBRACE, 
    separated_list(COMMA, separated_pair(key, abst_arrow, value)), 
  RBRACE)                                                               {$1}

%inline record_like(lsep, value): delimited(
    lsep, 
    separated_nonempty_list(COMMA, separated_pair(any_id, ASSIGN, value)), 
  RBRACE)                                                               {$1}

%inline arr_like(elem): 
  delimited(LBRACKET, separated_list(COMMA, elem), RBRACKET)            {$1}

%inline tuple_like(elem): 
  grouped(separated_pair(elem, COMMA, 
  separated_nonempty_list(COMMA, elem)))           {let h, t = $1 in h :: t}

%inline mod_like(open_kw, s): delimited(open_kw, list(s), END)          {$1}

%inline to_like(kw, content): preceded(kw, grouped(content))            {$1}

%inline grouped(expr_like): delimited(LPAREN, expr_like, RPAREN)        {$1}
%inline quoted(expr_like): delimited(LANGLE, expr_like, RANGLE)         {$1}

%inline access: 
  | COLON   {`access}
  | DCOLON  {`poly_access}

%inline bop: 
  | PLUS  {"+"}
  | MINUS {"-"}
  | TIMES {"*"}
  | DIV   {"/"}
  | EXP   {"**"}

  | APPEND  {"++"}
  | RANGE   {".."}

  | EQ    {"=="}
  | NEQ   {"/="}
  | CMP   {"?="}
  | AND   {"/\\"}
  | OR    {"\\/"}

  | CONS  {"-<"}
  | SNOC  {">-"}

p: p_t {$1, $loc}
p_t: 
  | p pbop p              {`bop ($1, $2, $3)}
  | p access p            {`access ($1, $2, $3)}
  | nonempty_list(p_term) {`sq $1}

p_term: 
  | ID        {`id $1}
  | named_arg {`named $1}
  | USCORE    {`wildcard}
  | BLANK     {`wildcard}
  
  | INT           {`int $1}
  | FLOAT         {`flo $1}
  | CHAR          {`char $1}
  | STR           {`str $1}
  | LPAREN RPAREN {`unit}
  | SUM           {`sum $1}
  | ATOM          {`atom $1}

  | tuple_like(p)           {`tup $1}
  | record_like(LBRACE, p)  {`prod $1}
  | record_like(PBRACE, p)  {`rows $1}
  | arr_like(p)             {`arr $1}
  | func_like(e, p)         {`dict $1}
  | grouped(p)              {$1}
  | quoted(p)               {`quoted $1}

%inline pbop: 
  | CONS {"-<"}
  | SNOC {">-"}
  | OR   {"\\/"}
