%{
  open Batteries

  open Parse_proc
  open T
%}

%token
  FN VAL LET IN OPEN MIX USE INST
  PUB OPAQ TYPE SPEC NEW DATA
  MOD SIG BEGIN END WITH TO AS

  PLUS MINUS TIMES DIV
  EXP CONS SNOC APPEND
  RANGE CMP EQ NEQ
  ASSIGN UPDATE

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

%right ASSIGN

%left CMP
%left EQ NEQ
%left SNOC
%right APPEND
%right CONS
%right RANGE
%left PLUS MINUS
%left TIMES DIV
%right EXP

%right COLON DCOLON

%start<Parse_proc.T.t> program

%%

program: 
  | s EOF {S $1}
  | e EOF {E $1}

spec: spec_t {$1, $loc}
%inline spec_t: 
  | SPEC ID MINUS ty    {SPSp ($2, $4)}
  | TYPE ID ASSIGN ty   {SPTy ($2, $4)}
  | TYPE ID             {SPAbst_ty ($2, $loc)}
  | DATA ID ASSIGN data {SPData ($2, $4)}

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
  | ID                            {TId $1}
  | ty_body constr_arrow ty_term  {TFn ($1, $3)}
  | ty_body access ty_body        {TAccess ($1, $2, $3)}
  | nonempty_list(ty_term)        {TSq $1}

ty_term: 
  | ty_term_t {$1, $loc}
  | grouped(ty_body) {$1}
%inline ty_term_t: 
  | LPAREN RPAREN     {TUnit}
  | METATYPE          {TMetatype $1}
  | to_like(WITH, ty) {TWith $1}

  | arr_like(ty_body)   {TArr $1}
  | quoted(ty_body)     {TQuoted $1}
  | mod_like(SIG, spec) {TSig $1}

data: data_t {$1, $loc}
%inline data_t: 
  | arr_like(pair(opt_unit(ty), SUM)) {DSum $1}
  | record_like(LBRACE, ty)           {DProd $1}
  | record_like(PBRACE, ty)           {DRows $1}

constr_arrow: pair(ASSIGN, RANGLE) {$1}
%inline abst_arrow:   pair(MINUS, RANGLE)  {$1}

%inline opt_unit(t): ioption(t) 
  {match $1 with Some s -> s | None -> ([], (TUnit, $loc)), $loc}

s: s_t {$1, $loc}
%inline s_t: 
  | ioption(pub) ioption(use_mods) FN p ASSIGN e
    {Fn (filter_id [$1; $2], $4, $6)}
  | ioption(pub) ioption(use_mods) VAL p ASSIGN e
    {Val (filter_id [$1; $2], $4, $6)}
  // | pub ioption(use_mods) p ASSIGN e
  //   {Val ($1 :: filter_id [$2], $3, $5)}
  // | use_mods p ASSIGN e
  //   {Val ([$1], $2, $4)}
  | OPEN e {Open $2}
  | USE e {Use $2}
  | MIX ioption(inst) e {Mix (filter_id [$2], $3)}
  | ioption(access_mods) ioption(use) ioption(new_)
    TYPE ID ASSIGN ty {Ty (filter_id [$1; $2; $3], $5, $7)}
  | TYPE ID {Abst_ty $2}
  | ioption(access_mods) DATA ID ASSIGN data
    {Data (filter_id [$1], $3, $5)}
  | mod_like(BEGIN, s) {Begin $1}

%inline any_id:
  | ID      {$1}
  | CAP_ID  {$1}

%inline named_arg: pair(LABEL, grouped(e)) {$1, $loc}

%inline new_: NEW   {`new_ty}
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
  | e access e          {Access ($1, $2, $3)}
  | e bop e             {Bop ($1, $2, $3)}
  | nonempty_list(term) {Sq $1}

term: 
  | term_t {$1, $loc}
  | grouped(e) {$1}
%inline term_t: 
  | ID                {Id $1}
  | SYMBOL            {Id $1}
  | USCORE            {Id "_"}
  | to_like(TO, e)    {To $1}
  | to_like(AS, p)    {As $1}
  | to_like(WITH, ty) {With $1}
  | named_arg         {Named $1}
  | METATYPE          {Metatype $1}
  | SUM               {Sum $1}
  | ATOM              {Atom $1}
  
  | to_like(UPDATE, record_like(LBRACE, e)) {Rec_upd $1}

  | STR   {Str $1}
  | CHAR  {Char $1}
  | INT   {Int $1}
  | FLOAT {Flo $1}

  | LPAREN RPAREN {Unit}
  | LANGLE RANGLE {Quoted (Fun [], $loc)}

  | grouped(bop)          {Sect $1}
  | grouped(pair(bop, e)) {let b, e = $1 in Sect_left (b, e)}
  | grouped(pair(e, bop)) {let e, b = $1 in Sect_right (e, b)}
  | quoted(bop)           {Quoted (Sect $1, $loc)}
  | quoted(pair(bop, e))  {let b, e = $1 in Quoted (Sect_left (b, e), $loc)}
  | quoted(pair(e, bop))  {let e, b = $1 in Quoted (Sect_right (e, b), $loc)}
  
  | delimited(LET, s, IN)   {Let $1}
  | mod_like(MOD, s)        {Mod $1}
  | record_like(LBRACE, e)  {Prod $1}
  | record_like(PBRACE, e)  {Rows $1}
  | tuple_like(e)           {Tup $1}
  | quoted(e)               {Quoted $1}

  | arr_like(e)     {Arr $1}
  | func_like(p, e) {Fun $1}

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

%inline bop: bop_t {$1, $loc}
%inline bop_t: 
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

  | CONS  {"-<"}
  | SNOC  {">-"}

p: p_t {$1, $loc}
%inline p_t: 
  | p pbop p              {PBop ($1, $2, $3)}
  | p access p            {PAccess ($1, $2, $3)}
  | nonempty_list(p_term) {PSq $1}

p_term: 
  | p_term_t {$1, $loc}
  | grouped(p) {$1}
p_term_t: 
  | ID        {PId $1}
  | named_arg {PNamed $1}
  | USCORE    {PWildcard}
  | BLANK     {PWildcard}
  
  | INT           {PInt $1}
  | FLOAT         {PFlo $1}
  | CHAR          {PChar $1}
  | STR           {PStr $1}
  | LPAREN RPAREN {PUnit}
  | SUM           {PSum $1}
  | ATOM          {PAtom $1}

  | tuple_like(p)           {PTup $1}
  | record_like(LBRACE, p)  {PProd $1}
  | record_like(PBRACE, p)  {PRows $1}
  | arr_like(p)             {PArr $1}
  | func_like(e, p)         {PDict $1}
  | quoted(p)               {PQuoted $1}

%inline pbop: pbop_t {$1, $loc}
%inline pbop_t: 
  | CONS  {"-<"}
  | SNOC  {">-"}
  | PLUS  {"+"}
  | TIMES {"*"}
