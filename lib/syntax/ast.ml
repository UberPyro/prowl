open Metadata

(* link = relational function type *)
type 'a link = 'a * 'a option * 'a

type type_expr = _type_expr * Span.t
and _type_expr = 
  | ImplStack of impl_stack_wing link
  | ImplCostack of impl_costack_wing link
  | Expl of expl_wing link

and type_val = 
  | TLit of type_lit
  | TCon of type_con * type_expr
  | TVar of string
and type_lit = 
  | TInt
  | TString
and type_con = 
  | TQuote
  | TList

and stack_elem = 
  | SVar of type_val
  | SStack of string
and impl_stack_wing = stack_elem list

and stack_end = 
  | NextStack of string
  | Unit
and costack_elem = 
  | CStack of stack_end * impl_stack_wing
  | CCostack of string
and impl_costack_wing = costack_elem list

and costack_end = 
  | NextCostack of string
  | Void
and expl_wing = costack_end * impl_costack_wing

type expr = _expr * Span.t (* Types.dc * Types.dc *)
and _expr = 
  | Bop of expr * bop * expr
  | SectLeft of bop * expr
  | SectRight of expr * bop
  | Sect of bop
  | Uop of expr * uop
  | Dop of expr * dop * expr
  | Nop of nop

  | Lit of lit
  | Var of string
  | Let of stmt list * expr

and bop = 
  | Aop of aop
  | Cop of cop

(* arithmetic operators *)
and aop = 
  | Add
  | Sub
  | Mul

(* comparison operators *)
and cop = 
  | Eq
  | Neq
  | Gt
  | Lt
  | Ge
  | Le

(* unary operators *)
and uop = 
  | Dag
  | Mark
  | Plus
  | Star

(* dataflow operators *)
and dop = 
  | Tensor
  | Ponder
  | Fork
  | Pick
  | Cross
  | Guess
  
  | Jux
  | Contra
  | Union

(* nullary/stack operators *)
and nop = 
  | Gen | Fab | Exch | Elim | Cmp
  | Dup | Zap | Swap | Cons | Dip | Cat | Unit
  | DivMod | Lin | Bin | Parse | Show
  | Noop | Id | Ab

and lit = 
  | Int of int
  | String of string

  | Quote of expr
  | List of expr list

and stmt = _stmt * Metadata.Span.t
and _stmt = 
  | Def of string * expr
  [@@deriving show]

type toplevel = stmt list [@@deriving show]
