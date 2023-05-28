open! Batteries

module type T = sig
  type t [@@deriving show]
end

module Make(M : T) = struct

  type expr = _expr * M.t
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
    | UVar of string
    | Let of stmt list * expr
    | Ex of string list * expr

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
    | Ne
  
  (* unary (dataflow) operators *)
  and uop = 
    | Dag
  
  (* dataflow operators *)
  and dop = 
    | Tensor
    | Ponder
    | Fork
    | Pick
    
    | Jux
    | Union
  
  (* nullary/stack operators *)
  and nop = 
    | Gen | Fab | Exch | Elim | Cmp
    | Dup | Zap | Swap | Cons | Dip | Cat | Unit
    | DivMod | Lin | Bin | Parse | Show
  
  and lit = 
    | Int of int
    | String of string

    | Quote of expr
    | List of expr list
  
  and stmt = _stmt * M.t
  and _stmt = 
    | Def of string * expr
    [@@deriving show]
  
  type toplevel = stmt list [@@deriving show]

end
