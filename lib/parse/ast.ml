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

    | Var of string
    | UVar of string
    | Let of stmt list * expr
    | Ex of string list * expr

  (* binary operators *)
  and bop = _bop * M.t
  and _bop = 
    | Add
    | Sub
    | Mul
    | DivMod

    | Eq
    | Neq
    | Gt
    | Lt
    | Ge
    | Ne
  
  (* unary (dataflow) operators *)
  and uop = _uop * M.t
  and _uop = 
    | Dag
  
  (* dataflow operators *)
  and dop = _dop * M.t
  and _dop = 
    | Tensor
    | Ponder
    | Fork
    | Pick
    
    | Jux
    | Union
  
  (* nullary/stack operators *)
  and nop = _nop * M.t
  and _nop = 
    | Gen | Fab | Exch | Elim
    | Dup | Zap | Swap | Cons | Dip | Cat | Unit
    | Lin | Parse | Show
  
  and stmt = _stmt * M.t
  and _stmt = 
    | Def of string * expr
    [@@deriving show]
  
  type toplevel = stmt list [@@deriving show]

end
