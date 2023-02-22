open! Batteries

type kind = 
  | KVar of Var.t
  | KSeq of Var.t
  | KCat of kind list
  | KDag of kind

type ty = 
  | TId of Name.t
  | TVar of Var.t
  | TStack of Var.t
  | TCostack of Var.t
  | TCat of ty list
  | TDag of ty
  | TQuote of rel
  | TList of rel

and rel = ty * Mode.t

and data = Tag of ty * Name.t

type expr = 
  | Id of Name.t
  | Var of Var.t
  | Stack of Var.t
  | Costack of Var.t
  | Cat of expr list
  | Dag of ty

  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Quote of expr
  | List of expr list
  | Block of expr * Mode.t
  | Tag of Name.t

  | Let of (Name.t * ty option * expr) list * expr
  | Unop of expr * Name.t
  | Binop of expr * Name.t * expr
  | SectLeft of Name.t * expr
  | SectRight of expr * Name.t
  | Sect of Name.t

type def = 
  | Val of Name.t * ty option * expr
  | Type of ty
  | Data of data
