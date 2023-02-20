open! Batteries

type det = {total:bool; bare:bool}
type mode = det * det

type kind = 
  | KVar of Var.t
  | KSeq of Var.t
  | KCat of kind list
  | KDag of kind

type ty = 
  | TId of string
  | TVar of Var.t
  | TStack of Var.t
  | TCostack of Var.t
  | TCat of ty list
  | TDag of ty
  | TQuote of rel
  | TList of rel

and rel = ty * mode

and data = Tag of ty * string

type expr = 
  | Id of string
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
  | Block of expr * mode
  | Tag of string

  | Let of (string * ty option * expr) list * expr
  | Unop of expr * string
  | Binop of expr * string * expr
  | SectLeft of string * expr
  | SectRight of expr * string
  | Sect of string

type def = 
  | Val of string * ty option * expr
  | Type of ty
  | Data of data
