open! Batteries

type kind = _kind * Span.t [@@deriving show]
and _kind = 
  | KVar
  | KSeq
  | KCat of kind list
  | KDag of kind
  [@@deriving show]

type ty = _ty * Span.t [@@deriving show]
and _ty = ty_expr * Mode.t [@@deriving show]

and ty_expr = _ty_expr * Span.t [@@deriving show]
and _ty_expr = 
  | TId of string
  | TVar of Var.t
  | TStack of Var.t
  | TCostack of Var.t
  | TCat of ty list
  | TDag of ty
  | TQuote of ty
  | TList of ty
  [@@deriving show]

and data = _data * Span.t [@@deriving show]
and _data = Tag of ty * string [@@deriving show]

type expr = _expr * Span.t [@@deriving show]
and _expr = 
  | Id of string
  | Var of Var.t
  | Stack of Var.t
  | Costack of Var.t
  | Cat of expr list
  | Dag of expr
  | New of string

  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Quote of expr
  | List of expr list
  | Block of expr * Mode.t
  | Tag of string

  | Let of (string * ty option * expr) list * expr
  
  | Unop of expr * string
  | Binop of expr * string * expr
  | SectLeft of string * expr
  | SectRight of expr * string
  | Sect of string
  | Arrow of string * string
  [@@deriving show]

type def = _def * Span.t [@@deriving show]
and _def = 
  | Val of string * ty option * expr
  | Type of ty
  | Data of data
  [@@deriving show]
