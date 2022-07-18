module rec Param : sig

  type t = 
    | TyVal of string
    | TyStack of int
    | Span of string

end = Param

and Name : sig
  
  type t = 
    | Word of string
    | Symbol of string

end = Name

and Mod : sig

  type t =
    | Id of string
    | Access of t * string
    | Unit of t
    | New of t
    | With of string option * t * t
    | Seals of string option * t * t
    | Lam of (string * t) list * t
    | Cat of t list
    | USig of port * t * string list
    | Value of Expr.t * t
  
  and port = Import | Export

end = Mod


and Component : sig

  type t = 
    | Val of (Pat.t * Expr.t) list
    | Fun of (Quant.opt * Pat.t * Name.t * Expr.t) list
    | Spec of Name.t * Type.t
    | Type of Param.t list * string * Type.head
    | Data of Param.t list * string * Data.t
    | Kind of Param.t list * string
    | Sig of impl * string * Mod.t
    | Mod of (impl * pat list * string * ascription * Mod.t) list
    | Open of impl * Mod.t
    | From of Mod.t * Name.t list
    | Include of Mod.t
  
  and impl = Implicit | Explicit

  and pat = string * ascription
  and ascription = 
    | Opaque of Mod.t
    | Transparent of Mod.t
    | Blank

end = Component


and Quant : sig

  type t = Mark | Plus | Star
  type greed = Greedy | Reluctant | Possessive
  type opt = t option
  type param = 
    | Quant of t
    | Unit
    | Id of string

end = Quant


and Type : sig

  type t = stack * Quant.opt * stack
  and stack = 
    | StackLit of tail * head list
    | Both of stack * stack
    | Either of stack * stack

  and head = 
    | Id of string
    | Var of string
    | Param of head * param list
    | Quote of t
    | Map of t
    | Sig of Mod.t
    | Mod of Mod.t
    | ImplMod of string * Mod.t
    | Opt of string * head option
    | Access of string * t
  
  and tail = 
    | Num of int
    | Bottom
  
  and param = 
    | TyStack of stack
    | TyVar of head
    | Span of Quant.param

end = Type


and Data : sig
  
  type t = 
    | Record of record
    | Variant of variant list
  
  and record = (string * Type.t option) list
  and variant = 
    | Tacit of Type.head list * string
    | Pointed of record * string

end = Data


and Expr : sig

  type t = 
    | Id of string
    
    | Int of int
    | Float of float
    | String of string
    | Char of char

    | Quote of t
    | Map of (t * t) list
    | Mod of Mod.t * Mod.t option

    | Variant of string
    | Record of string option * (string * t option) list
    | Opt of string * t option
    | Implicit of Mod.t

    | BinOp of t * string * t
    | Sect of string * bkind
    | SectLeft of string * t * bkind
    | SectRight of t * string * bkind

    | SeqOp of string * t list
    | Suffix of t * Quant.t * Quant.greed
    | Exclaim of t

    | AsExpr of Pat.t * Type.t option * t
    | LamExpr of Quant.opt * Pat.t * Name.t * Type.t option * t
    | ValExpr of (Pat.t * t * Type.t option) list
    | FunExpr of (Quant.opt * Pat.t * Name.t * Expr.t * Type.t option) list * t

  and bkind = Pointy | Smooth

end = Expr


and Pat : sig

  type t = 
    | Id of string
    | Asc of t * Type.head
    | Stack of string

    | Int of int
    | Float of float
    | String of string
    | Char of char

    | Quote of t
    | Map of (Expr.t * t) list
    | Mod of Mod.t * Mod.t option

    | Variant of string
    | Record of (string * t option) list * bool  (* true = more args *)
    | Opt of string * t
    | Implicit of string * Mod.t

    | BinOp of t * string * t
    | SeqOp of string * t list
    | Suffix of t * Quant.t * Quant.greed
    | Exclaim of t

end = Pat
