module Span = struct

  type bound = 
    | Int of int
    | Id of string
    | Unbound
  
  type t = 
    | Exact of bound
    | Range of bound * bound

end


module Parameter = struct

  type t = 
    | Var of string
    | Quote of stack * stack
    | Span of Span.t
  
  and stack = tail * string list
  
  and tail = 
    | Stackvar of int
    | Bottom

end


module Name = struct
  
  type t = 
    | Word of string
    | Symbol of string * hardness
    | Bindop of string

  and hardness = 
    | Soft
    | Hard

end


module rec Mod : sig

  type t =
    | Id of string
    | Access of t * string
    | Unit of t
    | New of t
    | With of string option * t * t
    | Seals of string option * t * t
    | Lam of (string * t) list * t
    | Cat of t list
    | Complete of t
    | Refine of t * Refinement.t list
    | USig of usig
    | Value of Expr.t * t

  and usig = 
    | Import of t * string list
    | Export of t * string list

end = Mod


and Component : sig

  type t = 
    | Def of Pat.t list * Name.t * det_check * Type.t option * Expr.t
    | Spec of Name.t * Type.t
    | Type of Parameter.t list * string * Type.head
    | Kind of Parameter.t list * string
    | Mod of impl * modtype * ascribed list * string * ascryption option * Mod.t
    | ModRec of (impl * ascribed list * ascribed * Mod.t) list
    | Open of impl * Mod.t
    | Mix of Mod.t
    | Tag of Exn.tag
  
  and det_check = 
    | Mark
    | Plus
    | Star
  
  and ascribed = Mod.t * ascryption
  
  and ascryption = 
    | Opaque of Mod.t
    | Transparent of Mod.t
  
  and impl = Implicit | Explicit

  and modtype = 
    | Sig
    | Struct
    | Mixin

end = Component


and Refinement : sig
  
  type t = 
    | Type of Parameter.t list * string * Type.head
    | Kind of Parameter.t list * string

end = Refinement


and Type : sig

  type t = stack algebra * multistack algebra

  and 'a algebra = 
    | Product of 'a algebra * 'a algebra
    | Left of 'a algebra
    | Right of 'a algebra
    | Self of 'a

  and multistack = det * stack * string list

  and stack = tail * head

  and head = 
    | Id of string
    | Var of string
    | Linear of string
    | Span of det
    | Quote of t
    | Map of t
    | Mod of Component.modtype * Mod.t
    | ImplMod of string * Mod.t
    | Opt of string * t option
    | Tensor of t * det list

  and tail = 
    | Stackvar of int
    | Fresh
    | Bottom

  and det = Span.t

end = Type


and Data : sig
  
  type t = 
    | Record of record
    | Variant of Type.head list * string
    | PointedVariant of record * string
  
  and record = string * Type.t option

end = Data


and Exn : sig

  type tag = Type.head list * string * errtype

  and errtype = Checked | Panic

end = Exn


and Expr : sig

  type t = 
    | Id of string
    
    | Int of int
    | Float of float
    | String of string
    | Char of char

    | Quote of t
    | Map of (t * t) list
    | Mod of Component.modtype * Mod.t * Mod.t option
    | Tensor of t list list

    | Variant of string
    | Record of string option * (string * t option) list
    | Opt of string * t option
    | Impl of Mod.t

    | BinOp of t * string * t
    | SectLeft of string * t
    | SectRight of t * string
    | Sect of string

    | HardBind of string
    | HardLeft of string * t
    | HardRight of t * string
    | Hard of string

    | SeqOp of string * t list
    | Suffix of t * quant * greed
    | Exclaim of t * Exn.errtype
    | Linear of t

    | AsExpr of Pat.t list * t
    | AsOp of string * Pat.t list * t
    | LetExpr of (Pat.t list * Name.t * Component.det_check * Type.t option) list * t
    | LetRec of (Pat.t list * Name.t * Component.det_check * Type.t option) list * t
  
  and quant = 
    | Mark
    | Plus
    | Star
  
  and greed = 
    | Greedy
    | Reluctant
    | Possessive

end = Expr


and Pat : sig

  type t = 
    | Id of string
    | Asc of t * Type.head

    | Int of int
    | Float of float
    | String of string
    | Char of char

    | Quote of t
    | Map of (Expr.t * t) list
    | Mod of Component.modtype * Mod.t * Mod.t option
    | Tensor of t list list

    | Variant of string
    | Record of (string * t option) list * bool  (* true = more args *)
    | Opt of string * t
    | Impl of string * Mod.t

    | BinOp of t * string * t
    | SeqOp of string * t list
    | Suffix of t * Expr.quant * Expr.greed
    | Linear of t

end = Pat
