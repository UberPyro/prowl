type dir = string list

(* Signature *)
type sign = 
  | SId of string
  | SDir of dir * sign
  | SCon of spec list

and spec = 
  | SOpen of modl
  | SMix of sign
  | SKind of int * string
  | STy of param list * string * vl
  | SData of param list * string * data
  | SSpec of string * ty
  | SSign of string * sign
  | SModl of string * sign

and param = 
  | RGen of string
  | RFun of int * int

and mod_args = (string * string) list
and mod_args2 = mod_args * mod_args  (* first for implicits *)


(* Module *)
and modl = 
  | MId of string
  | MDir of dir * modl
  | MCat of modl list
  | MAbs of mod_args2 * modl
  | MThunk of modl
  | MCon of defn list
  | MExpr of dir * string

and defn = 
  | DOpen of modl
  | DMix of modl
  | DKind of int * string
  | DTy of param list * string * vl
  | DData of param list * string * data
  | DDef of pat list * string * ty option * expr
  | DSign of string * sign
  | DModl of mod_args2 * string * sign option * modl
  | DRec of (mod_args2 * string * sign * modl) list


(* Type *)
and ty = mod_args * costack * costack

and costack = 
  | CImpl of stack list
  | CExpl of int * stack list

and stack = 
  | KImpl of vl list
  | KExpl of string * vl list

and vl = 
  | VId of string
  | VDir of dir * vl
  | VGen of string
  | VFun of ty
  | VList of ty
  | VMap of vl * ty
  | VMod of sign

and data = 
  | Record of (string * ty) list
  | Variant of (vl list * string) list


(* Expression *)
and expr = 
  | ECat of expr list
  | EBop of expr * string * expr
  | EUop of expr * string
  | ELet of args * string * ty option * expr * expr
  | ELetRec of ((args * string * ty option) * expr) list * expr
  | EAs of args * expr

  | EInt of int
  | EFloat of float
  | EChar of char
  | EString of string
  | EQuote of expr
  | EList of expr list
  | EMap of (expr * expr) list
  | EId of string
  | EDir of dir * expr

  | EVariant of string
  | ERecord of (string * expr) list
  | ECase of refclass * (args * expr) list
  | EAlt of expr list
  | EDrive of expr list
  | EModl of string * sign option

  | SoftSect of string
  | HardSect of string
  | SoftLeft of string * expr
  | HardLeft of string * expr
  | SoftRight of expr * string
  | HardRight of expr * string

and args = mod_args * pat list

and refclass = 
  | Irrefutable
  | Refutable
  | Silent


(* Pattern *)
and pat = 
  | PCat of pat list
  | PInt of int
  | PFloat of float
  | PChar of char
  | PString of string
  | PQuote of pat
  | PList of pat list
  | PMap of (expr * pat) list
  | PId of string
  | PDir of dir * pat

  | PVariant of string
  | PRecord of (string * pat) list
  | PModl of string * sign

  | PSnoc of pat * pat
  | PConj of pat * pat
  | PDisj of pat * pat
