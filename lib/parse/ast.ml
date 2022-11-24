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

and args = (string * string) list
and args2 = args * args  (* first for implicits *)


(* Module *)
and modl = 
  | MId of string
  | MDir of dir * modl
  | MCat of modl list
  | MAbs of args2 * modl
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
  | DModl of args2 * string * sign option * modl
  | DRec of (args2 * string * sign * modl) list


(* Type *)
and ty = args * costack * costack

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
  | VMod of sign

and data = 
  | Record of (string * ty) list
  | Variant of (vl list * string) list


(* Expression *)
and expr


(* Pattern *)
and pat
