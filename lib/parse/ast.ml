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
  | SSpec of string * ty
  | SSign of string * sign
  | SModl of (string * sign) list * string * sign

and param = 
  | RGen of string
  | RFun of int * int


(* Module *)
and modl = 
  | MId of string
  | MDir of dir * modl
  | MCat of modl list
  | MAbs of (string * sign) list * modl
  | MThunk of modl
  | MCon of defn list
  | MExpr of dir * string

and defn = 
  | DOpen of modl
  | DMix of modl
  | DKind of int * string
  | DTy of param list * string * vl
  | DDef of pat list * string * ty option * expr
  | DSign of string * sign
  | DModl of (string * sign) list * string * sign option * modl
  | DRec of ((string * sign) list * string * sign * modl) list


(* Type *)
and ty

and vl


(* Expression *)
and expr


(* Pattern *)
and pat
