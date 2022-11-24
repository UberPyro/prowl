type dir = string list
type path = dir * string

(* Signature *)
type sign = 
  | SId of path
  | SCon of spec list

and spec = 
  | SOpen of modl
  | SMix of sign
  | SKind of string list * string
  | STy of param list * string * vl
  | SSpec of string * ty

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

and defn = 
  | 

and recr = Rec | Seq


(* Type *)
and ty

and vl


(* Expression *)
and expr


(* Pattern *)
and pat
