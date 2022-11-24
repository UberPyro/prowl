type dir = string list
type path = dir * string

(* Signature *)
type sign = 
  | SId of path
  | SCon of spec list

and spec = 
  | POpen of modl
  | PMix of sign


(* Module *)
and modl


(* Type *)
and ty


(* Expression *)
and expr


(* Pattern *)
and pat
