type mod_ty = 
  | Sig of sig_body
  | SigID of string list

and sig_body = spec list
and spec = 
  | Kind of int * string
  | Spec of string * ty
  | Openspec of mod_expr
  | Mixsig of mod_ty


and ty

and mod_expr
(* type mod_expr = 
  | Body of mod_body
  | MCat of mod_expr list
  | MThunk of mod_expr
  (* | MAs *)
  
and mod_body = 
  |  *)
