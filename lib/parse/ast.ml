type modty = 
  | ISig of spec list
  | IId of string list

and spec = 
  | SKind of int * string
  | STy of param * string * ty
  | SData of param * string * data
  | SSpec of string * ty
  | SOpen of modexpr
  | SMix of modty

and param = 
  | PGen of string
  | PSeq of int * int

and ty = costack * costack
and 'a seqty = string option * 'a list
and costack = stack seqty
and stack = var seqty
and var = 
  | TVar of string
  | TGen of string
  | TQuote of ty
  | TList of ty

and data = 
  | Variant of case list
  | Record of record

and case = 
  | VSeq of costack
  | VTag of record

and record = (string * ty) list

and modexpr


(* type mod_ty = 
  | Sig of sig_body
  | SigID of string list

and sig_body = spec list
and spec = 
  | Kind of int * string
  | Spec of string * ty
  | Openspec of mod_expr
  | Mixsig of mod_ty


and ty

and mod_expr *)
(* type mod_expr = 
  | Body of mod_body
  | MCat of mod_expr list
  | MThunk of mod_expr
  (* | MAs *)
  
and mod_body = 
  |  *)
