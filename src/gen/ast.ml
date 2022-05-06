type access_mod = Pub | Opaq

type sp = 
  | SDef of string * ty
  | STy of string * (string list * ty) option

and ty = constr list * ty_eff
and constr = string * string list
and ty_eff = ty_term list * ty_term list
and ty_term = 
  | TId of string
  | TGen of string
  | TAccess of ty_term * ty_term
  | TCapture of ty_eff
  | TList of ty_eff
  | TMap of ty_term * ty_eff
  | TUnit
  | TVoid
  | TBin of ty_eff list list
  | TData of (ty_term * string) list list
  | TSig of sp list
  | TMod of sp list


