type access_mod = Pub | Opaq

type sp = 
  | SDef of string * ty
  | STy of string * (string list * ty) option

and ty = constr list * ty_eff
and constr = string * string list
and ty_eff = ty_term * ty_term
and ty_term = 
  | TId of string
  | TGen of string
  | TAccess of ty_term * string
  | TCat of ty_term list
  | TCapture of ty_eff
  | TList of ty_eff
  | TMap of ty_term * ty_eff
  | TUnit
  | TVoid
  | TBin of ty_eff list list
  | TData of (ty_term list * string) list list
  | TSig of sp list
  | TMod of sp list

and s = 
  | Def of access_mod * [`def | `impl] * p * e * ty option
  | Open of e
  | Use of e
  | Mix of [`impl] option * e
  | Ty of access_mod * [`name | `alias | `class_]
    * string * (string list * ty) option

and greed = Gre | Rel | Cut
and quant = 
  | Opt
  | Plus
  | Star
  | Num of e
  | Min of e
  | Max of e
  | Range of e * e

and stack_comb = 
  | Dup of int
  | Zap of int
  | Rot of int
  | Run of int

and e = 
  | Id of string
  | Access of e * string
  | Get of e * e
  (* Todo: Data & Tuple access *)

  | Int of int
  | Flo of float
  | Char of char
  | Str of string
  | Unit

  | List of e list
  | Map of (e * e) list
  | Bin of int * e list * int
  | Data of string
  | Prod of e list
  | Mod of s list
  | Capture of e

  | Sect of string
  | SectLeft of string * e
  | SectRight of e * string

  | Cat of e list
  | Sym of string
  | Bop of e * string * e
  | StackComb of stack_comb list

  | Let of (string * p * e) list * e
  | As of string * p * e

  | Quant of e * quant * greed
  | Case of e * (greed * e) list
  | Inv of e list
  | Span of e * e

  | Noncap of e
  | Cap of e
  | Atomic of e

and p = 
  | PId of string
  | PAccess of p * string
  | PBlank
  | PCat of p list
  | PAsc of p * ty
  | POpen
  | PUse

  | PInt of int
  | PFlo of float
  | PStr of string
  | PChar of char
  | PUnit

  | PList of p list
  | PMap of (e * p) list
  | PBin of int * p list * int
  | PData of string
  | PProd of p list
  | PCapture of p

  | PBop of p * string * p
  | PSym of string

and program = access_mod * e
