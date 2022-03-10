open Sexplib.Conv

module type Meta = sig
  type 'a m [@@deriving sexp]
end

module T(M : Meta) = struct
  open M

  type access_st = [`pub] [@@deriving sexp]
  type access_ty = [`opaq | access_st] [@@deriving sexp]
  type class_ty = [`use] [@@deriving sexp]
  type class_st = [`inst | class_ty] [@@deriving sexp]

  type mods_st = [access_st | class_st] list [@@deriving sexp]
  type mods_ty = [`new_ty | access_ty | class_ty] list [@@deriving sexp]
  type mods_class_st = [`inst] list [@@deriving sexp]

  (* specs *)
  type sp = sp_t m [@@deriving sexp]
  and sp_t = 
    | SPSp of string * ty
    | SPTy of string * ty
    | SPAbst_ty of string m
    | SPData of string * data
    [@@deriving sexp]

  (* types *)
  and ty = (constr list * ty_body) m     [@@deriving sexp]
  and constr = (string * string list) m  [@@deriving sexp]

  and ty_body = ty_body_t m  [@@deriving sexp]
  and ty_body_t = 
    | TId of string
    | TSq of ty_body list
    | TAccess of ty_body * accop * ty_body
    | TFn of ty_body * ty_body
    | TArr of ty_body list
    | TQuoted of ty_body
    | TUnit
    | TMetatype of string
    | TSig of sp list
    | TWith of ty
    [@@deriving sexp]

  and data = data_t m  [@@deriving sexp]
  and data_t = 
    | DSum of (ty * string) list
    | DProd of (string * ty) list
    | DRows of (string * ty) list
    [@@deriving sexp]

  and accop = [
    | `access
    | `poly_access
    | `macro
  ] [@@deriving sexp]

  (* statements *)
  type s = s_t m [@@deriving sexp]
  and s_t = 
    | Fn of mods_st * p * e
    | Val of mods_st * p * e
    | Open of e
    | Use of e
    | Mix of mods_class_st * e
    | Ty of mods_ty * string * ty
    | Abst_ty of string
    | Data of mods_ty * string * data
    | Begin of s list
    [@@deriving sexp]

  (* expressions *)
  and e = e_t m [@@deriving sexp]
  and e_t = 
    | Sq of e list
    | Id of string
    | Let of s
    | To of e
    | As of p
    | With of ty
    | Rec_upd of (string * e) list
    | Metatype of string
    | Mod of s list
    | Named of named_arg

    | Sum of string
    | Atom of string
    | Prod of (string * e) list
    | Rows of (string * e) list
    | Tup of e list
    | Str of string
    | Int of int
    | Flo of float
    | Char of char
    | Unit

    | Arr of e list
    | Fun of (p * e) list
    | Quoted of e

    | Access of e * accop * e
    | Bop of e * bop * e
    | Sect of bop
    | Sect_left of bop * e
    | Sect_right of e * bop
    [@@deriving sexp]

  and bop = string m [@@deriving sexp]

  and p = p_t m [@@deriving sexp]
  and p_t = 
    | PId of string
    | PAs of p
    | PWildcard
    | PSq of p list
    | PNamed of named_arg
    | PInt of int
    | PFlo of float
    | PChar of char
    | PStr of string
    | PUnit
    
    | PSum of string
    | PAtom of string
    | PProd of (string * p) list
    | PRows of (string * p) list
    | PTup of p list

    | PArr of p list
    | PDict of (e * p) list
    | PQuoted of p
    
    | PBop of p * p_bop * p
    | PAccess of p * accop * p
    [@@deriving sexp]
  
  and p_bop = string m [@@deriving sexp]

  and named_arg = (string * e) m [@@deriving sexp]

  type t = 
    | S of s
    | E of e
    [@@deriving sexp]

end
