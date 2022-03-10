module type Meta = sig
  type 'a m
end

module T(M : Meta) = struct
  open M

  type access_st = [`pub]
  type access_ty = [`opaq | access_st]
  type class_ty = [`use]
  type class_st = [`inst | class_ty]

  type mods_st = [access_st | class_st] list
  type mods_ty = [`new_ty | access_ty | class_ty] list
  type mods_class_st = [`Inst] list

  (* specs *)
  type sp = sp_t m
  and sp_t = 
    | SPSp of string * ty
    | SPTy of string * ty
    | SPAbst_ty of string m
    | SPData of string * data

  (* types *)
  and ty = (constr list * ty_body) m    
  and constr = (string * string list) m 

  and ty_body = ty_body_t m 
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
  

  and data = data_t m 
  and data_t = 
    | DSum of (ty * string) list
    | DProd of (string * ty) list
    | DRows of (string * ty) list
  

  and accop = accop_t m 
  and accop_t = [
    | `access
    | `poly_access
    | `macro
  ] 

  (* statements *)
  type s = s_t m 
  and s_t = 
    | Fn of mods_st * p * e
    | Val of mods_st * p * e
    | Open of e
    | Mix of mods_class_st * e
    | Ty of mods_ty * string * data
    | Abst_ty of string
    | Data of mods_ty * string * data
    | Begin of s list
  

  (* expressions *)
  and e = e_t m 
  and e_t = 
    | Sq of e list
    | Id of string
    | Let of s
    | To of ty
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

  and bop = string m 

  and p = p_t m 
  and p_t = 
    | PId of string
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
  
  
  and p_bop = string m 

  and named_arg = (string * e) m 

  type t = 
    | S of s
    | E of e
  

end
