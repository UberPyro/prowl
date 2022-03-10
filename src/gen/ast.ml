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
  and sp_t = [
    | `sp of string * ty
    | `ty of string * ty
    | `abst_ty of string m
    | `data of string * data
  ]

  (* types *)
  and ty = (constr list * ty_body) m    
  and constr = (string * string list) m 

  and ty_body = ty_body_t m 
  and ty_body_t = [
    | `id of string
    | `sq of ty_body list
    | `access of ty_body * accop * ty_body
    | `fn of ty_body * ty_body
    | `arr of ty_body list
    | `quoted of ty_body
    | `unit
    | `metatype of string
    | `sig_ of sp list
    | `with_ of ty
  ] 

  and data = data_t m 
  and data_t = [
    | `sum of (ty * string) list
    | `prod of (string * ty) list
    | `rows of (string * ty) list
  ] 

  and accop = accop_t m 
  and accop_t = [
    | `access
    | `poly_access
    | `macro
  ] 

  (* statements *)
  type s = s_t m 
  and s_t = [
    | `fn of mods_st * p * e
    | `val_ of mods_st * p * e
    | `open_ of e
    | `mix of mods_class_st * e
    | `ty of mods_ty * string * data
    | `abst_ty of string
    | `data of mods_ty * string * data
    | `begin_ of s list
  ] 

  (* expressions *)
  and e = e_t m 
  and e_t = [
    | `sq of e list
    | `id of string
    | `let_ of s
    | `to_ of ty
    | `as_ of p
    | `with_ of ty
    | `rec_upd of (string * e) list
    | `metatype of string
    | `mod_ of s list
    | `named of named_arg

    | `sum of string
    | `atom of string
    | `prod of (string * e) list
    | `rows of (string * e) list
    | `tup of e list
    | `str of string
    | `int of int
    | `flo of float
    | `char of char
    | `unit

    | `arr of e list
    | `fun_ of (p * e) list
    | `quoted of e

    | `access of e * accop * e
    | `bop of e * bop * e
    | `sect of bop
    | `sect_left of bop * e
    | `sect_right of e * bop
  ] 

  and bop = string m 

  and p = p_t m 
  and p_t = [
    | `id of string
    | `wildcard
    | `sq of p list
    | `named of named_arg
    | `int of int
    | `flo of float
    | `char of char
    | `str of string
    | `unit
    
    | `sum of string
    | `atom of string
    | `prod of (string * p) list
    | `rows of (string * p) list
    | `tup of p list

    | `arr of p list
    | `dict of (e * p) list
    | `quoted of p
    
    | `bop of p * p_bop * p
  ] 
  
  and p_bop = string m 

  and named_arg = (string * e) m 

  type t = [
    | `stmt of s
    | `expr of e
  ] 

end
