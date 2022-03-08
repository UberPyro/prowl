open Batteries
open Sexplib.Conv

module type Meta = sig
  type 'a m [@@deriving sexp]
end

module T(M : Meta) = struct
  open M

  type access_st = [`pub]               [@@deriving sexp]
  type access_ty = [`opaq | access_st]  [@@deriving sexp]
  type class_ty = [`use]                [@@deriving sexp]
  type class_st = [`inst | class_ty]    [@@deriving sexp]

  type mods_st = [access_st | class_st] list            [@@deriving sexp]
  type mods_ty = [`new_ty | access_ty | class_ty] list  [@@deriving sexp]
  type mods_class_st = [`Inst] option                   [@@deriving sexp]

  (* specs *)
  type sp = sp_t m [@@deriving sexp]
  and sp_t = [
    | `sp of string * ty
    | `ty of string * ty
    | `abst_ty of string m
    | `data of string * data
  ] [@@deriving sexp]

  (* types *)
  and ty = (constr list * ty_body) m    [@@deriving sexp]
  and constr = (string * string list) m [@@deriving sexp]

  and ty_body = ty_body_t m [@@deriving sexp]
  and ty_body_t = [
    | `id of string
    | `sq of ty_body list
    | `fn of ty_body * ty_body
    | `arr of ty_body list
    | `quoted of ty_body
    | `unit
    | `metatype of string
    | `sig_ of sp list
    | `with_ of ty
  ] [@@deriving sexp]

  and data = data_t m [@@deriving sexp]
  and data_t = [
    | `sum of (ty * string) list
    | `prod of (string * ty) list
    | `rows of (string * ty) list
  ] [@@deriving sexp]

  (* statements *)
  type s = s_t m [@@deriving sexp]
  and s_t = [
    | `fn of mods_st * p * e
    | `val_ of mods_st * p * e
    | `open_ of e
    | `mix of mods_class_st * e
    | `ty of mods_ty * string * data
    | `abst_ty of string
    | `data of mods_ty * string * data
    | `begin_ of s list
  ] [@@deriving sexp]

  (* expressions *)
  and e = e_t m [@@deriving sexp]
  and e_t = [
    | `sq of e list
    | `id of string
    | `access of string * e
    | `poly_access of string * e
    | `macro of string * e
    | `let_ of s
    | `to_ of ty
    | `as_ of p
    | `with_ of ty
    | `metatype of string
    | `mod_ of s list
    | `named of named_arg

    | `sum of string
    | `atom of string
    | `prod of (string * e) list
    | `rows of (string * e) list
    | `tup of e list
    | `str_lit of string
    | `int_lit of int
    | `flo_lit of float
    | `char_lit of char
    | `unit_lit

    | `arr_lit of e list
    | `fun_lit of (p * e) list
    | `quoted of e

    | `bop of e * bop * e
    | `sect of bop
    | `sect_left of bop * e
    | `sect_right of e * bop
  ] [@@deriving sexp]

  and bop = bop_t m [@@deriving sexp]
  and bop_t = [
    | `add | `sub | `mul | `div
    | `exp | `cons | `snoc | `append
    | `range | `eq | `neq | `cmp
  ] [@@deriving sexp]

  and p = p_t m [@@deriving sexp]
  and p_t = [
    | `id of string
    | `wildcard
    | `sq of p list
    | `named of named_arg
    | `int_pat of int
    | `flo_pat of float
    | `char_pat of char
    | `str_pat of string
    | `unit_pat
    
    | `sum of string
    | `atom of string
    | `prod of (string * p) list
    | `rows of (string * p) list
    | `tup of p list

    | `arr of p list
    | `dict of (e * p) list
    | `quoted of p
    
    | `bop of p * p_bop * p
  ] [@@deriving sexp]
  
  and p_bop = p_bop_t m [@@deriving sexp]
  and p_bop_t = [
    | `cons
    | `snoc
    | `alt
  ] [@@deriving sexp]

  and named_arg = (string * e) m [@@deriving sexp]

  type t = [
    | `stmt of s
    | `expr of e
  ] [@@deriving sexp]

end
