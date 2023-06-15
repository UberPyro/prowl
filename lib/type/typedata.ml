open! Batteries
open Uref

let pp_uref fmt x y = fmt x (uget y)

type 'a type_seq = 'a _type_seq uref
and 'a _type_seq = 
  | Bottom
  | Push of 'a type_seq * 'a
  | Concat of 'a type_seq * int
  [@@deriving show]

type type_value = _type_value uref
and _type_value = 
  | DLit of dlit
  | DCon of dcon * dlink
  | DVar of int

and dlit = 
  | DInt
  | DString

and dcon = 
  | DQuote
  | DList

and type_stack = type_value type_seq
and type_costack = type_stack type_seq
and dlink = {
  arg_wing : type_costack;
  low_wing : type_costack;
  res_wing : type_costack;
} [@@deriving show]

let mk_var () = uref @@ DVar (unique ())
let mk_seq () = uref @@ Concat (uref Bottom, unique ())
let push s x = uref @@ Push (s, x)
let concat s str = uref @@ Concat (s, str)

let mk_unit u = uref @@ Concat (uref Bottom, u)

let mk_unital_costack () = 
  uref @@ Push (uref @@ Concat (uref Bottom, unique ()), mk_seq ())
