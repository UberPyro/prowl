open Batteries
module Dict = Map.Make(struct type t = string let compare = compare end)
open Dict.Infix

open Ast
open Util

type ty_val = 
  | YInt
  | YStr
  | YBin of ty_val list list
  | YCapture of ty_eff
         (* is class?     type ctx *)  (* data ctx *)
  | YSig of (bool * ty_val) Dict.t * ty_val Dict.t

type e_val = 
  | VInt of int
  | VStr of string
  | EBin of (e_val, e_val) Either.t
  | ECapture of e
                          (* is impl *)
  | EMod of ty_val Dict.t * (bool * ty_val) Dict.t

(* type st = {
  tyctx: 
} *)
