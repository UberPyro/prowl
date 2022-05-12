open Batteries
module Dict = Map.Make(struct type t = string let compare = compare end)
open! Dict.Infix

open Ast

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
  | VBin of int * e list * int
  | VCapture of e
                          (* is impl *)
  | VMod of ty_val Dict.t * (bool * ty_val) Dict.t

type st = {
  tyctx: ty_val Dict.t;
  ctx: e_val Dict.t;
  stk: e_val list;
}

let string_of_v = function
  | VInt i -> string_of_int i
  | VStr s -> s
  
  | _ -> failwith "Unimplemented"

let null_st = {tyctx=Dict.empty; ctx=Dict.empty; stk=[]}
let lit st v = Some {st with stk = v :: st.stk}

let rec program st (_, expr) = e st expr

and e st (expr, _) = match expr with
  | Int i -> lit st (VInt i)
  | Str s -> lit st (VStr s)
  | Bin (i1, v, i2) -> lit st (VBin (i1, v, i2))
  | Capture ast -> lit st (VCapture ast)
  
  | _ -> failwith "Unimplemented"
