open Batteries
module Dict = Map.Make(struct type t = string let compare = compare end)
open! Dict.Infix

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
  | VEith of (e_val, e_val) Either.t
  | VPair of e_val * e_val
  | VCapture of e
                          (* is impl *)
  | VMod of ty_val Dict.t * (bool * ty_val) Dict.t

type st = {
  tyctx: ty_val Dict.t;
  ctx: e_val Dict.t;
  stk: e_val list;
}

let null_st = {tyctx=Dict.empty; ctx=Dict.empty; stk=[]}
let lit st v = Some {st with stk = v :: st.stk}

let rec eith_of_ints i1 v i2 = match i1, i2 with
  | 0, 0 -> v
  | n, 0 -> eith_of_ints (n - 1) (VEith (Either.R v)) 0
  | n, m -> eith_of_ints n (VEith (Either.L v)) (m - 1)

let rec pair_of_lst = function
  | h1 :: h2 :: t -> pair_of_lst (VPair (h1, h2) :: t)
  | h :: [] -> h
  | [] -> failwith "Monoples are banned"

let rec program st (_, expr) = e st expr

and e st (expr, _) = match expr with
  | Int i -> lit st (VInt i)
  | Str s -> lit st (VStr s)
  | Bin (i1, [o], i2) -> 
    Option.bind (e st o) (fun v -> lit st (eith_of_ints i1 v i2))
    
  | Bin (i1, lst, i2) -> 
    let prod = List.map (e st) lst |> pair_of_lst in
    lit st (eith_of_ints i1 prod i2)
  (* | Capture ast -> VCapture ast *)
  
  | _ -> failwith "Todo"
