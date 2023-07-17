open! Batteries
open Uref
open Tuple4

open Syntax
open Ast
open System
open Unify
open Ulist
open Umode

let link m s = 
  Dict.find_option m s
  |> Option.default_delayed @@ fun () -> 
    let nu = ufresh () in
    Dict.add m s nu;
    nu

let link_var m s = 
  Dict.find_option m s
  |> Option.default_delayed @@ fun () -> 
    let nu = mk_var () in
    Dict.add m s nu;
    nu

let link_mode m b = 
  Dict.find_option m b
  |> Option.default_delayed @@ fun () -> 
    let nu = bfresh_ () in
    Dict.add m b nu;
    nu

let rec mode_expr m = _mode_expr m %> simp %> uref
and _mode_expr m = function
  | None -> bfresh_ ()  (* lax generality *)
  | Some m_ -> match m_ with
    | BLit (d1, d2) -> [[ref @@ BConst (det_expr d1, det_expr d2)]]
    | BAnd (b1, b2) -> mul_basic (_mode_expr m b1) (_mode_expr m b2)
    | BXor (b1, b2) -> _mode_expr m b1 @ _mode_expr m b2
    | BVar s -> link_mode (fourth m) s

and det_expr = function
  | Fn -> true, true
  | Pt -> false, true
  | Mt -> true, false
  | Rl -> false, false

let rec fn_expr m = function
  | Explicit (c1, c2, b) -> costack_expr m c1, costack_expr m c2, mode_expr m b
  | ImplicitCostack (z1, z2, b) -> 
    let c0 = ufresh () in
    List.map (stack_expr m) z1 |> ujoin c0, 
    List.map (stack_expr m) z2 |> ujoin c0, 
    mode_expr m b
  | ImplicitStack (w1, w2, b) -> 
    let s0 = ufresh () in
    let s1 = List.map (value_expr m) w1 |> ujoin s0 in
    let s2 = List.map (value_expr m) w2 |> ujoin s0 in
    let c0 = ufresh () in
    s1 @>> c0, s2 @>> c0, mode_expr m b

and costack_expr m (opt, z) = 
  List.map (stack_expr m) z
  |> ujoin @@ match opt with
    | None -> unil ()
    | Some name -> link (first m) name

and stack_expr m (opt, w) = 
  List.map (value_expr m) w
  |> ujoin @@ match opt with
    | None -> unil ()
    | Some name -> link (second m) name

and value_expr m = function
  | TyInt -> uref @@ Lit Int
  | TyString -> uref @@ Lit String
  | TyQuote ty -> uref @@ Con (fn_expr m ty, Quote)
  | TyList ty -> uref @@ Con (fn_expr m ty, List)
  | TyVal s -> link_var (third m) s

let ty_expr x = fn_expr Dict.(create 16, create 16, create 16, create 16) x
