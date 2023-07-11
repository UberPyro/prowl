open! Batteries
open Uref
open Tuple3

open Syntax
open Ast
open System
open Util
open Ull

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

let rec fn_expr m = function
  | Explicit (c1, c2) -> costack_expr m c1, costack_expr m c2
  | ImplicitCostack (z1, z2) -> 
    let c0 = ufresh () in
    List.map (stack_expr m) z1 |> ujoin c0, 
    List.map (stack_expr m) z2 |> ujoin c0
  | ImplicitStack (w1, w2) -> 
    let s0 = ufresh () in
    let s1 = List.map (value_expr m) w1 |> ujoin s0 in
    let s2 = List.map (value_expr m) w2 |> ujoin s0 in
    let c0 = ufresh () in
    s1 @>> c0, s2 @>> c0

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

let ty_expr = fn_expr Dict.(create 16, create 16, create 16)
