open! Batteries
open Tuple4

open Syntax
open Ast
open System
open Uref

let link_costack m s = 
  Dict.find_option m s
  |> Option.default_delayed @@ fun () -> 
    let nu = Costack.ufresh () in
    Dict.add m s nu;
    nu

let link_stack m s = 
  Dict.find_option m s
  |> Option.default_delayed @@ fun () -> 
    let nu = Stack.ufresh () in
    Dict.add m s nu;
    nu

let link_var m s = 
  Dict.find_option m s
  |> Option.default_delayed @@ fun () -> 
    let nu = mk_var () in
    Dict.add m s nu;
    nu

let link_bvar m s = 
  Dict.find_option m s
  |> Option.default_delayed @@ fun () -> 
    let nu = ref @@ Det.BVar (unique ()) in
    Dict.add m s nu;
    nu

let rec fn_expr m = function
  | Explicit (c1, c2, d1, d2) -> 
    costack_expr m c1, costack_expr m c2, det_expr m d1, det_expr m d2
  | ImplicitCostack (z1, z2, d1, d2) -> 
    let c0 = Costack.ufresh () in
    List.map (stack_expr m) z1 |> Costack.ujoin c0, 
    List.map (stack_expr m) z2 |> Costack.ujoin c0, 
    det_expr m d1, det_expr m d2
  | ImplicitStack (w1, w2, d1, d2) -> 
    let s0 = Stack.ufresh () in
    let s1 = List.map (value_expr m) w1 |> Stack.ujoin s0 in
    let s2 = List.map (value_expr m) w2 |> Stack.ujoin s0 in
    let c0 = Costack.ufresh () in
    s1 @>> c0, s2 @>> c0, det_expr m d1, det_expr m d2

and costack_expr m (opt, z) = 
  List.map (stack_expr m) z
  |> Costack.ujoin @@ match opt with
    | None -> Costack.unil ()
    | Some name -> link_costack (first m) name

and stack_expr m (opt, w) = 
  List.map (value_expr m) w
  |> Stack.ujoin @@ match opt with
    | None -> Stack.unil ()
    | Some name -> link_stack (second m) name

and value_expr m = function
  | TyInt -> Value.usyn "int" []
  | TyString -> Value.usyn "string" []
  | TyQuote ty -> Value.usyn "quote" [Value.uatom @@ fn_expr m ty]
  | TyList ty -> Value.usyn "list" [Value.uatom @@ fn_expr m ty]
  | TyVal s -> link_var (third m) s

and det_expr m = _det_expr m %> Det.simp %> uref
and _det_expr m = function
  | DLit d -> uget d
  | DAnd (d1, d2) -> Det.mul_basic (_det_expr m d1) (_det_expr m d2)
  | DXor (d1, d2) -> _det_expr m d1 @ _det_expr m d2
  | DVar s -> [[link_bvar (fourth m) s]]


let ty_expr x = fn_expr Dict.(create 16, create 16, create 16, create 16) x
