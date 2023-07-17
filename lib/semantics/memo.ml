open! Batteries
open Uref
open Tuple4

open System

open Unify
open Ulist
open Umode

module IM = Hashtbl.Make(struct
  include Hashtbl
  include Int
end)

type t = value IM.t * stack IM.t * costack IM.t * boolean IM.t

let mk_memo () = (
  IM.create 16,
  IM.create 16,
  IM.create 16, 
  IM.create 16
)

let rec freshen_value m v : value = match uget v with
  | Lit _ -> v
  | Con ((i, o, d), con) -> 
    uref @@ Con ((freshen_costack m i, freshen_costack m o, freshen_mode m d), con)
  | Var k -> 
    IM.find_option (first m) k |> Option.default_delayed @@ fun () -> 
      let nu = mk_var () in
      IM.add (first m) k nu;
      nu

and freshen_stack m s : stack = match uget s with
  | UCons (u, us) -> 
    uref @@ UCons (freshen_value m u, freshen_stack m us)
  | UNil -> s
  | USeq i -> 
    IM.find_option (second m) i |> Option.default_delayed @@ fun () -> 
      let nu = ufresh () in
      IM.add (second m) i nu;
      nu
    
and freshen_costack m s : costack = match uget s with
  | UCons (u, us) -> 
    uref @@ UCons (freshen_stack m u, freshen_costack m us)
  | UNil -> s
  | USeq i -> 
    IM.find_option (third m) i |> Option.default_delayed @@ fun () -> 
      let nu = ufresh () in
      IM.add (third m) i nu;
      nu

and freshen_mode m s = uget s |> freshen_mode_ m |> uref
and freshen_mode_ m b = b |> List.map @@ List.map begin fun r -> match !r with
  | BVar k -> 
    IM.find_option (fourth m) k |> Option.default_delayed @@ fun () -> 
      let nu = ref (BVar (unique ())) in
      IM.add (fourth m) k nu;
      nu
  | BExpr b -> ref @@ BExpr (freshen_mode_ m b)
  | _ -> r
end
