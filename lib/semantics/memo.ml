open! Batteries
open Uref

open System

open Util
open Ull

module IM = Hashtbl.Make(struct
  include Hashtbl
  include Int
end)

type t = value IM.t * stack IM.t * costack IM.t

let mk_memo () = (
  IM.create 16,
  IM.create 16,
  IM.create 16
)

let rec freshen_value ((vm, _, _) as memo) v : value = match uget v with
  | Lit _ -> v
  | Con ((i, o), con) -> 
    uref @@ Con (Tuple2.mapn (freshen_costack memo) (i, o), con)
  | Var k | AnnotVar k -> 
    IM.find_option vm k |> Option.default_delayed @@ fun () -> 
      let nu = mk_var () in
      IM.add vm k nu;
      nu

and freshen_stack ((_, sm, _) as memo) s : stack = match uget s with
  | UCons (u, us) -> 
    uref @@ UCons (freshen_value memo u, freshen_stack memo us)
  | UNil -> s
  | USeq i -> 
    IM.find_option sm i |> Option.default_delayed @@ fun () -> 
      let nu = ufresh () in
      IM.add sm i nu;
      nu
    
and freshen_costack ((_, _, cm) as memo) s : costack = match uget s with
  | UCons (u, us) -> 
    uref @@ UCons (freshen_stack memo u, freshen_costack memo us)
  | UNil -> s
  | USeq i -> 
    IM.find_option cm i |> Option.default_delayed @@ fun () -> 
      let nu = ufresh () in
      IM.add cm i nu;
      nu
