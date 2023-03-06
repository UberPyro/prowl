open! Batteries

open Syntax
open Type

let rec derive ((k_, sp) : Ast.kind) : Kind.t = match k_ with
  | `var -> Kind.lit Kind.KStar
  | `seq -> Kind.start ()
  | `jux [] -> Kind.connected ()
  | `jux (h :: t) -> 
    let i = derive h, snd h in
    let rec connect_pairs (h_, sp_h as h) = function
      | (i_, sp_i as i) :: t -> 
        Kind.connect h_ i_;
        connect_pairs i t
      | [] -> h in
    let o = connect_pairs i (List.map (fun x -> derive x, snd x) t) in
    fst (fst i), snd (fst o)
  | `dag k -> derive k |> Tuple2.swap
