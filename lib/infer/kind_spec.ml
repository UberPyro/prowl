open! Batteries

open Syntax
open Meta
open Type

let rec derive ((k_, _) : Ast.kind) : Kind.t = match k_ with
  | `var -> Kind.lit Kind.KStar
  | `seq -> Kind.start ()
  | `jux [] -> Kind.connected ()
  | `jux (h :: t) -> 
    let i = derive h, snd h in
    let rec connect_pairs (h_, sp_h as h) = function
      | (i_, sp_i as i) :: t -> 
        begin try Kind.connect h_ i_ with
        | Kind.DifferentlyKinded (s1, s2) -> raise @@ Error.TypeError (
            Span.join sp_h sp_i, 
            Printf.sprintf "Cannot unify kinds [%s] and [%s]" s1 s2
          )
        end;
        connect_pairs i t
      | [] -> h in
    let o = connect_pairs i (List.map (fun x -> derive x, snd x) t) in
    fst (fst i), snd (fst o)
  | `dag k -> derive k |> Tuple2.swap
