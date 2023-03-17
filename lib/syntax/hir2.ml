(* Escape analysis and binder insertion *)
open! Batteries

open Meta

type expr = _expr * Span.t [@@deriving show]
and _expr = [
  | Ast.variable
  | Ast.literal
  | Hir1.comb

  | `jux of expr list
  | `dag of expr
  | `prime of expr
  | `quote of expr
  | `block of expr * Ast.monodet * Ast.monodet
  | `bind_uvar of Var.t list * expr

  | `bind_var of int * (string * expr) list * expr
] [@@deriving show]

module Vocab = Map.Make(Var)

module Ins = struct

  type t = Span.t Vocab.t * Span.t Vocab.t * Hir1.expr

  let ins v e : Hir1.expr = `bind_uvar ([v], e), snd e

  let rec expr_outer (r : t) : t = 
    let unbound, bound, e = expr_inner r in
    unbound, bound, Vocab.fold (fun k _ a -> ins k a) unbound e
  
  and expr_inner (unbound, bound, (((e_, sp) : Hir1.expr) as e) as r) : t = 
    match e_ with
    | #Ast.literal | #Hir1.comb | #Ast.lexical_variable -> r
    | `var v | `stack v | `costack v -> 
      begin unbound |> if Vocab.mem v bound then Fun.id
      else Vocab.add v sp end, bound, e
    | `jux es -> List.fold_left begin fun (u, b, es') e' -> 
      let u', b', e'' = expr_inner (u, b, e') in
      u', b', e'' :: es'
    end (unbound, bound, []) es
    |> fun (u, b, es') -> u, b, (`jux (List.rev es'), sp)
    | `dag e' -> 
      let u', b', e'' = expr_inner (unbound, bound, e') in
      u', b', (`dag e'', sp)
    | `prime e' -> 
      let u', b', e'' = expr_inner (unbound, bound, e') in
      u', b', (`prime e'', sp)
    | `quote e' -> 
      let u', b', e'' = expr_outer (unbound, bound, e') in
      u', b', (`quote e'', sp)
    | `block (e', d1, d2) -> 
      let u', b', e'' = expr_outer (unbound, bound, e') in
      u', b', (`block (e'', d1, d2), sp)
    | `bind_var (bs, e_) -> 
      let u', b', e'' = expr_inner (unbound, bound, e_) in
      let bs' = List.fold_left begin fun (u, b, bs') (s, e') -> 
        let u', b', e'' = expr_outer (u, b, e') in
        u', b', (s, e'') :: bs'
      end (unbound, bound, []) bs |> Tuple3.third |> List.rev in
      u', b', (`bind_var (bs', e''), sp)
    | `bind_uvar (vs, e') -> 
      let u', b', e'' = expr_inner begin 
        unbound,
        List.fold_left (fun vc v -> Vocab.add v sp vc) bound vs,
        e'
      end in
      u', b', (`bind_uvar (vs, e''), sp)
  
  let expr e = expr_outer Vocab.(empty, empty, e)
  
end

(* Consider binder insertion first *)
module Esc = struct

  (* let rec expr v ((e_, sp) : Hir1.expr) : expr = begin match e_ with
    | #Ast.literal | #Hir1.comb | #Ast.lexical_variable as e_ -> e_


    | _ -> failwith "Todo"
  end, sp *)

end
