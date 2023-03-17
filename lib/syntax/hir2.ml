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

  let (let+) t f = Tuple3.map3 f t

  let rec expr_outer (r : t) : t = 
    let unbound, _, ((_, sp) as e) = expr_inner r in
    let+ _ = r in (`bind_uvar (List.of_enum (Vocab.keys unbound), e), sp)
  
  and expr_inner (unbound, bound, (((e_, sp) : Hir1.expr) as e) as r) : t = 
    match e_ with
    | #Ast.literal | #Hir1.comb | #Ast.lexical_variable -> r
    | `var v | `stack v | `costack v -> 
      begin unbound |> if Vocab.mem v bound then Fun.id
      else Vocab.add v sp end, bound, e
    | `jux es -> 
    let+ x = List.fold_left begin fun (u, b, es') e' -> 
      let+ x = expr_inner (u, b, e') in x :: es'
    end (unbound, bound, []) es in `jux (List.rev x), sp
    | `dag e' -> let+ x = expr_inner (unbound, bound, e') in `dag x, sp
    | `prime e' -> let+ x = expr_inner (unbound, bound, e') in `prime x, sp
    | `quote e' -> let+ x = expr_outer (unbound, bound, e') in `quote x, sp
    | `block (e', d1, d2) -> 
      let+ x = expr_outer (unbound, bound, e') in `block (x, d1, d2), sp
    | `bind_var (bs, e_) -> 
      let bs' = List.fold_left begin fun (u, b, bs') (s, e') -> 
        let+ x = expr_outer (u, b, e') in (s, x) :: bs'
      end (unbound, bound, []) bs |> Tuple3.third |> List.rev in
      let+ x = expr_inner (unbound, bound, e_) in (`bind_var (bs', x), sp)
    | `bind_uvar (vs, e') -> 
      let b = List.fold_left (fun vc v -> Vocab.add v sp vc) bound vs in
      let+ x = expr_inner (unbound, b, e') in `bind_uvar (vs, x), sp
  
  let expr e = expr_outer Vocab.(empty, empty, e)
  
end

(* Consider binder insertion first *)
module Esc = struct

  (* let rec expr v ((e_, sp) : Hir1.expr) : expr = begin match e_ with
    | #Ast.literal | #Hir1.comb | #Ast.lexical_variable as e_ -> e_


    | _ -> failwith "Todo"
  end, sp *)

end
