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

  let ins v e : Hir1.expr = `bind_uvar ([v], e), snd e

  (* Can't be used as-is -- has to be woven into `expr`. *)
  let scan = 
    let rec go vc_unbound vc_bound ((e_, sp) : Hir1.expr) = match e_ with
      | #Ast.literal | #Hir1.comb | #Ast.lexical_variable -> vc_unbound, vc_bound
      | `var v | `stack v | `costack v -> 
        begin vc_unbound |> if Vocab.mem v vc_bound then Fun.id
        else Vocab.add v sp end, vc_bound
      | `jux es -> List.fold_left (fun (u, b) -> go u b) (vc_unbound, vc_bound) es
      | `dag e -> go vc_unbound vc_bound e
      | `prime e -> go vc_unbound vc_bound e
      
      | `quote _ | `block _ -> vc_unbound, vc_bound

      | `bind_var (_, e) -> go vc_unbound vc_bound e
      | `bind_uvar (vs, e) -> 
        go vc_unbound (List.fold_left (fun vc v -> Vocab.add v sp vc) vc_bound vs) e
    in go Vocab.empty Vocab.empty %> fst

  (* let rec expr vc ((e_, sp) : Hir1.expr) : Hir1.expr = begin match e_ with
    | 
  end *)

end

(* Consider binder insertion first *)
module Esc = struct

  (* let rec expr v ((e_, sp) : Hir1.expr) : expr = begin match e_ with
    | #Ast.literal | #Hir1.comb | #Ast.lexical_variable as e_ -> e_


    | _ -> failwith "Todo"
  end, sp *)

end
