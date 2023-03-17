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

module Esc = struct

  (* let rec expr v ((e_, sp) : Hir1.expr) : expr = begin match e_ with
    | #Ast.literal | #Hir1.comb | #Ast.lexical_variable as e_ -> e_


    | _ -> failwith "Todo"
  end, sp *)

end
