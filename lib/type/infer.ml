open! Batteries

open Types
module Code = Parse.Ast.Make(struct
  type t = Metadata.Span.t * dc * dc [@@deriving show]
end)

(* let rec infer =  *)
