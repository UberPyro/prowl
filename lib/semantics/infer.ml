open! Batteries
open Syntax
open System
open Ast.Make(struct type t = fn[@@deriving show] end)

(* let infer ctx uctx (ast, _sp, (i0, o0)) = match ast with
  | Bop ((_, _, (i1, o0) as left), Aop _, (_, _, (i2, o2) as right)) ->  *)
