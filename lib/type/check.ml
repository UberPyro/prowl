open Batteries
open Uref

open Ast
open Type

type 'a typed_content = <
  ast : 'a;
  loc : loc;
  ty : t * t;
>

let pp_typed_content = pp_content

module TypedAst = Wrap(struct
  type 'a t = 'a typed_content [@@deriving show]
end)
