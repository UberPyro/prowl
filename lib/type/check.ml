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

let new_costack () = 
  uref @@ Costack.Bot (Costack.fresh ())

let new_stack () = 
  uref @@ Stack.Bot (Stack.fresh ())

let push_stack s c = 
  uref @@ Costack.Seq (s, c)

let push_var v s = 
  uref @@ Stack.Seq (v, s)

let new_distack () = 
  push_stack (new_stack ()) (new_costack ())

let new_fn () = 
  new_costack (), new_costack ()

let new_endo () = 
  let c = new_costack () in
  c, c

let new_stack_endo () = 
  let c = new_distack () in
  c, c

let new_lit v = 
  let c1 = new_costack () in
  let s1 = new_stack () in
  let s2 = push_var v s1 in
  push_stack s1 c1, push_stack s2 c1
