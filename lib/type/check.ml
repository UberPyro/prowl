open Batteries
open Uref

open Ast
open Type

module Dict = Map.Make(struct
  type t = string
  let compare = compare
end)

type 'a typed_content = <
  ast : 'a;
  loc : loc;
  ty : t * t;
>

let ascribe f ty obj = object
  method ast = f obj#ast
  method loc = obj#loc
  method ty = ty
end

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
  T (push_stack s1 c1), T (push_stack s2 c1)

let ascribe_lit lit = ascribe Fun.id (new_lit lit)

let rec type_expr c e = 
  ascribe (List.map (type_word c)) (new_endo ()) e

and type_word (c : (Type.t * Type.t) Dict.t) (w : ContentAst.w) : TypedAst.w = 
  match w#ast with
  | IntLit _ -> ascribe_lit (uref Var.(Mono Int)) w
  (* | CharLit _ -> ascribe_lit (uref Var.(Mono Char)) w *)
  (* | Id s -> ascribe Fun.id (Dict.find s c) w *)
  (* | QuoteLit e -> 
    let ty = type_expr c e in *)


(* let rec type_expr e = 
  ascribe_node (new_endo ()) (e#ast <- List.map type_word e#ast) *)

(* and type_word = 
  let open TypedAst in
  | ContentAst.IntLit *)
let z (y : ContentAst.w) = ascribe_lit (uref Var.(Mono Int)) y