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

let ascribe x ty obj = object
  method ast = x
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
  T (push_stack (new_stack ()) (new_costack ()))

let new_fn () = 
  new_costack (), new_costack ()

let new_endo () = 
  let c = new_costack () in
  T c, T c

let new_stack_endo () = 
  let c = new_distack () in
  c, c

let new_lit v = 
  let c1 = new_costack () in
  let s1 = new_stack () in
  let s2 = push_var v s1 in
  T (push_stack s1 c1), T (push_stack s2 c1)

let rec type_expr c (e : ContentAst.e) : TypedAst.e = 
  ascribe (List.map (List.map (type_word c)) e#ast) (new_endo ()) e

and type_word c (w : ContentAst.w) : TypedAst.w = 
  let open TypedAst in
  match w#ast with
  | IntLit i -> ascribe (IntLit i) (new_lit (uref Var.(Mono Int))) w
  | CharLit c -> ascribe (CharLit c) (new_lit (uref Var.(Mono Char))) w
  | Id s -> ascribe (Id s) (Dict.find s c) w
  | QuoteLit e -> 
    let te = type_expr c e in
    let i, o = te#ty in
    ascribe (QuoteLit te) (new_lit (uref Var.(Duo (Quote, i, o)))) w
  | ListLit es -> 
    begin match List.map (type_expr c) es with
      | [] ->
        ascribe 
          (ListLit [])
          (new_lit (uref Var.(Duo (List, new_distack (), new_distack ()))))
          w
      | h :: _ as tes -> 
        let i, o = h#ty in
        ascribe (ListLit tes) (new_lit (uref Var.(Duo (List, i, o)))) w end
  | Expr e -> 
    let te = type_expr c e in
    ascribe (Expr te) te#ty w

  
  (* | CharLit _ -> ascribe_lit (uref Var.(Mono Char)) w *)
  (* | Id s -> ascribe Fun.id (Dict.find s c) w *)
  (* | QuoteLit e -> 
    let ty = type_expr c e in *)


(* let rec type_expr e = 
  ascribe_node (new_endo ()) (e#ast <- List.map type_word e#ast) *)

(* and type_word = 
  let open TypedAst in
  | ContentAst.IntLit *)

