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
  
let rec infer e = 
  let rec cat ix ox = function
    | h1 :: (h2 :: _ as t) -> 
      let _, o1 = h1#ty in
      let i2, _ = h2#ty in
      infer_word h1; 
      unify o1 i2; 
      cat ix ox t
    | [x] -> 
      let i, o = x#ty in
      infer_word x; 
      unify ix i; 
      unify ox o
    | [] -> unify ix ox in
  let rec alt ix ox = function
    | h1 :: (h2 :: _ as t) -> 
      let i1, o1 = new_endo () in
      cat i1 o1 h1; 
      let i2, o2 = new_endo () in
      cat i2 o2 h2; 
      let s1 = new_stack () in
      let s2 = new_stack () in
      let c1 = new_costack () in
      let c2 = push_stack s1 c1 in
      let c3 = push_stack s2 c2 in
      let c4 = push_stack s2 c1 in
      let t1, t2, t3, t4 = T c1, T c2, T c3, T c4 in
      unify ix i1; 
      unify o1 t3; 
      unify t2 i2; 
      unify o2 t1; 
      alt t4 ox t
    | [x] -> 
      let i, o = new_endo () in
      cat i o x; 
      unify ix i; 
      unify ox o
    | [] -> failwith "The empty alternation is syntactically void" in
  let i0, o0 = e#ty in
  alt i0 o0 e#ast

and infer_word w = 
  let open TypedAst in match w#ast with
  | IntLit _ | CharLit _ | Id _ -> ()
  | QuoteLit e -> infer e
  | ListLit es -> List.iter infer es
  | Expr e -> infer e
