open! Batteries
open Uref

open Type
open Ast
open Typedata
open Util

let rec elab_expr (i : Intern.t) (e, _sp) = match e with
  | ImplStack (w1, w2) -> 
    let r = mk_seq () in
    let c = mk_seq () in
    {
      arg_wing = push c (elab_impl_stack i r w1);
      low_wing = push c r;
      res_wing = push c (elab_impl_stack i r w2);
    }
  | ImplCostack (w1, w2) -> 
    let c = mk_seq () in
    {
      arg_wing = elab_impl_costack i c w1;
      low_wing = c;
      res_wing = elab_impl_costack i c w2;
    }
  | Expl ((e1, w1), (e2, w2), (e3, w3)) -> 
    let elab_bot = function
      | NextCostack s -> mk_unit (Intern.intern i s)
      | Void -> uref Bottom in
    {
      arg_wing = elab_impl_costack i (elab_bot e1) w1;
      low_wing = elab_impl_costack i (elab_bot e2) w2;
      res_wing = elab_impl_costack i (elab_bot e3) w3;
    }

and elab_impl_stack (i : Intern.t) expl_stack w : type_stack = 
  List.fold_left begin fun a -> function
    | SVar v -> push a @@ elab_val i v
    | SStack s -> concat a @@ Intern.intern i s
  end expl_stack w

and elab_impl_costack (i : Intern.t) expl_costack w : type_costack = 
  List.fold_left begin fun a -> function
    | CStack (Unit, w) -> push a @@ elab_impl_stack i (uref Bottom) w
    | CStack (NextStack s, w) -> 
      push a @@ elab_impl_stack i (mk_unit (Intern.intern i s)) w
    | CCostack s -> concat a (Intern.intern i s)
  end expl_costack w

and elab_val i = uref % function
  | TLit l -> DLit (elab_lit l)
  | TCon (l, e) -> DCon (elab_con l, elab_expr i e)
  | TVar s -> DVar (Intern.intern i s)

and elab_lit = function
  | TInt -> DInt
  | TString -> DString

and elab_con = function
  | TQuote -> DQuote
  | TList -> DList
