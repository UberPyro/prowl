open! Batteries
open Printf

open Metadata
open Syntax
open Ast
open System

open Ctx
open Unify
open Ucommon

module V = Value
module S = Stack
module C = Costack

exception InferError of
    Span.t
  * Ctx.t
  * string

let and2 d0 e0 d1 e1 d2 e2 = 
  Det.unify d0 (uref @@ Det.mul_basic (uget d1) (uget d2));
  Det.unify e0 (uref @@ Det.mul_basic (uget e1) (uget e2))
let and1 d0 e0 d1 e1 = Det.unify d0 d1, Det.unify e0 e1
let and2_ d0 d1 d2 = 
  Det.unify d0 (uref @@ Det.mul_basic (uget d1) (uget d2))
let and2_fresh () = 
  let d0, d1, d2 = Det.(bfresh (), bfresh (), bfresh ()) in
  and2_ d0 d1 d2;
  d0, d1, d2
let and2fresh () = and2_fresh (), and2_fresh ()
let union d1 d2 = d1 @ d2 @ Det.mul_basic d1 d2
let union_ d1 d2 = uref @@ union (uget d1) (uget d2)
let or_ d0 d1 d2 = Det.unify d0 @@ union_ d1 d2
let or2_fresh () = 
  let d0, d1, d2 = Det.(bfresh (), bfresh (), bfresh ()) in
  or_ d0 d1 d2;
  d0, d1, d2
let or2fresh () = or2_fresh (), or2_fresh ()
let b_any det = uref @@ [[ref (Det.BConst det)]]
let set_det d0 det = Det.unify d0 (b_any det)
let set_true d0 e0 = set_det d0 (true, true); set_det e0 (true, true)
let set_int v0 = Value.unify v0 (Value.usyn "int" [])
let set_hof hof v fn = Value.unify v @@ V.usyn hof [V.uatom fn]

let push_int u = V.usyn "int" [] @> u
let push_str u = V.usyn "string" [] @> u
let push_quo fn u = V.usyn "quote" [V.uatom fn] @> u
let push_list fn u = V.usyn "list" [V.uatom fn] @> u

let rec infer ctx (ast, sp, (i0, o0, d0, e0 as fn0)) = try match ast with
  | Bop (left, Aop _, right) -> ints @@ bop ctx fn0 left right
  | Bop (left, Cop _, right) -> ints2 @@ bop_cmp ctx fn0 left right
  | Bop (left, Lop Cat, right) -> cat "quote" @@ bop ctx fn0 left right
  | Bop (left, Lop Ap, right) -> cat "list" @@ bop ctx fn0 left right
  | Bop (left, Lop Alt, right) -> alt "quote" @@ bop ctx fn0 left right
  | Bop (left, Lop Append, right) -> app @@ bop ctx fn0 left right
  | Bop (left, Lop Join, right) -> alt "list" @@ bop ctx fn0 left right
  | SectLeft (Aop _, right) -> ints @@ sect_left ctx fn0 right
  | SectLeft (Cop _, right) -> ints2 @@ sect_left_cmp ctx fn0 right
  | SectLeft (Lop Cat, right) -> cat "quote" @@ sect_left ctx fn0 right
  | SectLeft (Lop Ap, right) -> cat "list" @@ sect_left ctx fn0 right
  | SectLeft (Lop Alt, right) -> alt "quote" @@ sect_left ctx fn0 right
  | SectLeft (Lop Append, right) -> app @@ sect_left ctx fn0 right
  | SectLeft (Lop Join, right) -> alt "list" @@ sect_left ctx fn0 right
  | SectRight (left, Aop _) -> ints @@ sect_right ctx fn0 left
  | SectRight (left, Cop _) -> ints2 @@ sect_right_cmp ctx fn0 left
  | SectRight (left, Lop Cat) -> cat "quote" @@ sect_right ctx fn0 left
  | SectRight (left, Lop Ap) -> cat "list" @@ sect_right ctx fn0 left
  | SectRight (left, Lop Alt) -> alt "quote" @@ sect_right ctx fn0 left
  | SectRight (left, Lop Append) -> app @@ sect_right ctx fn0 left
  | SectRight (left, Lop Join) -> alt "list" @@ sect_right ctx fn0 left
  | Sect Aop _ -> ints @@ sect fn0
  | Sect Cop _ -> ints2 @@ sect_cmp fn0
  | Sect Lop Cat -> cat "quote" @@ sect fn0
  | Sect Lop Ap -> cat "list" @@ sect fn0
  | Sect Lop Alt -> alt "quote" @@ sect fn0
  | Sect Lop Append -> app @@ sect fn0
  | Sect Lop Join -> alt "list" @@ sect fn0
    
  | Uop ((_, _, (i1, o1, d1, e1) as just), Dag) -> 
    infer (ctx |> swap_uvar |> swap_svar) just;
    o0 =?= i1; i0 =?= o1; 
    Det.unify d0 d1; Det.unify e0 e1
  
  | Uop ((_, _, (i1, o1, _, _) as just), (Mark | Star)) -> 
    infer ctx just;
    i1 =?= o1; i0 =?= i1; o0 =?= o1; 
    set_det d0 (false, true);
    set_det e0 (false, true)
  
  | Uop ((_, _, (i1, o1, d1, e1) as just), Plus) -> 
    infer ctx just;
    i1 =?= o1; i0 =?= i1; o0 =?= o1; 
    Det.unify d0 (union_ d1 (b_any (false, true)));
    Det.unify e0 (union_ e1 (b_any (false, true)))
  
  | Dop ((_, _, (i1, o1, _, _) as left), Ponder, (_, _, (i2, o2, _, _) as right)) -> 
    base_and ctx fn0 left right;
    i0 =?= C.rebase i1 i2; o0 =?= C.rebase o1 o2;
  
  | Dop ((_, _, (i1, o1, _, _) as left), Pick, (_, _, (i2, o2, _, _) as right)) -> 
    base_and ctx fn0 left right;
    i0 =?= C.rebase i1 i2;
    o0 =?= o1; o0 =?= o2
  
  | Dop ((_, _, (i1, o1, _, _) as left), Guess, (_, _, (i2, o2, _, _) as right)) -> 
    base_and ctx fn0 left right;
    i0 =?= i1; i0 =?= i2;
    o0 =?= C.rebase o1 o2
  
  | Dop ((_, _, (i1, o1, _, _) as left), Tensor, (_, _, (i2, o2, _, _) as right)) -> 
    base_and ctx fn0 left right;
    i1 =?= mk_poly_costack ();
    o1 =?= mk_poly_costack ();
    i2 =?= mk_end_costack ();
    o2 =?= mk_end_costack ();
    let s0, c0 = C.upop i1 in
    i0 =?= S.rebase s0 (C.upop i2 |> fst) @>> c0;
    let s1, c1 = C.upop o1 in
    o0 =?= S.rebase s1 (C.upop o2 |> fst) @>> c1
  
  | Dop ((_, _, (i1, o1, _, _) as left), Fork, (_, _, (i2, o2, _, _) as right)) -> 
    base_and ctx fn0 left right;
    o1 =?= mk_poly_costack ();
    o2 =?= mk_poly_costack ();
    i0 =?= i1; i0 =?= i2;
    let s0, c0 = C.upop o1 in
    o0 =?= S.rebase s0 (C.upop o2 |> fst) @>> c0
  
  | Dop ((_, _, (i1, o1, _, _) as left), Cross, (_, _, (i2, o2, _, _) as right)) -> 
    base_and ctx fn0 left right;
    i1 =?= mk_poly_costack ();
    i2 =?= mk_end_costack ();
    let s0, c0 = C.upop i1 in
    i0 =?= S.rebase s0 (C.upop i2 |> fst) @>> c0;
    o0 =?= o1; o0 =?= o2
  
  | Dop ((_, _, (i1, o1, _, _) as left), Jux, (_, _, (i2, o2, _, _) as right)) -> 
    base_and ctx fn0 left right;
    i0 =?= i1;
    o1 =?= i2;
    o0 =?= o2
  
  | Dop ((_, _, (i1, o1, _, _) as left), Union, (_, _, (i2, o2, _, _) as right)) -> 
    base_or ctx fn0 left right;
    i0 =?= i1; i0 =?= i2;
    o0 =?= o1; o0 =?= o2
  
  | Nop Gen -> 
    let stunted, stack = C.ufresh (), S.ufresh () in
    i0 =?= stack @>> stunted;
    o0 =?= stack @>> S.ufresh () @>> stunted;
    set_true d0 e0
  
  | Nop Fab -> 
    let costack = mk_poly_costack () in
    i0 =?= costack;
    o0 =?= S.ufresh () @>> costack;
    set_true d0 e0
  
  | Nop Exch -> 
    let stunted, s1, s2 = C.ufresh (), S.ufresh (), S.ufresh () in
    i0 =?= s1 @>> s2 @>> stunted;
    o0 =?= s2 @>> s1 @>> stunted;
    set_true d0 e0
  
  | Nop Elim -> 
    let stunted, stack = C.ufresh () , S.ufresh () in
    let costack = stack @>> stunted in
    i0 =?= stack @>> costack;
    o0 =?= costack;
    set_true d0 e0
  
  | Nop Cmp -> 
    let stunted, stack = C.ufresh () , S.ufresh () in
    let costack = stack @>> stunted in
    i0 =?= push_int @@ push_int costack;
    o0 =?= stack @>> stack @>> costack;
    set_true d0 e0
  
  | Nop Dup -> 
    let var = mk_var () in
    let costack = var @> mk_poly_costack () in
    i0 =?= costack;
    o0 =?= var @> costack;
    set_true d0 e0
  
  | Nop Zap -> 
    let costack = mk_poly_costack () in
    i0 =?= mk_var () @> costack;
    o0 =?= costack;
    set_true d0 e0
  
  | Nop Swap -> 
    let costack, v1, v2 = mk_poly_costack (), mk_var (), mk_var () in
    i0 =?= v1 @> v2 @> costack;
    o0 =?= v2 @> v1 @> costack;
    set_true d0 e0
  
  | Nop Cons -> 
    let c0, c1, c2 = Tuple3.mapn C.ufresh ((), (), ()) in
    let v = mk_var () in
    let d1, d2 = Det.bfresh (), Det.bfresh () in
    i0 =?= push_quo (v @> c1, c2, d1, d2) @@ v @> c0;
    o0 =?= push_quo (c1, c2, d1, d2) @@ c0;
    set_true d0 e0
  
  | Nop Dip -> 
    let c0, c1, v = C.ufresh (), C.ufresh (), V.uvar () in
    let d1, d2 = Det.bfresh (), Det.bfresh () in
    i0 =?= push_quo (c0, c1, d1, d2) @@ v @> c0;
    o0 =?= v @> c1;
    Det.unify d0 d1; Det.unify e0 d2

  | Nop Unit -> 
    let c0, c1 = mk_poly_costack (), mk_poly_costack () in
    let v = mk_var () in
    i0 =?= v @> c0;
    o0 =?= push_quo (c1, v @> c1, b_any (true, true), b_any (true, true)) @@ c0;
    set_true d0 e0
  
  | Nop Pure -> 
    let c0, c1 = mk_poly_costack (), mk_poly_costack () in
    let v = mk_var () in
    i0 =?= v @> c0;
    o0 =?= push_list (c1, v @> c1, b_any (true, true), b_any (true, true)) @@ c0;
    set_true d0 e0
  
  | Nop DivMod -> 
    let c1 = push_int @@ push_int @@ mk_poly_costack () in
    i0 =?= c1; o0 =?= c1;
    set_true d0 e0
  
  | Nop Lin -> 
    let c1, c2 = mk_poly_costack (), mk_poly_costack () in
    let d1, d2, d3 = and2_fresh () in
    let e1, e2, e3 = and2_fresh () in
    let s = S.ufresh () in
    let c0 = s @>> C.ufresh () in
    i0 =?= push_quo (c1, c2, d2, e2) @@ push_list (c1, c2, d3, e3) @@ s @>> c0;
    o0 =?= push_list (c1, c2, d1, e1) @@ c0;
    set_true d0 e0
  
  | Nop Parse -> 
    let c0 = mk_poly_costack () in
    i0 =?= push_str c0;
    o0 =?= push_int c0;
    set_true d0 e0
  
  | Nop Show -> 
    let c0 = mk_poly_costack () in
    i0 =?= push_int c0;
    o0 =?= push_str c0;
    set_true d0 e0
  
  | Nop Noop -> i0 =?= o0; set_true d0 e0

  | Nop Id -> 
    let c0 = mk_var () @> mk_poly_costack () in
    i0 =?= c0; o0 =?= c0;
    set_true d0 e0
  
  | Nop Ab -> 
    let c0 = mk_var () @> mk_poly_costack () in
    i0 =?= c0; o0 =?= c0; 
    set_det d0 (true, false);
    set_det e0 (true, false)
  
  | Lit Int _ -> 
    let c0 = mk_poly_costack () in
    i0 =?= c0; o0 =?= push_int c0;
    set_det d0 (false, true); set_det d0 (true, true)
  
  | Lit String _ -> 
    let c0 = mk_poly_costack () in
    i0 =?= c0; o0 =?= push_str c0;
    set_det d0 (false, true); set_det d0 (true, true)
  
  | Lit Quote (_, _, fn1 as just) -> 
    infer ctx just;
    let c0 = mk_poly_costack () in
    i0 =?= c0;
    o0 =?= push_quo fn1 c0;
    set_det d0 (false, true); set_det d0 (true, true)
  
  | Lit List lst -> 
    let fn0 = mk_poly_costack (), mk_poly_costack (), Det.bfresh (), Det.bfresh () in
    List.iter begin fun (_, _, fn as just) -> 
      infer ctx just;
      Fn.unify fn0 fn
    end lst;
    let c0 = mk_poly_costack () in
    i0 =?= c0;
    o0 =?= push_list fn0 c0;
    set_det d0 (false, true); set_det d0 (true, true)
  
  | UVar s -> 
    let v = mk_var () in
    unify_uvar s v ctx;
    let c = mk_poly_costack () in
    i0 =?= c;
    o0 =?= v @> c;
    set_true d0 e0
  
  | StackVar s -> 
    let z = S.ufresh () in
    unify_ustack s z ctx;
    let c = mk_poly_costack () in
    i0 =?= c;
    o0 =?= z @>> c;
    set_true d0 e0
  
  | Ex (s, (_, _, (i1, o1, d1, e1) as just), b) ->
    let v = V.uvar () in
    let ctx' = introduce_uvar s v ctx in
    infer ctx' just;
    begin if b then (i0 =?= v @> i1; uincr_op s ctx')
    else i0 =?= i1 end;
    o0 =?= o1;
    let uc = Map.find s ctx'.ucount |> (!) in
    and2_ d0 d1 @@ b_any @@ (uc > 1, uc < 1);
    let udagc = Map.find s ctx'.udagcount |> (!) in
    and2_ e0 e1 @@ b_any @@ (udagc > 1, udagc < 1)
  
  | Each ((_, _, (i1, o1, d1, e1) as just), s, b) ->
    let z = S.ufresh () in
    let ctx' = introduce_stkvar s z ctx in
    infer ctx' just;
    i0 =?= i1;
    begin if b then (o0 =?= z @>> o1; sincr s ctx')
    else o0 =?= o1 end;
    let sdagc = Map.find s ctx'.sdagcount |> (!) in
    and2_ d0 d1 @@ b_any (sdagc < 1, sdagc > 1);
    let sc = Map.find s ctx'.scount |> (!) in
    and2_ e0 e1 @@ b_any (sc < 1, sc > 1)
  
  | Var k -> 
    let (generalized, (i1, o1, d1, e1)), _ = 
      find_rec_opt k ctx
      |> Option.default_delayed @@ fun () -> 
        let msg = sprintf "Cannot find unbound variable [%s]" k in
        UnifError msg |> raise in
    let i, o, d, e = 
      if generalized then begin
        Fn.refresh_memo ();
        Fn.generalize (Fn.memo ()) (i1, o1, d1, e1);
      end else i1, o1, d1, e1 in
    i0 =?= i; o0 =?= o;
    Det.unify d0 d; Det.unify e0 e
  
  | Let (stmts, e) -> infer (stmts_rec ctx stmts) e

  with UnifError msg -> raise @@ InferError (sp, ctx, msg)

and stmts_rec ctx stmts = 
  let ctx' = insert_many begin stmts |> List.map @@ function
    | Def (s, None, (_, _, fn)), _ -> s, (false, fn)
    | Def (d, Some ty, (_, _, fn)), _ -> 
      Fn.unify fn (Elab.ty_expr ty);
      d, (true, fn)
  end ctx in
  List.iter (fun (Def (_, _, e), _) -> infer ctx' e) stmts;
  ctx'

and base_and ctx (_, _, d0, e0) (_, _, (_, _, d1, e1) as left) (_, _, (_, _, d2, e2) as right) = 
  infer ctx left;
  infer ctx right;
  and2_ d0 d1 d2;
  and2_ e0 e1 e2

and base_or ctx (_, _, d0, e0) (_, _, (_, _, d1, e1) as left) (_, _, (_, _, d2, e2) as right) = 
  infer ctx left;
  infer ctx right;
  or_ d0 d1 d2;
  or_ e0 e1 e2

and bop ctx (i0, o0, d0, e0) (_, _, (i1, o1, d1, e1) as left) (_, _, (i2, o2, d2, e2) as right) = 
  infer ctx left;
  infer ctx right;
  let v0, v1, v2 = Value.(uvar (), uvar (), uvar ()) in
  let c0, c1, c2 = mk_poly_costack (), mk_poly_costack (), mk_poly_costack () in
  i0 =?= c0; o0 =?= v0 @> c0;
  i1 =?= c1; o1 =?= v1 @> c1;
  i2 =?= c2; o2 =?= v2 @> c2;
  and2_ d0 d1 d2; and2_ e0 e1 e2;
  v0, v1, v2

and sect_left ctx (i0, o0, d0, e0) (_, _, (i2, o2, d2, e2) as right) = 
  infer ctx right;
  let v0, v1, v2 = Value.(uvar (), uvar (), uvar ()) in
  let c0, c2 = mk_poly_costack (), mk_poly_costack () in
  i0 =?= v1 @> c0; o0 =?= v0 @> c0;
  i2 =?= c2; o2 =?= v2 @> c0;
  Det.unify d0 d2; Det.unify e0 e2;
  v0, v1, v2

and sect_right ctx (i0, o0, d0, e0) (_, _, (i1, o1, d1, e1) as left) = 
  infer ctx left;
  let v0, v1, v2 = Value.(uvar (), uvar (), uvar ()) in
  let c0, c1 = mk_poly_costack (), mk_poly_costack () in
  i0 =?= v2 @> c0; o0 =?= v0 @> c0;
  i1 =?= c1; o1 =?= v1 @> c0;
  Det.unify d0 d1; Det.unify e0 e1;
  v0, v1, v2

and sect (i0, o0, d0, e0) = 
  let v0, v1, v2 = Value.(uvar (), uvar (), uvar ()) in
  let c0 = mk_poly_costack () in
  i0 =?= v2 @> v1 @> c0; o0 =?= v0 @> c0;
  set_true d0 e0; 
  v0, v1, v2

and bop_cmp ctx (i0, o0, d0, e0) (_, _, (i1, o1, d1, e1) as left) (_, _, (i2, o2, d2, e2) as right) = 
  infer ctx left;
  infer ctx right;
  let v1, v2 = Value.(uvar (), uvar ()) in
  let s0 = Stack.ufresh () in
  let c0, c1, c2 = s0 @>> mk_init_costack (), mk_poly_costack (), mk_poly_costack () in
  i0 =?= c0; o0 =?= s0 @>> c0;
  i1 =?= c1; o1 =?= v1 @> c1;
  i2 =?= c2; o2 =?= v2 @> c2;
  and2_ d0 d1 d2; and2_ e0 e1 e2;
  v1, v2

and sect_left_cmp ctx (i0, o0, d0, e0) (_, _, (i2, o2, d2, e2) as right) = 
  infer ctx right;
  let v1, v2 = Value.(uvar (), uvar ()) in
  let s0 = Stack.ufresh () in
  let c0, c2 = mk_poly_costack (), mk_poly_costack () in
  i0 =?= v1 @> c0; o0 =?= s0 @>> c0;
  i2 =?= c2; o2 =?= v2 @> c0;
  Det.unify d0 d2; Det.unify e0 e2;
  v1, v2

and sect_right_cmp ctx (i0, o0, d0, e0) (_, _, (i1, o1, d1, e1) as left) = 
  infer ctx left;
  let v1, v2 = Value.(uvar (), uvar ()) in
  let s0 = Stack.ufresh () in
  let c0, c1 = mk_poly_costack (), mk_poly_costack () in
  i0 =?= v2 @> c0; o0 =?= s0 @>> c0;
  i1 =?= c1; o1 =?= v1 @> c0;
  Det.unify d0 d1; Det.unify e0 e1;
  v1, v2

and sect_cmp (i0, o0, d0, e0) = 
  let v1, v2 = Value.(uvar (), uvar ()) in
  let s0 = Stack.ufresh () in
  let c0 = mk_poly_costack () in
  i0 =?= v2 @> v1 @> c0; o0 =?= s0 @>> c0;
  set_true d0 e0; 
  v1, v2

and ints (v0, v1, v2) = 
  set_int v0; set_int v1; set_int v2

and ints2 (v1, v2) = set_int v1; set_int v2

and cat hof (v0, v1, v2) = 
  let c0, c1, c2 = Costack.(ufresh (), ufresh (), ufresh ()) in
  let d0, d1, d2 = and2_fresh () in
  let e0, e1, e2 = and2_fresh () in
  set_hof hof v0 (c0, c2, d0, e0);
  set_hof hof v1 (c0, c1, d1, e1);
  set_hof hof v2 (c1, c2, d2, e2)

and alt hof (v0, v1, v2) = 
  let c0, c1 = Costack.(ufresh () , ufresh ()) in
  let d0, d1, d2 = or2_fresh () in
  let e0, e1, e2 = or2_fresh () in
  set_hof hof v0 (c0, c1, d0, e0);
  set_hof hof v1 (c0, c1, d1, e1);
  set_hof hof v2 (c0, c1, d2, e2)

and app (v0, v1, v2) = 
  let c0, c1 = Costack.(ufresh () , ufresh ()) in
  let d0, d1, d2 = and2_fresh () in
  let e0, e1, e2 = and2_fresh () in
  set_hof "list" v0 (c0, c1, d0, e0);
  set_hof "list" v1 (c0, c1, d1, e1);
  set_hof "list" v2 (c0, c1, d2, e2)

let top_stmts ctx = 
  List.fold_left begin fun ctx' -> function
    | Def (d, None, (_, _, fn as e)), _ -> 
      infer (insert d (false, fn) ctx') e;
      insert d (true, fn) ctx'
    | Def (d, Some ty, (_, _, fn as e)), _ -> 
      let elab_ty = Elab.ty_expr ty in
      Fn.unify fn elab_ty;
      let annotctx = insert d (true, fn) ctx' in
      infer annotctx e;
      annotctx
  end ctx

let prog : (_stmt * Span.t) list -> 'a = 
  top_stmts empty
