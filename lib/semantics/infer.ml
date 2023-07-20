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
let or_ d0 d1 d2 = 
  let d1, d2 = uget d1, uget d2 in
  Det.unify d0 (uref @@ d1 @ d2 @ Det.mul_basic d1 d2)
let or2_fresh () = 
  let d0, d1, d2 = Det.(bfresh (), bfresh (), bfresh ()) in
  or_ d0 d1 d2;
  d0, d1, d2
let or2fresh () = or2_fresh (), or2_fresh ()
let btrue () = uref @@ [[ref (Det.BConst (true, true))]]
let set_true d0 = Det.unify d0 (btrue ())
let set_int v0 = Value.unify v0 (Value.usyn "int" [])
let set_hof hof v fn = Value.unify v @@ V.usyn hof [V.uatom fn]

let push_int u = V.usyn "int" [] @> u
let push_str u = V.usyn "string" [] @> u
let push_quo fn u = V.usyn "quote" [V.uatom fn] @> u
let push_list fn u = V.usyn "list" [V.uatom fn] @> u

let rec infer ctx (ast, sp, (i0, o0, d0, e0)) = try match ast with
  | Bop ((_, sp1, (i1, o1, d1, e1) as left), Aop _, (_, sp2, (i2, o2, d2, e2) as right)) -> 
    let u = mk_init_costack () in
    unify_arg sp1 ctx left u i1 o1;
    unify_arg sp2 ctx right u i2 o2;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= push_int p;
    and2 d0 e0 d1 e1 d2 e2
  
  | Bop ((_, sp1, (i1, o1, d1, e1) as left), Cop _, (_, sp2, (i2, o2, d2, e2) as right)) -> 
    let u = mk_init_costack () in
    unify_arg sp1 ctx left u i1 o1;
    unify_arg sp2 ctx right u i2 o2;
    let s = S.ufresh () in
    let p = s @>> C.ufresh () in
    i0 =?= p;
    o0 =?= s @>> p;
    and2 d0 e0 d1 e1 d2 e2

  | Bop ((_, sp1, (i1, o1, d1, e1) as left), Lop Cat, (_, sp2, (i2, o2, d2, e2) as right)) -> 
    let u = mk_init_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    let (f1, f2, f3), (g1, g2, g3) = and2fresh () in
    unify_ho_arg sp1 ctx left u c1 c2 f2 g2 push_quo i1 o1;
    unify_ho_arg sp2 ctx right u c2 c3 f3 g3 push_quo i2 o2;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= push_quo (c1, c3, f1, g1) p;
    and2 d0 e0 d1 e1 d2 e2
    
  | Bop ((_, sp1, (i1, o1, d1, e1) as left), Lop Ap, (_, sp2, (i2, o2, d2, e2) as right)) -> 
    let u = mk_init_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    let (f1, f2, f3), (g1, g2, g3) = and2fresh () in
    unify_ho_arg sp1 ctx left u c1 c2 f2 g2 push_list i1 o1;
    unify_ho_arg sp2 ctx right u c2 c3 f3 g3 push_list i2 o2;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= push_list (c1, c3, f1, g1) p;
    and2 d0 e0 d1 e1 d2 e2
  
  | Bop ((_, sp1, (i1, o1, d1, e1) as left), Lop Alt, (_, sp2, (i2, o2, d2, e2) as right)) -> 
    let u = mk_init_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    let (f1, f2, f3), (g1, g2, g3) = or2fresh () in
    unify_ho_arg sp1 ctx left u c1 c2 f2 g2 push_quo i1 o1;
    unify_ho_arg sp2 ctx right u c1 c2 f3 g3 push_quo i2 o2;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= push_quo (c1, c2, f1, g1) p;
    and2 d0 e0 d1 e1 d2 e2
    
  | Bop ((_, sp1, (i1, o1, d1, e1) as left), Lop Append, (_, sp2, (i2, o2, d2, e2) as right)) -> 
    let u = mk_init_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    let (f1, f2, f3), (g1, g2, g3) = and2fresh () in
    unify_ho_arg sp1 ctx left u c1 c2 f2 g2 push_list i1 o1;
    unify_ho_arg sp2 ctx right u c1 c2 f3 g3 push_list i2 o2;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= push_list (c1, c2, f1, g1) p;
    and2 d0 e0 d1 e1 d2 e2
    
  | Bop ((_, sp1, (i1, o1, d1, e1) as left), Lop Join, (_, sp2, (i2, o2, d2, e2) as right)) -> 
    let u = mk_init_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    let (f1, f2, f3), (g1, g2, g3) = or2fresh () in
    unify_ho_arg sp1 ctx left u c1 c2 f2 g2 push_list i1 o1;
    unify_ho_arg sp2 ctx right u c1 c2 f3 g3 push_list i2 o2;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= push_list (c1, c2, f1, g1) p;
    and2 d0 e0 d1 e1 d2 e2
    
  | SectLeft (Aop _, (_, sp1, (i1, o1, d1, e1) as just)) -> 
    let u = mk_init_costack () in
    unify_arg sp1 ctx just u i1 o1;
    let p = push_int @@ mk_poly_costack () in
    i0 =?= p;
    o0 =?= p; 
    Det.unify d0 d1; Det.unify e0 e1
  
  | SectLeft (Cop _, (_, sp1, (i1, o1, d1, e1) as just)) -> 
    let u = mk_init_costack () in
    unify_arg sp1 ctx just u i1 o1;
    let s = S.ufresh () in
    let p = s @>> C.ufresh () in
    i0 =?= push_int p;
    o0 =?= s @>> p;
    Det.unify d0 d1; Det.unify e0 e1
  
  | SectLeft (Lop Cat, (_, sp1, (i1, o1, d1, e1) as just)) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    let (f1, f2, f3), (g1, g2, g3) = and2fresh () in
    unify_ho_arg sp1 ctx just u c1 c2 f2 g2 push_quo i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_quo (c2, c3, f3, g3) p;
    o0 =?= push_quo (c1, c3, f1, g1) p;
    Det.unify d0 d1; Det.unify e0 e1
  
  | SectLeft (Lop Ap, (_, sp1, (i1, o1, d1, e1) as just)) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_list i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_list c1 c2 p;
    o0 =?= push_list c1 c3 p;
    Det.unify d0 d1; Det.unify e0 e1

  | SectLeft (Lop Alt, (_, sp1, (i1, o1, d1, e1) as just)) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_quo i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_quo c1 c2 p;
    o0 =?= push_quo c1 c2 p
    
  | SectLeft (Lop (Append | Join), (_, sp1, (i1, o1, d1, e1) as just)) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_list i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_list c1 c2 p;
    o0 =?= push_list c1 c2 p
    
  | SectRight ((_, sp1, (i1, o1, d1, e1) as just), Aop _) -> 
    infer ctx just;
    let u = mk_init_costack () in
    unify_arg sp1 ctx just u i1 o1;
    let p = push_int @@ mk_poly_costack () in
    i0 =?= p;
    o0 =?= p
  
  | SectRight ((_, sp1, (i1, o1, d1, e1) as just), Cop _) -> 
    infer ctx just;
    let u = mk_init_costack () in
    unify_arg sp1 ctx just u i1 o1;
    let s = S.ufresh () in
    let p = s @>> C.ufresh () in
    i0 =?= push_int p;
    o0 =?= s @>> p
  
  | SectRight ((_, sp1, (i1, o1, d1, e1) as just), Lop Cat) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_quo i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_quo c2 c3 p;
    o0 =?= push_quo c1 c3 p
  
  | SectRight ((_, sp1, (i1, o1, d1, e1) as just), Lop Ap) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_list i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_list c2 c3 p;
    o0 =?= push_list c1 c3 p

  | SectRight ((_, sp1, (i1, o1, d1, e1) as just), Lop Alt) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_quo i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_quo c1 c2 p;
    o0 =?= push_quo c1 c2 p;
    
  | SectRight ((_, sp1, (i1, o1, d1, e1) as just), Lop (Append | Join)) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_list i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_list c1 c2 p;
    o0 =?= push_list c1 c2 p;
    
  | Sect Aop _ -> 
    let p = push_int @@ mk_poly_costack () in
    i0 =?= push_int p;
    o0 =?= p
  
  | Sect Cop _ -> 
    let s = S.ufresh () in
    let p = s @>> C.ufresh () in
    i0 =?= push_int @@ push_int p;
    o0 =?= s @>> p
  
  | Sect (Lop Cat) -> 
    let c0 = mk_poly_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    i0 =?= push_quo c2 c3 @@ push_quo c1 c2 c0;
    o0 =?= push_quo c1 c3 c0
  
  | Sect (Lop Ap) -> 
    let c0 = mk_poly_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    i0 =?= push_list c2 c3 @@ push_list c1 c2 c0;
    o0 =?= push_list c1 c3 c0

  | Sect (Lop Alt) -> 
    let c0 = mk_poly_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    i0 =?= push_quo c1 c2 @@ push_quo c1 c2 c0;
    o0 =?= push_quo c1 c2 c0;
    
  | Sect (Lop (Append | Join)) -> 
    let c0 = mk_poly_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    i0 =?= push_list c1 c2 @@ push_list c1 c2 c0;
    o0 =?= push_list c1 c2 c0;
    
  | Uop ((_, _, (i1, o1, d1, e1) as just), Dag) -> 
    infer ctx just;
    o0 =?= i1;
    i0 =?= o1
  
  | Uop ((_, _, (i1, o1, d1, e1) as just), (Mark | Star | Plus)) -> 
    infer ctx just;
    i1 =?= o1;
    i0 =?= i1;
    o0 =?= o1
  
  | Dop ((_, _, (i1, o1, d1, e1) as left), Ponder, (_, _, (i2, o2, d2, e2) as right)) -> 
    infer ctx left;
    infer ctx right;
    i0 =?= C.rebase i1 i2;
    o0 =?= C.rebase o1 o2
  
  | Dop ((_, _, (i1, o1, d1, e1) as left), Pick, (_, _, (i2, o2, d2, e2) as right)) -> 
    infer ctx left;
    infer ctx right;
    i0 =?= C.rebase i1 i2;
    o0 =?= o1;
    o0 =?= o2
  
  | Dop ((_, _, (i1, o1, d1, e1) as left), Guess, (_, _, (i2, o2, d2, e2) as right)) -> 
    infer ctx left;
    infer ctx right;
    i0 =?= i1;
    i0 =?= i2;
    o0 =?= C.rebase o1 o2
  
  | Dop ((_, _, (i1, o1, d1, e1) as left), Tensor, (_, _, (i2, o2, d2, e2) as right)) -> 
    infer ctx left;
    infer ctx right;
    i1 =?= mk_poly_costack ();
    o1 =?= mk_poly_costack ();
    i2 =?= mk_end_costack ();
    o2 =?= mk_end_costack ();
    let s0, c0 = C.upop i1 in
    i0 =?= S.rebase s0 (C.upop i2 |> fst) @>> c0;
    let s1, c1 = C.upop o1 in
    o0 =?= S.rebase s1 (C.upop o2 |> fst) @>> c1
  
  | Dop ((_, _, (i1, o1, d1, e1) as left), Fork, (_, _, (i2, o2, d2, e2) as right)) -> 
    infer ctx left;
    infer ctx right;
    o1 =?= mk_poly_costack ();
    o2 =?= mk_poly_costack ();
    i0 =?= i1;
    i0 =?= i2;
    let s0, c0 = C.upop o1 in
    o0 =?= S.rebase s0 (C.upop o2 |> fst) @>> c0
  
  | Dop ((_, _, (i1, o1, d1, e1) as left), Cross, (_, _, (i2, o2, d2, e2) as right)) -> 
    infer ctx left;
    infer ctx right;
    i1 =?= mk_poly_costack ();
    i2 =?= mk_end_costack ();
    let s0, c0 = C.upop i1 in
    i0 =?= S.rebase s0 (C.upop i2 |> fst) @>> c0;
    o0 =?= o1;
    o0 =?= o2
  
  | Dop ((_, _, (i1, o1, d1, e1) as left), Jux, (_, _, (i2, o2, d2, e2) as right)) -> 
    infer ctx left;
    infer ctx right;
    i0 =?= i1;
    o1 =?= i2;
    o0 =?= o2
  
  | Dop ((_, _, (i1, o1, d1, e1) as left), Union, (_, _, (i2, o2, d2, e2) as right)) -> 
    infer ctx left;
    infer ctx right;
    i0 =?= i1;
    i0 =?= i2;
    o0 =?= o1;
    o0 =?= o2
  
  | Nop Gen -> 
    let stunted, stack = C.ufresh (), S.ufresh () in
    i0 =?= stack @>> stunted;
    o0 =?= stack @>> S.ufresh () @>> stunted
  
  | Nop Fab -> 
    let costack = mk_poly_costack () in
    i0 =?= costack;
    o0 =?= S.ufresh () @>> costack
  
  | Nop Exch -> 
    let stunted, s1, s2 = C.ufresh (), S.ufresh (), S.ufresh () in
    i0 =?= s1 @>> s2 @>> stunted;
    o0 =?= s2 @>> s1 @>> stunted
  
  | Nop Elim -> 
    let stunted, stack = C.ufresh () , S.ufresh () in
    let costack = stack @>> stunted in
    i0 =?= stack @>> costack;
    o0 =?= costack
  
  | Nop Cmp -> 
    let stunted, stack = C.ufresh () , S.ufresh () in
    let costack = stack @>> stunted in
    i0 =?= push_int @@ push_int costack;
    o0 =?= stack @>> stack @>> costack
  
  | Nop Dup -> 
    let var = mk_var () in
    let costack = var @> mk_poly_costack () in
    i0 =?= costack;
    o0 =?= var @> costack
  
  | Nop Zap -> 
    let costack = mk_poly_costack () in
    i0 =?= mk_var () @> costack;
    o0 =?= costack
  
  | Nop Swap -> 
    let costack, v1, v2 = mk_poly_costack (), mk_var (), mk_var () in
    i0 =?= v1 @> v2 @> costack;
    o0 =?= v2 @> v1 @> costack
  
  | Nop Cons -> 
    let c0, c1, c2 = Tuple3.mapn C.ufresh ((), (), ()) in
    let v = mk_var () in
    i0 =?= push_quo (v @> c1) c2 @@ v @> c0;
    o0 =?= push_quo c1 c2 @@ c0
  
  | Nop Dip -> 
    let c0, c1, v = C.ufresh (), C.ufresh (), V.uvar () in
    i0 =?= push_quo c0 c1 @@ v @> c0;
    o0 =?= v @> c1

  | Nop Unit -> 
    let c0, c1 = mk_poly_costack (), mk_poly_costack () in
    let v = mk_var () in
    i0 =?= v @> c0;
    o0 =?= push_quo c1 (v @> c1) @@ c0
  
  | Nop Pure -> 
    let c0, c1 = mk_poly_costack (), mk_poly_costack () in
    let v = mk_var () in
    i0 =?= v @> c0;
    o0 =?= push_quo c1 (v @> c1) @@ c0
  
  | Nop DivMod -> 
    let c1 = push_int @@ push_int @@ mk_poly_costack () in
    i0 =?= c1;
    o0 =?= c1
  
  | Nop Lin -> 
    let c1, c2 = mk_poly_costack (), mk_poly_costack () in
    let s = S.ufresh () in
    let c0 = s @>> C.ufresh () in
    i0 =?= push_quo c1 c2 @@ push_list c1 c2 @@ s @>> c0;
    o0 =?= push_list c1 c2 @@ c0
  
  | Nop Parse -> 
    let c0 = mk_poly_costack () in
    i0 =?= push_str c0;
    o0 =?= push_int c0
  
  | Nop Show -> 
    let c0 = mk_poly_costack () in
    i0 =?= push_int c0;
    o0 =?= push_str c0
  
  | Nop Noop -> i0 =?= o0

  | Nop (Id | Ab) -> 
    let c0 = mk_var () @> mk_poly_costack () in
    i0 =?= c0;
    o0 =?= c0
  
  | Lit Int _ -> 
    let c0 = mk_poly_costack () in
    i0 =?= c0;
    o0 =?= push_int c0
  
  | Lit String _ -> 
    let c0 = mk_poly_costack () in
    i0 =?= c0;
    o0 =?= push_str c0
  
  | Lit Quote (_, _, (c1, c2) as just) -> 
    infer ctx just;
    let c0 = mk_poly_costack () in
    i0 =?= c0;
    o0 =?= push_quo c1 c2 c0
  
  | Lit List lst -> 
    let fn0 = mk_poly_costack (), mk_poly_costack () in
    List.iter begin fun (_, _, fn as just) -> 
      infer ctx just;
      Fn.unify fn0 fn
    end lst;
    let c0 = mk_poly_costack () in
    let c1, c2 = fn0 in
    i0 =?= c0;
    o0 =?= push_list c1 c2 c0
  
  | UVar s -> 
    let v = mk_var () in
    unify_uvar s v ctx;
    let c = mk_poly_costack () in
    i0 =?= c;
    o0 =?= v @> c
  
  | StackVar s -> 
    let z = S.ufresh () in
    unify_ustack s z ctx;
    let c = mk_poly_costack () in
    i0 =?= c;
    o0 =?= z @>> c
  
  | Ex (s, (_, _, (i1, o1, d1, e1) as just), b) ->
    let v = Value.uvar () in
    let ctx' = introduce_uvar s v ctx in
    infer ctx' just;
    if b then i0 =?= v @> i1
    else i0 =?= i1;
    o0 =?= o1
  
  | Each ((_, _, (i1, o1, d1, e1) as just), s, b) ->
    let z = S.ufresh () in
    let ctx' = introduce_stkvar s z ctx in
    infer ctx' just;
    i0 =?= i1;
    if b then o0 =?= z @>> o1
    else o0 =?= o1
  
  | Var k -> 
    let (generalized, (i1, o1, d1, e1)), _ = 
      find_rec_opt k ctx
      |> Option.default_delayed @@ fun () -> 
        let msg = sprintf "Cannot find unbound variable [%s]" k in
        UnifError msg |> raise in
    let i, o = 
      if generalized then begin
        Fn.refresh_memo ();
        Fn.generalize (Fn.memo ()) (i1, o1, d1, e1);
      end else i1, o1 in
    i0 =?= i;
    o0 =?= o
  
  | Let (stmts, e) -> infer (stmts_rec ctx stmts) e

  with UnifError msg -> raise @@ InferError (sp, ctx, msg)

and stmts_rec ctx stmts = 
  let ctx' = insert_many begin stmts |> List.map @@ function
    | Def (s, None, (_, _, (i, o))), _ -> s, (false, (i, o))
    | Def (d, Some ty, (_, _, (i, o as fn))), _ -> 
      Fn.unify fn (Elab.ty_expr ty);
      d, (true, (i, o))
  end ctx in
  List.iter (fun (Def (_, _, e), _) -> infer ctx' e) stmts;
  ctx'

(* and unify_arg sp ctx ast u i o = 
  infer ctx ast;
  try i =?= u; o =?= push_int u
  with UnifError msg -> InferError (sp, ctx, msg) |> raise

and unify_ho_arg sp ctx ast u c1 c2 d1 d2 f i o = 
  infer ctx ast;
  begin try
    i =?= u;
    o =?= f (c1, c2, d1, d2) u;
  with UnifError msg -> InferError (sp, ctx, msg) |> raise end

and cat (i0, o0, d0, e0) (i1, o1, d1, e1) (i2, o2, d2, e2) = 
  i0 =?= i1;
  o1 =?= i2;
  o0 =?= o2;
  and2 d0 e0 d1 e1 d2 e2

and alt (i0, o0, d0, e0) (i1, o1, d1, e1) (i2, o2, d2, e2) = 
  i0 =?= i1;
  i0 =?= i2;
  o0 =?= o1;
  o0 =?= o2;
  or_ d0 d1 d2;
  or_ e0 e1 e2 *)

(* and base_cat (i0, o0, d0, e0) (_, _, d1, e1) (_, _, d2, e2) i1 o1 = 
  i0 =?= i1; o0 =?= o1;
  and2_ d0 d1 d2;
  and2_ e0 e1 e2 *)

and bop (i0, o0, d0, e0) (i1, o1, d1, e1) (i2, o2, d2, e2) = 
  let v0, v1, v2 = Value.(uvar (), uvar (), uvar ()) in
  let c0, c1, c2 = mk_poly_costack (), mk_poly_costack (), mk_poly_costack () in
  i0 =?= c0; o0 =?= v0 @> c0;
  i1 =?= c1; o1 =?= v1 @> c1;
  i2 =?= c2; o2 =?= v2 @> c2;
  and2_ d0 d1 d2; and2_ e0 e1 e2;
  v0, v1, v2

and sect_left (i0, o0, d0, e0) (i2, o2, d2, e2) = 
  let v0, v1, v2 = Value.(uvar (), uvar (), uvar ()) in
  let c0, c2 = mk_poly_costack (), mk_poly_costack () in
  i0 =?= v1 @> c0; o0 =?= v0 @> c0;
  i2 =?= c2; o2 =?= v2 @> c0;
  Det.unify d0 d2; Det.unify e0 e2;
  v0, v1, v2

and sect_right (i0, o0, d0, e0) (i1, o1, d1, e1) = 
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
  set_true d0; set_true e0; 
  v0, v1, v2

and bop_cmp (i0, o0, d0, e0) (i1, o1, d1, e1) (i2, o2, d2, e2) = 
  let v1, v2 = Value.(uvar (), uvar ()) in
  let s0 = Stack.ufresh () in
  let c0, c1, c2 = s0 @>> mk_init_costack (), mk_poly_costack (), mk_poly_costack () in
  i0 =?= c0; o0 =?= s0 @>> c0;
  i1 =?= c1; o1 =?= v1 @> c1;
  i2 =?= c2; o2 =?= v2 @> c2;
  and2_ d0 d1 d2; and2_ e0 e1 e2;
  v1, v2

and sect_left_cmp (i0, o0, d0, e0) (i2, o2, d2, e2) = 
  let v1, v2 = Value.(uvar (), uvar ()) in
  let s0 = Stack.ufresh () in
  let c0, c2 = mk_poly_costack (), mk_poly_costack () in
  i0 =?= v1 @> c0; o0 =?= s0 @>> c0;
  i2 =?= c2; o2 =?= v2 @> c0;
  Det.unify d0 d2; Det.unify e0 e2;
  v1, v2

and sect_right_cmp (i0, o0, d0, e0) (i1, o1, d1, e1) = 
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
  set_true d0; set_true e0; 
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

and app hof (v0, v1, v2) = 
  let c0, c1 = Costack.(ufresh () , ufresh ()) in
  let d0, d1, d2 = and2_fresh () in
  let e0, e1, e2 = and2_fresh () in
  set_hof hof v0 (c0, c1, d0, e0);
  set_hof hof v1 (c0, c1, d1, e1);
  set_hof hof v2 (c0, c1, d2, e2)

let top_stmts ctx = 
  List.fold_left begin fun ctx' -> function
    | Def (d, None, (_, _, (i, o) as e)), _ -> 
      infer (insert d (false, (i, o)) ctx') e;
      insert d (true, (i, o)) ctx'
    | Def (d, Some ty, (_, _, (i, o as fn) as e)), _ -> 
      let elab_ty = Elab.ty_expr ty in
      Fn.unify fn elab_ty;
      let annotctx = insert d (true, (i, o)) ctx' in
      infer annotctx e;
      annotctx
  end ctx

let prog : (_stmt * Span.t) list -> 'a = 
  top_stmts empty
