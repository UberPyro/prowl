open! Batteries
open! Uref
open Printf

open Metadata
open Syntax
open Ast
open System
open Memo
open Nullify

open Util
open Ull

exception InferError of
    Span.t
  * (string, bool * costack * costack) Ouro.t
  * value Dict.t
  * string

let rec infer ctx uctx (ast, sp, (i0, o0)) = try match ast with
  | Bop ((_, sp1, (i1, o1) as left), Aop _, (_, sp2, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    let u = mk_init_costack () in
    let z = Lit Int @> u in
    begin try
      i1 =?= u;
      o1 =?= z
    with UnifError msg -> InferError (sp1, ctx, uctx, msg) |> raise end;
    begin try
      i2 =?= u;
      o2 =?= z
    with UnifError msg -> InferError (sp2, ctx, uctx, msg) |> raise end;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= Lit Int @> p
  
  | Bop ((_, sp1, (i1, o1) as left), Cop _, (_, sp2, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    let u = mk_init_costack () in
    let z = Lit Int @> u in
    begin try
      i1 =?= u;
      o1 =?= z;
    with UnifError msg -> InferError (sp1, ctx, uctx, msg) |> raise end;
    begin try
      i2 =?= u;
      o2 =?= z;
    with UnifError msg -> InferError (sp2, ctx, uctx, msg) |> raise end;
    let s = ufresh () in
    let p = s @>> ufresh () in
    i0 =?= p;
    o0 =?= s @>> p
  
  | SectLeft (Aop _, (_, sp1, (i1, o1) as just)) -> 
    infer ctx uctx just;
    let u = mk_init_costack () in
    begin try
      i1 =?= u;
      o1 =?= Lit Int @> u;
    with UnifError msg -> InferError (sp1, ctx, uctx, msg) |> raise end;
    let p = Lit Int @> mk_poly_costack () in
    i0 =?= p;
    o0 =?= p
  
  | SectLeft (Cop _, (_, sp1, (i1, o1) as just)) -> 
    infer ctx uctx just;
    let u = mk_init_costack () in
    begin try
      i1 =?= u;
      o1 =?= Lit Int @> u;
    with UnifError msg -> InferError (sp1, ctx, uctx, msg) |> raise end;
    let s = ufresh () in
    let p = s @>> ufresh () in
    i0 =?= Lit Int @> p;
    o0 =?= s @>> p
  
  | SectRight ((_, sp1, (i1, o1) as just), Aop _) -> 
    infer ctx uctx just;
    let u = mk_init_costack () in
    begin try
      i1 =?= u;
      o1 =?= Lit Int @> u;
    with UnifError msg -> InferError (sp1, ctx, uctx, msg) |> raise end;
    let p = Lit Int @> mk_poly_costack () in
    i0 =?= p;
    o0 =?= p
  
  | SectRight ((_, sp1, (i1, o1) as just), Cop _) -> 
    infer ctx uctx just;
    let u = mk_init_costack () in
    begin try
      i1 =?= u;
      o1 =?= Lit Int @> u;
    with UnifError msg -> InferError (sp1, ctx, uctx, msg) |> raise end;
    let s = ufresh () in
    let p = s @>> ufresh () in
    i0 =?= Lit Int @> p;
    o0 =?= s @>> p
  
  | Sect Aop _ -> 
    let p = Lit Int @> mk_poly_costack () in
    i0 =?= Lit Int @> p;
    o0 =?= p
  
  | Sect Cop _ -> 
    let s = ufresh () in
    let p = s @>> ufresh () in
    i0 =?= Lit Int @> Lit Int @> p;
    o0 =?= s @>> p
  
  | Uop ((_, _, (i1, o1) as just), Dag) -> 
    infer ctx uctx just;
    o0 =?= i1;
    i0 =?= o1
  
  | Uop ((_, _, (i1, o1) as just), (Mark | Star | Plus)) -> 
    infer ctx uctx just;
    i1 =?= o1;
    i0 =?= i1;
    o0 =?= o1
  
  | Uop ((_, _, (i1, o1) as just), Loop) -> 
    infer ctx uctx just;
    let c0, c1 = ufresh (), ufresh () in
    let s0, s1 = ufresh (),ufresh () in
    i1 =?= s0 @>> c1;
    o1 =?= s0 @>> s1 @>> c1;
    i0 =?= s0 @>> c0;
    o0 =?= s1 @>> c0
  
  | Dop ((_, _, (i1, o1) as left), Ponder, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    i0 =?= rebase i1 i2;
    o0 =?= rebase o1 o2
  
  | Dop ((_, _, (i1, o1) as left), Pick, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    i0 =?= rebase i1 i2;
    o0 =?= o1;
    o0 =?= o2
  
  | Dop ((_, _, (i1, o1) as left), Guess, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    i0 =?= i1;
    i0 =?= i2;
    o0 =?= rebase o1 o2
  
  | Dop ((_, _, (i1, o1) as left), Tensor, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    i1 =?= mk_poly_costack ();
    o1 =?= mk_poly_costack ();
    i2 =?= mk_end_costack ();
    o2 =?= mk_end_costack ();
    let s0, c0 = upop i1 in
    i0 =?= rebase s0 (upop i2 |> fst) @>> c0;
    let s1, c1 = upop o1 in
    o0 =?= rebase s1 (upop o2 |> fst) @>> c1
  
  | Dop ((_, _, (i1, o1) as left), Fork, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    o1 =?= mk_poly_costack ();
    o2 =?= mk_poly_costack ();
    i0 =?= i1;
    i0 =?= i2;
    let s0, c0 = upop o1 in
    o0 =?= rebase s0 (upop o2 |> fst) @>> c0
  
  | Dop ((_, _, (i1, o1) as left), Cross, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    i1 =?= mk_poly_costack ();
    i2 =?= mk_end_costack ();
    let s0, c0 = upop i1 in
    i0 =?= rebase s0 (upop i2 |> fst) @>> c0;
    o0 =?= o1;
    o0 =?= o2
  
  | Dop ((_, _, (i1, o1) as left), Jux, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    i0 =?= i1;
    o1 =?= i2;
    o0 =?= o2
  
  | Dop ((_, _, (i1, o1) as left), Contra, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    i0 =?= i2;
    o2 =?= i1;
    o1 =?= o0
  
  | Dop ((_, _, (i1, o1) as left), Union, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    i0 =?= i1;
    i0 =?= i2;
    o0 =?= o1;
    o0 =?= o2
  
  | Nop (Gen | Fab) -> 
    let stunted, stack = ufresh (), ufresh () in
    let costack = stack @>> stunted in
    i0 =?= costack;
    o0 =?= stack @>> costack
  
  | Nop Exch -> 
    let stunted, s1, s2 = ufresh (), ufresh (), ufresh () in
    i0 =?= s1 @>> s2 @>> stunted;
    o0 =?= s2 @>> s1 @>> stunted
  
  | Nop Elim -> 
    let stunted, stack = ufresh () , ufresh () in
    let costack = stack @>> stunted in
    i0 =?= stack @>> costack;
    o0 =?= costack
  
  | Nop Cmp -> 
    let stunted, stack = ufresh () , ufresh () in
    let costack = stack @>> stunted in
    i0 =?= Lit Int @> Lit Int @> costack;
    o0 =?= stack @>> costack
  
  | Nop Dup -> 
    let var = mk_var () in
    let costack = var @@> mk_poly_costack () in
    i0 =?= costack;
    o0 =?= var @@> costack
  
  | Nop Surf -> 
    let c0 = ufresh () in
    let s1, s2, s3 = Tuple3.mapn ufresh ((), (), ()) in
    i0 =?= s1 @>> s2 @>> s3 @>> c0;
    o0 =?= s3 @>> s1 @>> s2 @>> c0
  
  | Nop Zap -> 
    let costack = mk_poly_costack () in
    i0 =?= mk_var () @@> costack;
    o0 =?= costack
  
  | Nop Swap -> 
    let costack, v1, v2 = mk_poly_costack (), mk_var (), mk_var () in
    i0 =?= v1 @@> v2 @@> costack;
    o0 =?= v2 @@> v1 @@> costack
  
  | Nop Cons -> 
    let c0, c1, c2 = Tuple3.mapn ufresh ((), (), ()) in
    let v = mk_var () in
    i0 =?= Con ((v @@> c1, c2), Quote) @> v @@> c0;
    o0 =?= Con ((c1, c2), Quote) @> c0
  
  | Nop Dip -> 
    let c0, c1, v = ufresh (), ufresh (), mk_var () in
    i0 =?= Con ((c0, c1), Quote) @> v @@> c0;
    o0 =?= v @@> c1

  | Nop Cat -> 
    let c0 = mk_poly_costack () in
    let c1, c2, c3 = Tuple3.mapn ufresh ((), (), ()) in
    i0 =?= Con ((c2, c3), Quote) @> Con ((c1, c2), Quote) @> c0;
    o0 =?= Con ((c1, c2), Quote) @> c0
  
  | Nop Unit -> 
    let c0, c1 = mk_poly_costack (), mk_poly_costack () in
    let v = mk_var () in
    i0 =?= v @@> c0;
    o0 =?= Con ((c1, v @@> c1), Quote) @> c0
  
  | Nop Dig -> 
    let c0 = mk_poly_costack () in
    let v1, v2, v3 = Tuple3.mapn mk_var ((), (), ()) in
    i0 =?= v1 @@> v2 @@> v3 @@> c0;
    o0 =?= v3 @@> v1 @@> v2 @@> c0
  
  | Nop DivMod -> 
    let c1 = Lit Int @> Lit Int @> mk_poly_costack () in
    i0 =?= c1;
    o0 =?= c1
  
  | Nop Lin -> 
    let c1, c2 = mk_poly_costack (), mk_poly_costack () in
    let s = ufresh () in
    let c0 = s @>> ufresh () in
    i0 =?= Con ((c1, c2), Quote) @> Con ((c1, c2), List) @> s @>> c0;
    o0 =?= Con ((c1, c2), List) @> c0
  
  | Nop Parse -> 
    let c0 = mk_poly_costack () in
    i0 =?= Lit String @> c0;
    o0 =?= Lit Int @> c0
  
  | Nop Show -> 
    let c0 = mk_poly_costack () in
    i0 =?= Lit Int @> c0;
    o0 =?= Lit String @> c0
  
  | Nop Noop -> i0 =?= o0

  | Nop (Id | Ab) -> 
    let c0 = mk_var () @@> mk_poly_costack () in
    i0 =?= c0;
    o0 =?= c0
  
  | Lit Int _ -> 
    let c0 = mk_poly_costack () in
    i0 =?= c0;
    o0 =?= Lit Int @> c0
  
  | Lit String _ -> 
    let c0 = mk_poly_costack () in
    i0 =?= c0;
    o0 =?= Lit String @> c0
  
  | Lit Quote (_, _, fn as just) -> 
    infer ctx uctx just;
    let c0 = mk_poly_costack () in
    i0 =?= c0;
    o0 =?= Con (fn, Quote) @> c0
  
  | Lit List lst -> 
    let fn0 = mk_poly_costack (), mk_poly_costack () in
    List.iter begin fun (_, _, fn as just) -> 
      infer ctx uctx just;
      unify_fn fn0 fn
    end lst;
    let c0 = mk_poly_costack () in
    i0 =?= c0;
    o0 =?= Con (fn0, List) @> c0
  
  | UVar s -> 
    let v = mk_var () in
    let c = mk_poly_costack () in
    i0 =?= v @@> c;
    o0 =?= c;
    Dict.find_option uctx s
    |> Option.default_delayed (fun () -> 
      let msg = sprintf "Cannot find unbound unification variable [%s]" s in
      UnifError msg |> raise)
    |> unify v
  
  | Ex (s, (_, _, (i1, o1) as just)) -> 
    infer ctx uctx just;
    i0 =?= i1;
    o0 =?= o1;
    Dict.add uctx s (mk_var ())
  
  | Var k -> 
    let (generalized, i1, o1), _ = 
      Ouro.find_rec_opt k ctx
      |> Option.default_delayed @@ fun () -> 
        let msg = sprintf "Cannot find unbound variable [%s]" k in
        UnifError msg |> raise in
    let cache = mk_memo () in
    let transform = 
      if generalized then freshen_costack cache
      else Fun.id in
    i0 =?= transform i1;
    o0 =?= transform o1;
  
  | Let (stmts, e) -> infer (stmts_rec ctx uctx stmts) uctx e

  with UnifError msg -> raise @@ InferError (sp, ctx, uctx, msg)

and stmts_rec ctx uctx stmts = 
  let ctx' = Ouro.insert_many begin stmts |> List.map @@ function
    | Def (s, None, (_, _, (i, o))), _ -> s, (false, i, o)
    | Def (d, Some ty, (_, _, (i, o as fn))), _ -> 
      unify_fn fn (Elab.ty_expr ty);
      d, (true, i, o)
  end ctx in
  List.iter begin function
    | Def (_, None, e), _ -> infer ctx' uctx e
    | Def (_, Some ty, (_, sp, fn as e)), _ -> 
      infer ctx' uctx e;
      let elab_ty = Elab.ty_expr ty in
      begin try unify_fn (Copy.fn fn) (nullify_fn elab_ty) with
        UnifError _ -> 
          let msg = sprintf
            "Annotation [%s] is more general than inferred type [%s]"
            (Pretty.Show.str_fn elab_ty)
            (Pretty.Show.str_fn fn) in
          InferError (sp, ctx, uctx, msg) |> raise
      end
  end stmts;
  ctx'

let top_stmts ctx uctx = 
  List.fold_left begin fun ctx' -> function
    | Def (d, None, (_, _, (i, o) as e)), _ -> 
      infer (Ouro.insert d (false, i, o) ctx') uctx e;
      Ouro.insert d (true, i, o) ctx'
    | Def (d, Some ty, (_, sp, (i, o as fn) as e)), _ -> 
      let elab_ty = Elab.ty_expr ty in
      let nulled = nullify_fn elab_ty in
      unify_fn fn elab_ty;
      let annotctx = Ouro.insert d (true, i, o) ctx' in
      infer annotctx uctx e;
      begin try unify_fn (Copy.fn fn) nulled with
        UnifError _ -> 
          let msg = sprintf
            "Annotation [%s] is more general than inferred type [%s]"
            (Pretty.Show.str_fn (Elab.ty_expr ty))
            (Pretty.Show.str_fn fn) in
          InferError (sp, ctx, uctx, msg) |> raise
      end;
      annotctx
  end ctx

let prog : (_stmt * Span.t) list -> 'a = 
  top_stmts Ouro.empty @@ Dict.create 32
