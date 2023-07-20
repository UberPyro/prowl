open! Batteries
open Printf

open Metadata
open Syntax
open Ast
open System

open Util
open Unify
open Ucommon

module V = Value
module S = Stack
module C = Costack

exception InferError of
    Span.t
  * (string, bool * Fn.t) Ouro.t
  * string

let push_int u = V.usyn "int" [] @> u
let push_str u = V.usyn "string" [] @> u
let push_quo c1 c2 u = V.usyn "quote" [V.uatom (c1, c2)] @> u
let push_list c1 c2 u = V.usyn "list" [V.uatom (c1, c2)] @> u

let rec infer ctx (ast, sp, (i0, o0)) = try match ast with
  | Bop ((_, sp1, (i1, o1) as left), Aop _, (_, sp2, (i2, o2) as right)) -> 
    let u = mk_init_costack () in
    unify_arg sp1 ctx left u i1 o1;
    unify_arg sp2 ctx right u i2 o2;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= push_int p
  
  | Bop ((_, sp1, (i1, o1) as left), Cop _, (_, sp2, (i2, o2) as right)) -> 
    let u = mk_init_costack () in
    unify_arg sp1 ctx left u i1 o1;
    unify_arg sp2 ctx right u i2 o2;
    let s = S.ufresh () in
    let p = s @>> C.ufresh () in
    i0 =?= p;
    o0 =?= s @>> p

  | Bop ((_, sp1, (i1, o1) as left), Lop Cat, (_, sp2, (i2, o2) as right)) -> 
    let u = mk_init_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    unify_ho_arg sp1 ctx left u c1 c2 push_quo i1 o1;
    unify_ho_arg sp2 ctx right u c2 c3 push_quo i2 o2;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= push_quo c1 c3 p
    
  | Bop ((_, sp1, (i1, o1) as left), Lop Ap, (_, sp2, (i2, o2) as right)) -> 
    let u = mk_init_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    unify_ho_arg sp1 ctx left u c1 c2 push_list i1 o1;
    unify_ho_arg sp2 ctx right u c2 c3 push_list i2 o2;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= push_list c1 c3 p
  
  | Bop ((_, sp1, (i1, o1) as left), Lop Alt, (_, sp2, (i2, o2) as right)) -> 
    let u = mk_init_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    unify_ho_arg sp1 ctx left u c1 c2 push_quo i1 o1;
    unify_ho_arg sp2 ctx right u c1 c2 push_quo i2 o2;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= push_quo c1 c2 p
    
  | Bop ((_, sp1, (i1, o1) as left), Lop (Append | Join), (_, sp2, (i2, o2) as right)) -> 
    let u = mk_init_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    unify_ho_arg sp1 ctx left u c1 c2 push_list i1 o1;
    unify_ho_arg sp2 ctx right u c1 c2 push_list i2 o2;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= push_list c1 c2 p
    
  | SectLeft (Aop _, (_, sp1, (i1, o1) as just)) -> 
    let u = mk_init_costack () in
    unify_arg sp1 ctx just u i1 o1;
    let p = push_int @@ mk_poly_costack () in
    i0 =?= p;
    o0 =?= p
  
  | SectLeft (Cop _, (_, sp1, (i1, o1) as just)) -> 
    let u = mk_init_costack () in
    unify_arg sp1 ctx just u i1 o1;
    let s = S.ufresh () in
    let p = s @>> C.ufresh () in
    i0 =?= push_int p;
    o0 =?= s @>> p
  
  | SectLeft (Lop Cat, (_, sp1, (i1, o1) as just)) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_quo i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_quo c1 c2 p;
    o0 =?= push_quo c1 c3 p
  
  | SectLeft (Lop Ap, (_, sp1, (i1, o1) as just)) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_list i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_list c1 c2 p;
    o0 =?= push_list c1 c3 p

  | SectLeft (Lop Alt, (_, sp1, (i1, o1) as just)) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_quo i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_quo c1 c2 p;
    o0 =?= push_quo c1 c2 p
    
  | SectLeft (Lop (Append | Join), (_, sp1, (i1, o1) as just)) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_list i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_list c1 c2 p;
    o0 =?= push_list c1 c2 p
    
  | SectRight ((_, sp1, (i1, o1) as just), Aop _) -> 
    infer ctx just;
    let u = mk_init_costack () in
    unify_arg sp1 ctx just u i1 o1;
    let p = push_int @@ mk_poly_costack () in
    i0 =?= p;
    o0 =?= p
  
  | SectRight ((_, sp1, (i1, o1) as just), Cop _) -> 
    infer ctx just;
    let u = mk_init_costack () in
    unify_arg sp1 ctx just u i1 o1;
    let s = S.ufresh () in
    let p = s @>> C.ufresh () in
    i0 =?= push_int p;
    o0 =?= s @>> p
  
  | SectRight ((_, sp1, (i1, o1) as just), Lop Cat) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_quo i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_quo c2 c3 p;
    o0 =?= push_quo c1 c3 p
  
  | SectRight ((_, sp1, (i1, o1) as just), Lop Ap) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2, c3 = Tuple3.mapn C.ufresh ((), (), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_list i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_list c2 c3 p;
    o0 =?= push_list c1 c3 p

  | SectRight ((_, sp1, (i1, o1) as just), Lop Alt) -> 
    infer ctx just;
    let u = mk_init_costack () in
    let c1, c2 = Tuple2.mapn C.ufresh ((), ()) in
    unify_ho_arg sp1 ctx just u c1 c2 push_quo i1 o1;
    let p = mk_poly_costack () in
    i0 =?= push_quo c1 c2 p;
    o0 =?= push_quo c1 c2 p;
    
  | SectRight ((_, sp1, (i1, o1) as just), Lop (Append | Join)) -> 
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
    
  | Uop ((_, _, (i1, o1) as just), Dag) -> 
    infer ctx just;
    o0 =?= i1;
    i0 =?= o1
  
  | Uop ((_, _, (i1, o1) as just), (Mark | Star | Plus)) -> 
    infer ctx just;
    i1 =?= o1;
    i0 =?= i1;
    o0 =?= o1
  
  | Dop ((_, _, (i1, o1) as left), Ponder, (_, _, (i2, o2) as right)) -> 
    infer ctx left;
    infer ctx right;
    i0 =?= C.rebase i1 i2;
    o0 =?= C.rebase o1 o2
  
  | Dop ((_, _, (i1, o1) as left), Pick, (_, _, (i2, o2) as right)) -> 
    infer ctx left;
    infer ctx right;
    i0 =?= C.rebase i1 i2;
    o0 =?= o1;
    o0 =?= o2
  
  | Dop ((_, _, (i1, o1) as left), Guess, (_, _, (i2, o2) as right)) -> 
    infer ctx left;
    infer ctx right;
    i0 =?= i1;
    i0 =?= i2;
    o0 =?= C.rebase o1 o2
  
  | Dop ((_, _, (i1, o1) as left), Tensor, (_, _, (i2, o2) as right)) -> 
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
  
  | Dop ((_, _, (i1, o1) as left), Fork, (_, _, (i2, o2) as right)) -> 
    infer ctx left;
    infer ctx right;
    o1 =?= mk_poly_costack ();
    o2 =?= mk_poly_costack ();
    i0 =?= i1;
    i0 =?= i2;
    let s0, c0 = C.upop o1 in
    o0 =?= S.rebase s0 (C.upop o2 |> fst) @>> c0
  
  | Dop ((_, _, (i1, o1) as left), Cross, (_, _, (i2, o2) as right)) -> 
    infer ctx left;
    infer ctx right;
    i1 =?= mk_poly_costack ();
    i2 =?= mk_end_costack ();
    let s0, c0 = C.upop i1 in
    i0 =?= S.rebase s0 (C.upop i2 |> fst) @>> c0;
    o0 =?= o1;
    o0 =?= o2
  
  | Dop ((_, _, (i1, o1) as left), Jux, (_, _, (i2, o2) as right)) -> 
    infer ctx left;
    infer ctx right;
    i0 =?= i1;
    o1 =?= i2;
    o0 =?= o2
  
  | Dop ((_, _, (i1, o1) as left), Union, (_, _, (i2, o2) as right)) -> 
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
    begin match Ouro.find_rec_opt s ctx with
    | None -> 
      let msg = 
        sprintf "Cannot find unbound unification variable [%s]" s in
      UnifError msg |> raise
    | Some ((_, (_, o1)), _) -> 
      let c = mk_poly_costack () in
      i0 =?= c;
      o0 =?= (C.upop o1 |> fst |> S.upop |> fst) @> c
    end
  
  | StackVar s -> 
    begin match Ouro.find_rec_opt s ctx with
    | None -> 
      let msg = 
        sprintf "Cannot find unbound stack variable [%s]" s in
      UnifError msg |> raise
    | Some ((_, (_, o1)), _) -> 
      let c = C.ufresh () in
      i0 =?= c;
      o0 =?= (C.upop o1 |> fst) @>> c
    end
  
  | Ex (s, (_, _, (i1, o1) as just), b) ->
    let v = mk_var () in
    let c = mk_poly_costack () in
    infer (Ouro.insert s (false, (c, v @> c)) ctx) just;
    if b then i0 =?= v @> i1
    else i0 =?= i1;
    o0 =?= o1
  
  | Each ((_, _, (i1, o1) as just), s, b) ->
    let z = S.ufresh () in
    let c = mk_poly_costack () in
    infer (Ouro.insert s (false, (c, z @>> c)) ctx) just;
    i0 =?= i1;
    if b then o0 =?= z @>> o1
    else o0 =?= o1
  
  | Var k -> 
    let (generalized, (i1, o1)), _ = 
      Ouro.find_rec_opt k ctx
      |> Option.default_delayed @@ fun () -> 
        let msg = sprintf "Cannot find unbound variable [%s]" k in
        UnifError msg |> raise in
    let i, o = 
      if generalized then begin
        Fn.refresh_memo ();
        Fn.generalize (Fn.memo ()) (i1, o1);
      end else i1, o1 in
    i0 =?= i;
    o0 =?= o
  
  | Let (stmts, e) -> infer (stmts_rec ctx stmts) e

  with UnifError msg -> raise @@ InferError (sp, ctx, msg)

and stmts_rec ctx stmts = 
  let ctx' = Ouro.insert_many begin stmts |> List.map @@ function
    | Def (s, None, (_, _, (i, o))), _ -> s, (false, (i, o))
    | Def (d, Some ty, (_, _, (i, o as fn))), _ -> 
      Fn.unify fn (Elab.ty_expr ty);
      d, (true, (i, o))
  end ctx in
  List.iter (fun (Def (_, _, e), _) -> infer ctx' e) stmts;
  ctx'

and unify_arg sp ctx ast u i o = 
  infer ctx ast;
  try i =?= u; o =?= push_int u
  with UnifError msg -> InferError (sp, ctx, msg) |> raise

and unify_ho_arg sp ctx ast u c1 c2 f i o = 
  infer ctx ast;
  begin try
    i =?= u;
    o =?= f c1 c2 u;
  with UnifError msg -> InferError (sp, ctx, msg) |> raise end
  

let top_stmts ctx = 
  List.fold_left begin fun ctx' -> function
    | Def (d, None, (_, _, (i, o) as e)), _ -> 
      infer (Ouro.insert d (false, (i, o)) ctx') e;
      Ouro.insert d (true, (i, o)) ctx'
    | Def (d, Some ty, (_, _, (i, o as fn) as e)), _ -> 
      let elab_ty = Elab.ty_expr ty in
      Fn.unify fn elab_ty;
      let annotctx = Ouro.insert d (true, (i, o)) ctx' in
      infer annotctx e;
      annotctx
  end ctx

let prog : (_stmt * Span.t) list -> 'a = 
  top_stmts Ouro.empty
