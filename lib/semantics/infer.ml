open! Batteries
open! Uref
open Printf

open Syntax
open Ast
open System
open Memo

open Util
open Ull

module PP = struct
  type 'a t = (string * 'a) list [@@deriving show]
end

module Dict = struct
  include Hashtbl.Make(struct include Hashtbl include String end)
  let pp h fmt = to_list %> PP.pp h fmt
end

let inspect i o i_final o_final = 
  let arg, argcos = usplit i in
  let res, rescos = usplit o in
  match argcos, rescos with
  | None, _ | _, None -> failwith "Void sequence passed to dataflow"
  | Some ka, Some kr when ka = kr -> 
    ujoin i_final arg, ujoin o_final res
  | _ -> failwith "Insufficiently specific data passed to dataflow"

let inspect_biased disj conj disj_final = 
  let disj_list, disj_key = usplit disj in
  let _, conj_key = usplit conj in
  match disj_key, conj_key with
  | None, _ | _, None -> failwith "Void seq passed to dataflow [biased]"
  | Some kd, Some kc when kd = kc -> ujoin disj_final disj_list
  | _ -> failwith "Insufficiently specific data passed to dataflow [biased]"

let inspect_nested i o i_final o_final = 
  let arg, argcos = usplit i in
  let res, rescos = usplit o in
  match argcos, rescos with
  | None, _ | _, None -> failwith "Void sequence passed to dataflow"
  | Some ka, Some kr when ka = kr -> 
    map_hd (fun x -> ujoin x arg) i_final, 
    map_hd (fun x -> ujoin x res) o_final
  | _ -> failwith "Insufficiently specific data passed to dataflow"

let inspect_nested_biased disj conj disj_final = 
  let disj_list, disj_key = usplit disj in
  let _, conj_key = usplit conj in
  match disj_key, conj_key with
  | None, _ | _, None -> failwith "Void seq passed to dataflow [biased]"
  | Some kd, Some kc when kd = kc -> 
    map_hd (fun x -> ujoin x disj_list) disj_final
  | _ -> failwith "Insufficiently specific data passed to dataflow [biased]"

let rec infer ctx uctx (ast, _sp, (i0, o0)) = match ast with
  | Bop ((_, _, (i1, o1) as left), Aop _, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    let u = mk_unital_costack () in
    i1 =?= u;
    i2 =?= u;
    let z = Lit Int @> u in
    o1 =?= z;
    o2 =?= z;
    let p = mk_poly_costack () in
    i0 =?= p;
    o0 =?= Lit Int @> p
  
  | Bop ((_, _, (i1, o1) as left), Cop _, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    let u = mk_unital_costack () in
    i1 =?= u;
    i2 =?= u;
    let z = Lit Int @> u in
    o1 =?= z;
    o2 =?= z;
    let s = ufresh () in
    let p = s @>> ufresh () in
    i0 =?= p;
    o0 =?= s @>> p
  
  | SectLeft (Aop _, (_, _, (i1, o1) as just)) -> 
    infer ctx uctx just;
    let u = mk_unital_costack () in
    i1 =?= u;
    o1 =?= Lit Int @> u;
    let p = Lit Int @> mk_poly_costack () in
    i0 =?= p;
    o0 =?= p
  
  | SectLeft (Cop _, (_, _, (i1, o1) as just)) -> 
    infer ctx uctx just;
    let u = mk_unital_costack () in
    i1 =?= u;
    o1 =?= Lit Int @> u;
    let s = ufresh () in
    let p = s @>> ufresh () in
    i0 =?= Lit Int @> p;
    o0 =?= s @>> p
  
  | SectRight ((_, _, (i1, o1) as just), Aop _) -> 
    infer ctx uctx just;
    let u = mk_unital_costack () in
    i1 =?= u;
    o1 =?= Lit Int @> u;
    let p = Lit Int @> mk_poly_costack () in
    i0 =?= p;
    o0 =?= p
  
  | SectRight ((_, _, (i1, o1) as just), Cop _) -> 
    infer ctx uctx just;
    let u = mk_unital_costack () in
    i1 =?= u;
    o1 =?= Lit Int @> u;
    let s = ufresh () in
    let p = s @>> ufresh () in
    i0 =?= Lit Int @> p;
    o0 =?= s @>> p
  
  | Sect Aop _ -> 
    let p = Lit Int @> mk_poly_costack () in
    i0 =?= p;
    o0 =?= Lit Int @> p
  
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
  
  | Dop ((_, _, (i1, o1) as left), Ponder, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    let i, o = inspect i2 o2 i1 o1 in
    i0 =?= i;
    o0 =?= o
  
  | Dop ((_, _, (i1, o1) as left), Pick, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    let i = inspect_biased i2 o2 i1 in
    i0 =?= i;
    o0 =?= o1;
    o0 =?= o2
  
  | Dop ((_, _, (i1, o1) as left), Guess, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    let i = inspect_biased i2 o2 i1 in
    o0 =?= i;
    i0 =?= o1;
    i0 =?= o2
  
  | Dop ((_, _, (i1, o1) as left), Tensor, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    let i2s, o2s = ufresh (), ufresh () in
    i2 =?= i2s @>> unil ();
    o2 =?= o2s @>> unil ();
    let i, o = inspect_nested i2s o2s i1 o1 in
    i0 =?= i;
    o0 =?= o
  
  | Dop ((_, _, (i1, o1) as left), Fork, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    let i2s, o2s = ufresh (), ufresh () in
    i2 =?= i2s @>> unil ();
    o2 =?= o2s @>> unil ();
    let o = inspect_nested_biased i2s o2s o1 in
    i0 =?= i1;
    i0 =?= i2;
    o0 =?= o
  
  | Dop ((_, _, (i1, o1) as left), Cross, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    let i2s, o2s = ufresh (), ufresh () in
    i2 =?= i2s @>> unil ();
    o2 =?= o2s @>> unil ();
    let o = inspect_nested_biased i2s o2s o1 in
    o0 =?= i1;
    o0 =?= i2;
    i0 =?= o
  
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
      failwith "Unbound unification variable")
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
        sprintf "Cannot find unbound variable [%s]" k
        |> failwith in
    let cache = mk_memo () in
    let transform = 
      if generalized then freshen_costack cache
      else Fun.id in
    i0 =?= transform i1;
    o0 =?= transform o1;
  
  | Let (stmts, e) -> infer (stmts_rec false ctx uctx stmts) uctx e
  
and stmts_rec generalized ctx uctx stmts = 
  let unwrap (Def (s, (_, _, (i, o))), _) = s, (false, i, o) in
  let ctx' = Ouro.insert_many (List.map unwrap stmts) ctx in
  List.iter (fun (Def (_, e), _) -> infer ctx' uctx e) stmts;
  Ouro.vmap (fun (_, i, o) -> generalized, i, o) ctx'

let top_stmts ctx uctx = 
  List.fold_left begin fun ctx' (Def (d, (_, _, (i, o) as e)), _) -> 
    infer (Ouro.insert d (false, i, o) ctx') uctx e;
    Ouro.insert d (true, i, o) ctx'
  end ctx
