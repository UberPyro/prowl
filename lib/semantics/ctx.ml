open! Batteries
open Printf
open Util
open System

type varcounter = (string, int ref) Map.t
let vc_init s vc = Map.add s (ref 0) vc
let vc_incr s vc = Map.find s vc |> incr

type t = {
  ctx : (string, bool * Fn.t) Ouro.t;
  uctx : (string, Value.t) Map.t;
  ucount : varcounter;
  udagcount : varcounter;
  sctx : (string, Stack.t) Map.t;
  scount : varcounter;
  sdagcount : varcounter;
  dats : (string, string list * (string * Fn.t) list) Map.t;
  recs : (string, string list * (string * Fn.t) list) Map.t;
}

let empty : t = {
  ctx = Ouro.empty;
  uctx = Map.empty; 
  ucount = Map.empty;
  udagcount = Map.empty;
  sctx = Map.empty;
  scount = Map.empty;
  sdagcount = Map.empty;
  dats = Map.empty;
  recs = Map.empty;
}

let map_ctx f t = {t with ctx = f t.ctx}
let map_uctx f t = {t with uctx = f t.uctx}
let map_sctx f t = {t with sctx = f t.sctx}
let map_ucount f t = {t with ucount = f t.ucount}
let map_udagcount f t = {t with udagcount = f t.udagcount}
let map_scount f t = {t with scount = f t.scount}
let map_sdagcount f t = {t with sdagcount = f t.sdagcount}
let map_dats f t = {t with dats = f t.dats}
let map_recs f t = {t with recs = f t.recs}

let find_rec_opt k t = Ouro.find_rec_opt k t.ctx
let insert_many lst t = map_ctx (Ouro.insert_many lst) t
let insert k v t = map_ctx (Ouro.insert k v) t
let vmap f t = map_ctx (Ouro.vmap f) t

let unify_uvar n u t = 
  try Value.unify u @@ Map.find n @@ t.uctx;
  vc_incr n t.ucount with Not_found -> 
    Unify.Ucommon.UnifError (UnboundUVar n) |> raise

let uincr_op n t = vc_incr n t.udagcount

let unify_ustack n u t = 
  try Stack.unify u @@ Map.find n @@ t.sctx;
  vc_incr n t.scount with Not_found -> 
    Unify.Ucommon.UnifError (UnboundSVar n) |> raise

let sincr n t = vc_incr n t.scount

let swap_uvar t = {
  t with
  ucount = t.udagcount;
  udagcount = t.ucount;
}

let swap_svar t = {
  t with
  scount = t.sdagcount;
  sdagcount = t.scount;
}

let introduce_uvar n u t = 
  map_ucount (vc_init n) t
  |> map_udagcount (vc_init n)
  |> map_uctx (Map.add n u)

let introduce_stkvar n u t = 
  map_scount (vc_init n) t
  |> map_sdagcount (vc_init n)
  |> map_sctx (Map.add n u)

let add_dat s d = map_dats @@ Map.add s d
let add_rec s r = map_recs @@ Map.add s r

let pretty_ouro out t = 
  let lst = Ouro.to_list t.ctx in
  fprintf out "Context: \n";
  lst |> List.iter @@ fun (n, (b, fn)) -> 
    let g = if b then "+" else "-" in
    fprintf out "%s %s => " g n;
    Fn.pretty out fn;
    fprintf out "%s" "\n"

let pretty_uctx out t = 
  let lst = Map.to_seq t.uctx |> List.of_seq in
  fprintf out "Unification Variables: \n";
  lst |> List.iter @@ fun (n, uvar) -> 
    fprintf out "%s -> " n;
    Value.pretty out uvar;
    fprintf out "%s" "\n"

let pretty_stkctx out t = 
  let lst = Map.to_seq t.sctx |> List.of_seq in
  fprintf out "Stack Variables: \n";
  lst |> List.iter @@ fun (n, uvar) -> 
    fprintf out "%s -> " n;
    Stack.pretty out uvar;
    fprintf out "%s" "\n"

(* let pretty_dats out t = 
  let lst = Map.to_seq t.dats |> List.of_seq in
  fprintf out "Datatypes: \n";
  lst |> List.iter @@ fun (n, (xs, fn)) -> 
    fprintf out "%s => " n;
    List.iter (fprintf out "%s ") xs;
    fprintf out " =>> ";
    Fn.pretty out fn;
    fprintf out "%s" "\n"

let pretty_recs out t = 
  let lst = Map.to_seq t.recs |> List.of_seq in
  fprintf out "Records: \n";
  lst |> List.iter @@ fun (n, (xs, fn)) -> 
    fprintf out "%s => " n;
    List.iter (fprintf out "%s ") xs;
    fprintf out " =>> ";
    Fn.pretty out fn;
    fprintf out "%s" "\n" *)

let pretty out t = 
  pretty_ouro out t;
  fprintf out "%s" "\n";
  pretty_uctx out t;
  fprintf out "%s" "\n";
  pretty_stkctx out t;
  (* fprintf out "%s" "\n";
  pretty_dats out t;
  fprintf out "%s" "\n";
  pretty_recs out t *)
