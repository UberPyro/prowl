open! Batteries
open Uref
open Ull
open Printf

type tlit = TInt | TString
and tcon = TQuote | TList | TTree

and v = v_contents uref
and v_contents = 
  | TLit of tlit
  | TCon of tcon * dc * dc
  | TMeta of int

and s = v Ull.t
and ds = s * s

and c = ds Ull.t
and dc = c * c
[@@deriving show]

let rec unify_dc (x1, x2) (y1, y2) = 
  unify_c x1 y1;
  unify_c x2 y2
and unify_c c1 = Ull.unite unify_ds occ_ds c1

and unify_ds (x1, x2) (y1, y2) = 
  unify_s x1 y1;
  unify_s x2 y2
and unify_s s1 = Ull.unite unify_v occ_v s1

and unify_v = Uref.unite ~sel: begin curry @@ function
    | (TMeta _, v) | (v, TMeta _) -> v
    | v1, v2 when v1 = v2 -> v1
    | v1, v2 -> 
      let msg = 
        sprintf
          "Cannot unify distinct type literals [%s] and [%s]"
          (show_v_contents v1) (show_v_contents v2) in
      raise @@ UnifError msg
  end

and occ_v i = uget %> function
  | TCon (_, (x1, y1), (x2, y2)) -> 
    List.iter (occ_c i) [x1; y1; x2; y2]
  | TMeta j when i = j -> 
    let msg = "Cannot unify a variable with a type that contains it" in
    raise @@ UnifError msg
  | _ -> ()

and occ_c i = Ull.occurs occ_ds i
and occ_ds i (s1, s2) = List.iter (occ_s i) [s1; s2]
and occ_s i = Ull.occurs occ_v i

let mk_ds () : ds = 
  let u = ufresh () in
  u, u
let mk_c () : c = usome (mk_ds ())
let mk_dc () : dc = 
  let c = mk_c () in
  c, c
let (<+) ((c1, c2) : dc) v : dc = 
  c1, map_hd (fun (s1, s2) -> s1, ucons v s2) c2
let (<:) dc v_ = dc <+ uref v_

let mk_dvoid () : dc = ufresh (), unil ()
let (<::) ((c1, c2) : dc) (ds : ds) = c1, ucons ds c2

let no_ds () : ds = 
  let u = Ull.unil () in
  u, u
let no_c () : c = ujust (no_ds ())
let no_dc () : dc = 
  let c = no_c () in
  c, c

let dup_dc (dcl, dcr) : dc = dcl, dup_hd dcr

let free_dc () : dc = Ull.(ufresh (), ufresh ())

let rec freshen_v memo v = match uget v with
  | TLit _ -> v
  | TCon (tc, dcl, dcr) -> 
    uref @@ TCon (tc, freshen_dc memo dcl, freshen_dc memo dcr)
  | TMeta i -> uref @@ TMeta (Gen.freshen memo i)

and freshen_s memo (s : s) : s = Ull.freshen memo freshen_v s
and freshen_ds memo = Tuple2.mapn (freshen_s memo)
and freshen_c memo c = Ull.freshen memo freshen_ds c
and freshen_dc memo = Tuple2.mapn (freshen_c memo)

let rec pretty_v out = uget %> function
  | TLit TInt -> fprintf out "z "
  | TLit TString -> fprintf out "S* "
  | TMeta i -> fprintf out "'%d" i
  | TCon (TQuote, l, r) -> 
    fprintf out "[";
    pretty_fn out l r;
    fprintf out "]";
  | TCon (TList, l, r) -> 
    fprintf out "{";
    pretty_fn out l r;
    fprintf out "}";
  | TCon (TTree, l, r) -> 
    fprintf out "<<";
    pretty_fn out l r;
    fprintf out ">>";

and pretty_fn out l r = 
    pretty_dc out l;
    fprintf out " -- ";
    pretty_dc out r;

and pretty_s out = uget %> function
  | UCons (u, us) -> 
    pretty_s out us;
    pretty_v out u;
  | USeq i -> fprintf out "%d " i
  | UNil -> fprintf out ". "

and pretty_ds out (l, r) = 
  fprintf out "( ";
  pretty_s out l;
  fprintf out " : ";
  pretty_s out r;
  fprintf out ") ";

and pretty_c out = uget %> function
  | UCons (u, us) -> 
    pretty_c out us;
    pretty_ds out u;
  | USeq i -> fprintf out "%d " i
  | UNil -> fprintf out ". "

and pretty_dc out (l, r) = 
  fprintf out "( ";
  pretty_c out l;
  fprintf out " : ";
  pretty_c out r;
  fprintf out ") "

let dc_to_string dc = 
  let out = IO.output_string () in
  pretty_dc out dc;
  IO.close_out out
