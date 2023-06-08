open! Batteries
open Uref
open Ull

type tlit = TInt | TString
and tcon = TQuote | TList

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
        Printf.sprintf
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

let mk_ds () : ds = Ull.(ufresh (), ufresh ())
let mk_c () : c = usome (mk_ds ())
let mk_dc () : dc = mk_c (), mk_c ()
let (<+) ((c1, c2) : dc) v : dc = 
  c1, map_hd (fun (s1, s2) -> s1, ucons v s2) c2
let (<:) dc v_ = dc <+ uref v_

let no_ds () : ds = Ull.(unil (), unil ())
let no_c () : c = usome (no_ds ())
let no_dc () : dc = no_c (), no_c ()
