open! Batteries
open Uref
open Ulist

module HT = Hashtbl.Make(struct
  include Hashtbl
  include Int
end)

(* let find_memo rf ht k = 
  match HT.find_option ht k with
  | Some v -> v
  | None -> rf () *)

type 'a twin = 'a * 'a [@@deriving show]

type 'a seq = ('a, int) Ulist.t [@@deriving show]
type var = _var uref
and _var = 
  | Nom of int
  | Var of int
  | Quo of costack twin
  [@@deriving show]
and costack = var seq seq

let rec unify r = 
  r |> unite ~sel:begin curry @@ function
    | Quo (i0, o0) as q, Quo (i1, o1) -> 
      unify_costack i0 o0;
      unify_costack i1 o1;
      q
    | u, v when u = v -> u
    | u, v -> failwith @@ Printf.sprintf
      "Cannot unify types [%s] and [%s]" (show__var u) (show__var v)
  end

and unify_costack r = 
  unite_seq ~sel:(unite_seq ~sel:unify) r

let c = ref (-1)
let fresh () = incr c; !c

let fresh_var () = Var (fresh ())
let fresh_seq () = make (fresh ()) []

let unew f = uref % f % uget
let refresh ht = 
  let find_memo rf = HT.find_option ht %> Option.default_delayed rf in
  let seq f = remap f (find_memo fresh) in object (self)
  method var = unew @@ function
    | Var i -> Var (find_memo fresh i)
    | Quo (i, o) -> Quo (self#costack i, self#costack o)
    | x -> x
  method costack = seq (seq self#var)
end

let lit l = 
  let c = fresh_seq () in
  let s = fresh_seq () in
  push c s, push c (push s l)

let bop m = 
  let c = fresh_seq () in
  let s = fresh_seq () in
  let s1 = push s m in
  push c (push s1 m), push c s1

let endo e = 
  let c = fresh_seq () in
  let s = fresh_seq () in
  push c (push s e) |> fun x -> x, x
