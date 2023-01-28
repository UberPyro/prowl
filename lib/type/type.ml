open! Batteries
open Uref
open Ulist

module HT = Hashtbl.Make(struct
  include Hashtbl
  include Int
end)

type 'a twin = 'a * 'a [@@deriving show]

type 'a seq = ('a, int) Ulist.t [@@deriving show]
type var = _var uref
and _var = 
  | Nom of costack twin list * int
  | Var of int
  [@@deriving show]
and costack = var seq seq

let rec unify r = 
  r |> unite ~sel:begin curry @@ function
    | Nom (f0, n0) as n, Nom (f1, n1) 
      when n0 = n1 && List.(length f0 = length f1) -> 
      List.iter2 begin fun (i0, o0) (i1, o1) -> 
        unify_costack i0 o0;
        unify_costack i1 o1
      end f0 f1; 
      n
    | Var _ as v, _ | _, (Var _ as v) -> v

    | Nom (f0, n0), Nom (f1, n1) when List.(length f0 <> length f1) -> 
      failwith @@ Printf.sprintf
        "Cannot unify differently kinded types [%d] and [%d]" n0 n1
    | Nom (_, n0), Nom (_, n1) when n0 <> n1 -> failwith @@ Printf.sprintf
      "Cannot unify [%d] with [%d]" n0 n1  (* TODO: need to provide type names somehow *)
    | Nom _, Nom _ -> failwith "unreachable"
  end

and unify_costack r = 
  unite_seq ~sel:(unite_seq ~sel:unify) r

let c = ref (-1)
let fresh () = incr c; !c

let fresh_var () = uref @@ Var (fresh ())
let fresh_seq () = make (fresh ()) []

let unew f = uref % f % uget
let refresh () = 
  let ht = HT.create 8 in
  let find_memo rf = HT.find_option ht %> Option.default_delayed rf in
  let seq f = remap f (find_memo fresh) in object (self)
  method var = unew @@ function
    | Var i -> Var (find_memo fresh i)
    | Nom (lst, n) -> 
      Nom (List.map (fun (i, o) -> self#costack i, self#costack o) lst, n)
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

let connect (_, o1) (i2, _) = unify_costack o1 i2
let connect_in (i1, _) (i2, _) = unify_costack i1 i2
let connect_out (_, o1) (_, o2) = unify_costack o1 o2
