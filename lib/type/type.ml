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
  method stack = seq self#var
  method costack = seq self#stack
end

let lit l = 
  let c = fresh_seq () in
  let s = fresh_seq () in
  push c s, push c (push s l)

let bop m n o = 
  let c = fresh_seq () in
  let s = fresh_seq () in
  push c (push (push s m) n), push c (push s o)

let bop_endo m = bop m m m

let endo e = 
  let c = fresh_seq () in
  let s = fresh_seq () in
  push c (push s e) |> fun x -> x, x

let connect (_, o1) (i2, _) = unify_costack o1 i2
let connect_in (i1, _) (i2, _) = unify_costack i1 i2
let connect_out (_, o1) (_, o2) = unify_costack o1 o2
let connect_self (i, o) = unify_costack i o
let connect_parallel (i1, o1) (i2, o2) = 
    unify_costack i1 o1; 
    unify_costack i2 o2

let unconnected () = 
  let f () = push (fresh_seq ()) (push (fresh_seq ()) (fresh_var ())) in
  f (), f ()

let push_map c v = 
  map_top (fun s -> push s v) Fun.id c

let push_left (i, o) v = push_map i v, o
let push_right (i, o) v = i, push_map o v
let get_right_stack (_, o) = get_top o
let get_right io = io |> get_right_stack |> get_top

(* uids for primitives *)
let uid_int = fresh ()
let uid_float = fresh ()
let uid_char = fresh ()
let uid_string = fresh ()
let uid_quote = fresh ()
let uid_list = fresh ()

let nom_mono uid = uref @@ Nom ([], uid)
let nom_poly1 uid v = uref @@ Nom ([v], uid)

let nom_int = nom_mono uid_int
let nom_float = nom_mono uid_float
let nom_char = nom_mono uid_char
let nom_string = nom_mono uid_string
let nom_quote = nom_poly1 uid_quote
let nom_list = nom_poly1 uid_list
