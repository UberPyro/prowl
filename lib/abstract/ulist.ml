open! Batteries
open Uref

type ('a, 'b) t = ('a, 'b) _t uref
and ('a, 'b) _t = 
  | Push of ('a, 'b) t * 'a
  | Empty of 'b

let rec unite_seq ~sel r = 
  r |> unite ~sel:begin curry @@ function
    | Empty _ as s, Empty _ -> s
    | Push _ as s, Empty _ | Empty _, (Push _ as s) -> s
    | Push (a, t) as s, Push (b, u) -> 
      sel t u;
      unite_seq ~sel a b;
      s
  end

let make init = 
  List.fold_left begin fun ulst x -> 
    uref @@ Push (ulst, x) 
  end @@ uref @@ Empty init

let rec to_list ulst = match uget ulst with
  | Push (us, u) -> 
    let h, x = to_list us in
    u :: h, x
  | Empty x -> [], x

let rec iter f g ulst = match uget ulst with
  | Push (us, u) -> f u; iter f g us
  | Empty x -> g x
