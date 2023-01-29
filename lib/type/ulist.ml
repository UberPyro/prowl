open! Batteries
open Uref

let pp_uref f z y = f z (uget y)

exception Err

type ('a, 'b) t = ('a, 'b) _t uref
and ('a, 'b) _t = 
  | Push of ('a, 'b) t * 'a
  | Next of 'b
  | Bot [@@deriving show]

let rec unite_seq ~sel r = 
  r |> unite ~sel:begin curry @@ function
    | Next _ as s, Next _ -> s
    | Push _ as s, Next _ | Next _, (Push _ as s) -> s
    | Push (a, t) as s, Push (b, u) -> 
      sel t u;
      unite_seq ~sel a b;
      s
    | Push _, Bot | Bot, Push _ -> raise Err
    | Next _, Bot | Bot, Next _ -> Bot
    | Bot, Bot -> Bot
  end

let make init = 
  List.fold_left begin fun ulst x -> 
    uref @@ Push (ulst, x) 
  end @@ uref @@ Next init

let push us u = uref @@ Push (us, u)

let rec to_list ulst = match uget ulst with
  | Push (us, u) -> 
    let h, x = to_list us in
    u :: h, x
  | Next x -> [], Some x
  | Bot -> [], None

let rec iter f g ulst = match uget ulst with
  | Push (us, u) -> f u; iter f g us
  | Next x -> g x
  | Bot -> ()

let rec remap f g ulst = uref @@ match uget ulst with
  | Push (us, u) -> Push (remap f g us, f u)
  | Next x -> Next (g x)
  | Bot -> Bot

let map_top f g ulst = uref @@ match uget ulst with
  | Push (us, u) -> Push (us, f u)
  | Next x -> Next (g x)
  | Bot -> Bot
