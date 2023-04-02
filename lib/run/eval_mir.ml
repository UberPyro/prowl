open! Batteries
open Uref

open Syntax

exception EmptyStack
exception Polycall
exception Noncallable

let pp_uref fmt x y = fmt x (uget y)

let c = ref (-1)
let (!!)() = 
  incr c;
  !c

module LazyList = struct
  include LazyList
  module PP = struct
    type 'a t = 'a list [@@deriving show]
  end
  let pp f fmt = to_list %> PP.pp f fmt

  let rec append_delayed l1 f l2 = match LazyList.get l1 with
    | None -> f l2
    | Some (h, t) -> lazy (LazyList.Cons (h, append_delayed t f l2))

  let rec bind f x = match LazyList.get x with
    | None -> x
    | Some (h, t) -> append_delayed (f h) (bind f) t

  let pure x = cons x nil
  let (>>=) x f = bind f x
  let (>=>) f g x = pure x >>= f >>= g
  let (<|>) l1 l2 = LazyList.unique (LazyList.append l1 l2)

  (* intersection - test *)
  let ( *> ) l1 l2 = 
    let rec go1 s l1' l2' = match LazyList.get l2' with
      | None -> LazyList.nil
      | Some (h, t) -> 
        if Set.mem h s then lazy (LazyList.Cons (h, go1 s l1' t))
        else let rec go2 s' l3 = match LazyList.get l3 with
          | Some (h', t') -> 
            if h' = h then lazy (LazyList.Cons (h, go1 s' t' t))
            else go2 (Set.add h' s') t'
          | None -> LazyList.filter (fun e -> Set.mem e s') t in
        go2 s l1' in
    go1 Set.empty l1 l2

  (* let star x f = 
    let go y = 
      let z = y >>= f in



  ;; *)

  (* let star x f = 
    let go s y = 
      y >>= f >>= fun z -> if Set.mem z s then 


  ;; *)

end

let (>>=) = LazyList.(>>=)
let (>=>) = LazyList.(>=>)
let (<|>) = LazyList.(<|>)
let pure = LazyList.pure
let empty = LazyList.nil

module Dict = struct
  include Map.Make(String)
  module D = struct
    type 'a t = (string * 'a) list [@@deriving show]
  end
  let pp h fmt = bindings %> D.pp h fmt
end

type value = [
  | Prim.lit
  | `closure of Mir.expr * context
  | `closedList of (Mir.expr * context) list
  | `thunk of costack -> costack LazyList.t
  | `free of int
] [@@deriving show]

and context = Mir.expr Dict.t

and stack = value list

and costack = 
  | Real of stack
  | Fake of costack

let comap f = pure % function
  | Real s -> Real (f s)
  | c -> c

let (let+) x f = comap f x

let cobind f = function
  | Real s -> f s
  | c -> pure c

let (let*) x f = cobind f x

let pop = function
  | h :: t -> t, h
  | [] -> raise @@ EmptyStack

let pop2 = function
  | h1 :: h2 :: t -> t, h2, h1
  | _ -> raise @@ EmptyStack

let push t h = h :: t
let push2 t h2 h1 = h1 :: h2 :: t

let rec expr _ctx ((e_, _) : Mir.expr) i = match e_ with
  | `gen -> pure begin match i with
    | Real _ -> i
    | Fake _ -> Fake i
  end
  | `fab -> pure @@ Fake i
  | `elim -> pure begin match i with
    | Real _ -> i
    | Fake j -> j
  end
  | `exch -> pure begin match i with
    | Real _ -> Fake i
    | Fake j -> j
  end

  | `swap -> comap (pop2 %> fun (s, v2, v1) -> push2 s v1 v2) i
  | `unit -> comap (pop %> fun (s, v) -> 
    push s (`thunk (comap (fun s -> push s v)))) i
  | `cat -> comap (pop2 %> fun (s, v2, v1) -> 
    push s (`thunk (call v2 >=> call v1))) i
  | `call -> cobind (pop %> fun (s, v) -> call v (Real s)) i
  | `zap -> comap (pop %> fst) i
  | `dup -> comap (pop %> fun (s, v) -> push2 s v v) i

  | `dis -> comap (pop2 %> fun (s, v2, v1) -> 
    push s (`thunk (fun c -> call v2 c <|> call v1 c))) i

  | _ -> failwith "todo"

and value v = comap (fun s -> push s v)

and call v c = match v with
  | `closure (e', ctx') -> expr ctx' e' c
  | `thunk f -> f c
  | _ -> raise Noncallable
