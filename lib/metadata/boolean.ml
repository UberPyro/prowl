open Batteries
open Set

type 'a variable = 
  | Flexible of int
  | Constant of 'a
  [@@deriving show]

type 'a t = 'a variable Set.t Set.t
and 'a boolean = 'a t

module PP = struct
  type 'a t = 'a variable list list [@@deriving show]
end

let pp_boolean fmt g = to_list %> List.map to_list %> PP.pp fmt g

open struct
  let (let*) x f = List.map f x |> List.flatten
  let (let+) x f = List.map f x
  let pure = List.singleton
end

exception BooleanUnificationError

let zero = singleton empty
let one = empty

let (+) a b = union (diff a b) (diff b a)
let inc x = x + one
let sum s = List.fold (+) one s

let ( * ) a b = sum @@
  let* a' = to_list a in
  let+ b' = to_list b in
  singleton @@ union a' b'

let product s = List.fold ( * ) zero s

let factor x e = 
  map (remove x) @@ filter (mem x) e, 
  filter (Fun.negate @@ mem x) e

let variable x = singleton @@ singleton @@ Flexible x
let constant x = singleton @@ singleton @@ Constant x
let bfresh () = variable (unique ())

let substitute (x, ex) e = 
  let apply = function
    | Flexible x' when x = x' -> ex
    | e -> singleton @@ singleton e in
  let mul e = product (List.map apply @@ to_list e) in
  sum (List.map mul @@ to_list e)

let free_variables e = Set.of_list @@
  let* e' = to_list e in
  let* e'' = to_list e' in
  match e'' with
  | Flexible x -> pure x
  | Constant _ -> []

let solveStep x e = 
  let t1, t2 = factor (Flexible x) e in
  inc t1 * t2, inc t1 * variable x + t2

let combine problem = inc @@ product (List.map inc problem)

let split x problem = 
  List.filter (mem x % free_variables) problem, 
  List.filter (Fun.negate @@ mem x % free_variables) problem

let sort_on f = List.sort_unique (fun x y -> Stdlib.compare (f x) (f y))

let rec solve_all_impl u v = match u, v with
  | _, [] -> []
  | [], _ -> raise BooleanUnificationError
  | xs, problem -> 
    let x, (simple, problem') = 
      List.hd @@ sort_on (List.length % fst % snd) @@
        let+ x = xs in
        x, split x problem in
    let simple', answer = solveStep x (combine simple) in
    let y = solve_all (List.filter ((<>) x) xs) (simple' :: problem') in
    (x, answer) :: y

and solve_all xs problem = 
  solve_all_impl xs (List.filter (not % is_empty) problem)

let rec rename_answers = function
  | [] -> []
  | (x, e) :: th -> 
    (x, substitute (x, bfresh ()) e) :: rename_answers th

let rec back_substitute = function
  | [] -> []
  | (x, e) :: th -> 
    let th' = back_substitute th in
    (x, List.fold_right substitute th' e) :: th'

let unify e1 e2 = 
  let e = [e1 + e2] in
  let lst = to_list @@ List.fold union empty @@ List.map free_variables e in
  let th = solve_all lst e in
  back_substitute @@ rename_answers th
