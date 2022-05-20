open Ast

let rec process_bin (p, l, r as con) = function
  | (_, [], _) -> failwith "Empty"
  | (0, h1 :: h2 :: t, 0) -> process_bin con (0, p h1 h2 :: t, 0)
  | (0, [x], 0) -> x
  | (i, lst, 0) -> r (process_bin con (i - 1, lst, 0))
  | (i, lst, j) -> l (process_bin con (i, lst, j - 1))

let proc_ebin = process_bin (
  (fun (_, loc as e1) e2 -> Pair (e1, e2), loc), 
  (fun (_, loc as e) -> Left e, loc), 
  (fun (_, loc as e) -> Right e, loc)
)

let proc_pbin = process_bin (
  (fun (_, loc as p1) p2 -> PPair (p1, p2), loc), 
  (fun (_, loc as p) -> PLeft p, loc), 
  (fun (_, loc as p) -> PRight p, loc)
)
