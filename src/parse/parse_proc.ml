let rec process_bin (p, l, r as con) = function
  | (_, [], _) -> failwith "Empty"
  | (0, h1 :: h2 :: t, 0) -> process_bin con (0, p h1 h2 :: t, 0)
  | (0, [x], 0) -> x
  | (i, lst, 0) -> l (process_bin con (i - 1, lst, 0))
  | (i, lst, j) -> r (process_bin con (i, lst, j - 1))
