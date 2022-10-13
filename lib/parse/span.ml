type t = {
  start : loc; 
  fin : loc; 
}

and loc = {
  lin : int; 
  col : int
}

let span start_lin start_col end_lin end_col = {
  start = {
    lin = start_lin; 
    col = start_col; 
  }; 
  fin = {
    lin = end_lin; 
    col = end_col; 
  }; 
}

let cmp l1 l2 = 
  match compare l1.lin l2.lin with
  | 0 -> compare l1.col l2.col
  | n -> n

let max_loc l1 l2 = 
  if l2 > l1 then l2 else l1

let min_loc l1 l2 = 
  if l2 < l1 then l2 else l1

let join s1 s2 = {
  start = min_loc s1.start s2.start; 
  fin = max_loc s1.fin s2.fin; 
}

type span = t
