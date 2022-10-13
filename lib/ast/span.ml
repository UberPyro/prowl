open Tree_sitter_run
open Loc

type t = Loc.t

let make_span start_row start_col end_row end_col : t = {
  start = {row=start_row; column = start_col}; 
  end_ = {row=end_row; column=end_col}; 
}

let cmp l1 l2 = 
  match compare l1.row l2.row with
  | 0 -> compare l1.column l2.column
  | n -> n

let max_loc l1 l2 = 
  if l2 > l1 then l2 else l1

let min_loc l1 l2 = 
  if l2 < l1 then l2 else l1

let join s1 s2 = {
  start = min_loc s1.start s2.start; 
  end_ = max_loc s1.end_ s2.end_; 
}

type span = t
