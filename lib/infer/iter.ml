open! Batteries

let iter_pairs f g h x = 
  let i = List.fold_left (fun h h' -> f h h'; h') h x in
  g h i
