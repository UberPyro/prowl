open Data

type input = costack
type output = costack LazySet.t
type fn = (input -> output) * int * int

let jux ((f, fs, fc) : fn) ((g, gs, gc) : fn) (c : input) = 
  c |> f |> LazySet.bind_uniq g, min fs gs, min fc gc

(* let ponder ((f, fs, fc) : fn) ((g, gs, gc) : fn) (c : input) = 
  let c' = f c in
  match nk (Costack.height c') c' with *)

(* let tensor ((f, fs, fc) : fn) ((g, gs, gc) : fn) (c : input) =  *)
