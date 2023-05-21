(* Monadic context for execution *)

(* nondet * stack depth * costack depth *)
type t = Data.costack LazySet.t * int * int [@@deriving show]
(* use data heights to inform parallel and series operators *)

let make c s = LazySet.empty, c, s
let lone x c s = LazySet.pure x, c, s


