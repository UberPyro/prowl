(* Monadic context for execution *)

(* nondet * stack depth * costack depth *)
type t = Data.costack LazySet.t * int * int [@@deriving show]
(* use data heights to inform parallel and series operators *)

(* let make c s = LazySet.empty, c, s
let lone x c s = LazySet.pure x, c, s *)

(* let bind_min (f : Data.costack -> t) ((cs, stack_depth, costack_depth) : t) = 

  let stack_depth_min = ref 0 in
  let costack_depth_min = ref 0 in
  
  let f_cut c = 
    let cs_cut, stack_depth_cut, costack_depth_cut = f c in
    stack_depth_min := min !stack_depth_min stack_depth_cut;
    costack_depth_min := min !costack_depth_min costack_depth_cut;
    cs_cut in

  let x = LazySet.bind_uniq f_cut cs in
  x, !stack_depth_min, !costack_depth_min



;;

(* assume ctx already applied *)
let jux (f : Data.costack -> t) (g : Data.costack -> t) (c : Data.costack) = 
  let f_cs, f_stack_depth, f_costack_depth = f c in
  let g_cs, g_stack_depth, g_costack_depth = LazySet.bind_uniq g f_cs in *)
