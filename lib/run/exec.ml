open! Batteries
open Uref
open Printf

open Parse
open Metadata

module LS = LazySet
module Code = Ast.Make(Span)

let pp_uref fmt x y = fmt x (uget y)

exception ProwlError of Span.t * string

type value = _value uref
and _value = 
  | Lit of lit
  | Exec
  | Free
  | Empty

and lit = 
  | Int of int
  | String of string
  | List of exec

and exec = 
  | Closure of Code.expr * context
  | Thunk of fn * fn

and stack = value list
and costack = stack * int

and coord = int * int
and fn = costack * coord -> costack LazySet.t * coord * coord
and context = (Code.expr, _value) Context.t
[@@deriving show]

let costack_bimap f g = function
  | (s, 0), coord -> f s coord
  | c, coord -> g c coord

let pop1 sp = function
  | h :: t -> t, h
  | [] -> raise @@ ProwlError (sp, sprintf "Unexpected nonunital stack")

let pop1_int sp = pop1 sp %> fun (t, x) -> x |> uget %> function
  | Lit Int i -> t, i
  | _ -> raise @@ ProwlError (sp, sprintf "Unexpected noninteger")

let pop2_gen f sp = 
  f sp %> fun (t, h1) -> 
    f sp t |> fun (t2, h2) -> t2, h2, h1

let pop2 sp = pop2_gen pop1 sp
let pop2_int = pop2_gen pop1_int
  
let get_stacks sp (f : fn) = 
  let cs, tot, dep = f (([], 0), (0, 0)) in
  cs |> LS.map_uniq begin function
    | s, 0 -> s
    | _ -> raise @@ ProwlError (sp, sprintf "Unexpected costack push")
  end, tot, dep

let get_values sp (f : fn) = 
  let ss, _, _ = get_stacks sp f in
  ss |> LS.map_uniq @@ function
    | [x] -> x
    | _ -> raise @@ ProwlError (sp, sprintf "Unexpected nonunital stack")

let apply_values vs = function
  | (s, 0), (ds, dc) -> 
    LS.map_uniq (fun v -> v :: s, 0) vs, (ds + 1, dc), (ds, dc)
  | c, (ds, dc) -> LS.pure c, (ds, dc), (ds, dc)

let lift_bin f vs1 vs2 = 
  LS.bind_uniq (fun v1 -> LS.map_uniq (fun v2 -> f v1 v2) vs2) vs1

let gather_inputs spl spr w op = 
  uget %> function
    | Lit Int i -> begin uget %> function
        | Lit Int j -> w (op i j)
        | _ -> raise @@ ProwlError (spr, sprintf "Unexpected noninteger")
      end
    | _ -> raise @@ ProwlError (spl, sprintf "Unexpected noninteger")

let aop_impl : Code.aop -> 'a = function
  | Add -> (+)
  | Sub -> (-)
  | Mul -> ( * )

let aop spl spr f (op : Code.aop) g : fn = 
  let vs1 = get_values spl f in
  let vs2 = get_values spr g in
  let vs_op = aop_impl op in
  let vs_f = gather_inputs spl spr (fun x -> uref @@ Lit (Int x)) vs_op in
  apply_values @@ lift_bin vs_f vs1 vs2

let gather_bind spl spr vs1 vs2 w op = 
  vs1 |> LS.bind_uniq @@ uget %> function 
      | Lit Int i -> 
        vs2 |> LS.map_uniq begin uget %> function 
          | Lit Int j -> w (op i j)
          | _ -> raise @@ ProwlError (spr, sprintf "Unexpected noninteger")
        end
      | _ -> raise @@ ProwlError (spl, sprintf "Unexpected Noninteger")

let cop_impl : Code.cop -> 'a = function
  | Eq -> (=)
  | Neq -> (<>)
  | Gt -> (>)
  | Lt -> (<)
  | Ge -> (>=)
  | Le -> (<=)

let cop spl spr f (op : Code.cop) g : fn = 
  let vs1 = get_values spl f in
  let vs2 = get_values spr g in
  let vs_op = cop_impl op in
  function
  | (s, 0), (ds, dc) -> 
    gather_bind spl spr vs1 vs2 (fun x -> s, 1 - Bool.to_int x) vs_op, 
    (ds, dc + 1), (ds, dc)
  | c, (ds, dc) -> LS.pure c, (ds, dc + 1), (ds, dc)

let gather_bind_unary sp vs f = 
  vs |> LS.map_uniq @@ uget %> function
    | Lit Int i -> f i
    | _ -> raise @@ ProwlError (sp, sprintf "Unexpected noninteger")

let aop_left sp (op : Code.aop) g : fn = 
  let vs = get_values sp g in
  let vs_op = aop_impl op in
  function
  | (s, 0), (ds, dc) -> 
    let s', j = pop1_int sp s in
    gather_bind_unary sp vs (fun x -> uref (Lit (Int (vs_op j x))) :: s', 0), 
    (ds, dc), (ds - 1, dc)
  | c, coord -> LS.pure c, coord, coord

let aop_right sp f (op : Code.aop) : fn = 
  let vs = get_values sp f in
  let vs_op = aop_impl op in
  function
  | (s, 0), (ds, dc) -> 
    let s', i = pop1_int sp s in
    gather_bind_unary sp vs (fun x -> uref (Lit (Int (vs_op x i))):: s', 0), 
    (ds, dc), (ds - 1, dc)
  | c, coord -> LS.pure c, coord, coord

let aop_sect sp (op : Code.aop) = 
  let vs_op = aop_impl op in
  function
  | (s, 0), (ds, dc) -> 
    let s', i, j = pop2_int sp s in
    LS.pure (uref (Lit (Int (vs_op i j))) :: s', 0), 
    (ds - 1, dc), (ds - 2, dc)
  | c, coord -> LS.pure c, coord, coord

let cop_left sp (op : Code.cop) g : fn = 
  let vs = get_values sp g in
  let vs_op = cop_impl op in
  function
  | (s, 0), (ds, dc) -> 
    let s', j = pop1_int sp s in
    gather_bind_unary sp vs (fun x -> s', vs_op x j |> Bool.to_int), 
    (ds - 1, dc + 1), (ds - 1, dc)
  | c, (ds, dc) -> LS.pure c, (ds, dc + 1), (ds, dc)

let cop_right sp f (op : Code.cop) : fn = 
  let vs = get_values sp f in
  let vs_op = cop_impl op in
  function
  | (s, 0), (ds, dc) -> 
    let s', i = pop1_int sp s in
    gather_bind_unary sp vs (fun x -> s', vs_op i x |> Bool.to_int), 
    (ds - 1, dc + 1), (ds - 1, dc)
  | c, (ds, dc) -> LS.pure c, (ds, dc + 1), (ds, dc)

let cop_sect sp (op : Code.cop) : fn = 
  let vs_op = cop_impl op in
  function
  | (s, 0), (ds, dc) -> 
    let s', i, j = pop2_int sp s in
    LS.pure (s', vs_op j i |> Bool.to_int), 
    (ds - 2, dc + 1), (ds - 2, dc)
  | c, (dc, ds) -> LS.pure c, (ds, dc + 1), (ds, dc)
