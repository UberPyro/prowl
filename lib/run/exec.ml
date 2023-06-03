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

let aop spl spr f (op : Code.aop) g : fn = 
  let vs1 = get_values spl f in
  let vs2 = get_values spr g in
  let vs_op = match op with
    | Add -> (+)
    | Sub -> (-)
    | Mul -> ( * ) in
  let vs_f = uget %> function
    | Lit Int i -> begin uget %> function
        | Lit Int j -> uref @@ Lit (Int (vs_op i j)) 
        | _ -> raise @@ ProwlError (spr, sprintf "Unexpected noninteger")
      end
    | _ -> raise @@ ProwlError (spl, sprintf "Unexpected noninteger") in
  apply_values @@ lift_bin vs_f vs1 vs2

(* let cop spl spr f (op : Code.cop) g = 
  let vs1 = get_values spl f in
  let vs2 = get_values spr g in
  let vs_op = match op with
    | Eq -> (=)
    | Neq -> (<>)
    | Gt -> (>)
    | Lt -> (<)
    | Ge -> (>=)
    | Le -> (<=) in *)
  