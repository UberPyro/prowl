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

let _lit (_v : _value) (((s, i), (dc, ds)) : costack * coord) = 
  if i = 0 then LS.pure (uref _v :: s, i), (dc, ds + 1), (dc, ds)
  else LS.pure (s, i), (dc, ds), (dc, ds)

let get_value sp h bias = match h (([], 0), (0, 0)) |> Tuple3.map1 LazySet.list with
  | [[v], 0], _, _ -> v
  | [x, 0], _, _ -> 
    raise (ProwlError (sp, 
      sprintf "%s bop arg was nonunital, pushed +%d" bias (List.length x)))
  | [_, c], _, _ -> 
    raise (ProwlError (sp, 
      sprintf "%s bop arg pushed to costack +%d" bias c))
  | s, _, _ -> 
    raise (ProwlError (sp, 
      sprintf "%s bop arg was nondet, mult=%d" bias (List.length s)))

let extract_int sp v = match uget v with
  | Lit Int i -> i
  | x -> raise @@ ProwlError (sp, 
    sprintf "Expected integer, got %s" (show__value x))

let aop sp (op : Code.aop) (f : fn) (g : fn)
  (((s, i), (dc, ds)) : costack * coord) = 

  if dc = 0 then

    let i_l = get_value sp f "Left" |> extract_int sp in
    let i_r = get_value sp g "Right" |> extract_int sp in
    let i_o = match op with
      | Add -> i_l + i_r
      | Sub -> i_l - i_r
      | Mul -> i_l * i_r in
    
    (uref (Lit (Int i_o)) :: s, i), (dc, ds + 1), (dc, ds)

  else (s, i), (dc, ds), (dc, ds)
  
(* let divmod sp (f : fn) (g : fn) 
  (((s, i), (dc, ds)) : costack * coord) = 

  if dc = 0 then 

    let i_l = get_value sp f "Left" |> extract_int sp in
    let i_r = get_value sp g "Right" |> extract_int sp in
    let i_div = i_l / i_r in
    let i_mod = 
      let x = i_l mod i_r in
      if x > 0 then x
      else x + i_r in
    
    (uref (Lit (Int i_div)) :: uref (Lit (Int i_mod)) :: s, i), 
    (dc, ds + 2), (dc, ds)

  else (s, i), (dc, ds), (dc, ds) *)


