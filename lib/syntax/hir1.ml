(* Desugaring and distribution *)
open! Batteries

open Meta

type stack_comb = [
  | `cat
  | `call
  | `dup
  | `swap
  | `cons
  | `dip
  | `unit
] [@@deriving show]

type costack_comb = [
  | `gen
  | `fab
  | `elim
  | `flip
] [@@deriving show]

type comb = [stack_comb | costack_comb] [@@deriving show]

type expr = _expr * Span.t [@@deriving show]
and _expr = [
  | Ast.variable
  | Ast.literal
  | comb

  | `jux of expr list
  | `dag of expr
  | `prime of expr

  | `quote of expr
  | `block of expr * Ast.monodet * Ast.monodet

  | `bind_var of (string * expr) list * expr
  | `bind_uvar of Var.t list * expr
] [@@deriving show]

let rec distribute ((e_, sp) : expr) : expr = begin match e_ with
  | `dag (`jux es, sp') -> `jux (List.rev_map (fun e -> distribute (`dag e, sp')) es)
  | `dag (`prime e, sp') -> `prime (distribute (`dag e, sp'))
  | `dag (`block (e, d1, d2), sp') -> `block (distribute (`dag e, sp'), d2, d1)
  | `dag (`bind_var (bs, e), sp') -> 
    `bind_var (List.map (Tuple2.map2 distribute) bs, distribute (`dag e, sp'))
  | `dag (`bind_uvar (vs, e), sp') -> `bind_uvar (vs, distribute (`dag e, sp'))
  | `dag ((#Ast.variable | #Ast.literal | #comb | `quote _), sp') -> `dag (e_, sp')
  | `dag (`dag e, _) -> fst (distribute e)

  | #Ast.variable | #Ast.literal | #comb -> e_
  | `jux es -> `jux (List.map distribute es)
  | `prime e -> `prime (distribute e)
  | `quote e -> `quote (distribute e)
  | `block (e, d1, d2) -> `block (distribute e, d1, d2)
  | `bind_var (bs, e) -> `bind_var (List.map (Tuple2.map2 distribute) bs, distribute e)
  | `bind_uvar (vs, e) -> `bind_uvar (vs, distribute e)
end, sp

let juxtapose e_s sp : _expr = `jux (List.map (fun e_ -> e_, sp) e_s)

let rec expr ((e_, sp) : Ast.expr) : expr = begin match e_ with
  | #Ast.variable | #Ast.literal as e_ -> e_

  | `jux es -> `jux (List.map expr es)
  | `dag e -> `dag (expr e)
  | `prime e -> `prime (expr e)

  | `quote e -> `quote (expr e)
  | `list es -> List.fold_left begin fun acc ((_, sp') as e') -> 
    juxtapose [`gen; acc; `quote (expr e'); `id "mk"] sp'
  end (juxtapose [`fab; `id "mk"] sp) es
  | `block (e, d1, d2) -> `block (expr e, d1, d2)

  | `bind_var (bs, e) -> `bind_var (List.map (Tuple2.map2 expr) bs, expr e)
  | `bind_uvar (vs, e) -> `bind_uvar (vs, expr e)

  | `arrow (e1, e2) -> `jux [`dag (expr e1), snd e1;  expr e2]

  | `sect s -> juxtapose [`swap; `unit; `swap; `unit; `id s; `call] sp
  | `binop (e1, s, e2) -> 
    juxtapose [`quote (expr e1); `quote (expr e2); `id s; `call] sp
  | `sectLeft (s, e) -> juxtapose [`unit; `quote (expr e); `id s; `call] sp
  | `sectRight (e, s) -> 
    juxtapose [`unit; `quote (expr e); `swap; `id s; `call] sp
  | `unop (e, s) -> juxtapose [`quote (expr e); `id s; `call] sp
end, sp

let transform = distribute % expr
