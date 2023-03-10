open! Batteries

open Meta
open Type

type 'a mk_expr = 'a mk__expr * Span.t * unit [@@deriving show]
and 'a mk__expr = [
  | Ast.variable
  | Ast.literal

  | `jux of 'a mk_expr list
  | `dag of 'a mk_expr
  | `prime of 'a mk_expr

  | `quote of 'a mk_expr
  | `list of 'a mk_expr list
  | `block of 'a mk_expr * Ast.mode

  | `bind_var of (string * 'a option * 'a mk_expr) list * 'a mk_expr
  | `bind_uvar of Var.t list * 'a mk_expr

  | `unop of 'a mk_expr * string
  | `binop of 'a mk_expr * string * 'a mk_expr
  | `sectLeft of string * 'a mk_expr
  | `sectRight of 'a mk_expr * string
  | `sect of string
  | `arrow of 'a mk_expr * 'a mk_expr
] [@@deriving show]

and 'a mk_def = 'a mk__def * Span.t [@@deriving show]
and 'a mk__def = [
  | `valdef of string * 'a option * 'a mk_expr
  | `typedef of 'a
  | `data of 'a mk_data
] [@@deriving show]

and 'a mk_data = 'a mk__data * Span.t [@@derivings show]
and 'a mk__data = Tag of 'a * string [@@deriving show]

type ty = _ty * Ast.mode * Span.t * Kind.t [@@deriving show]
and _ty = [
  | `id of string
  | `jux of ty list
  | `dag of ty
  | `quote of ty
] [@@deriving show]

let rec expr f = Tuple3.map1 (_expr f)

and _expr (f : 'a -> 'b) : 'a mk__expr -> 'b mk__expr = function
  | #Ast.variable | #Ast.literal | `sect _ as x -> x
  | `jux es -> `jux (List.map (expr f) es)
  | `dag e -> `dag (expr f e)
  | `prime e -> `prime (expr f e)
  | `quote e -> `quote (expr f e)
  | `list es -> `list (List.map (expr f) es)
  | `block (e, m) -> `block (expr f e, m)
  | `bind_var (bs, e) -> `bind_var (
    List.map (Tuple3.map Fun.id (Option.map f) (expr f)) bs, 
    expr f e
  )
  | `bind_uvar (us, e) -> `bind_uvar (us, expr f e)
  | `unop (e, s) -> `unop (expr f e, s)
  | `binop (e1, s, e2) -> `binop (expr f e1, s, expr f e2)
  | `sectLeft (s, e) -> `sectLeft (s, expr f e)
  | `sectRight (e, s) -> `sectRight (expr f e, s)
  | `arrow (e1, e2) -> `arrow (expr f e1, expr f e2)

let rec def f = Tuple2.map1 (_def f)
and _def (f : 'a -> 'b) : 'a mk__def -> 'b mk__def = function
  | `valdef (s, ty_opt, e) -> `valdef (s, Option.map f ty_opt, expr f e)
  | `typedef ty -> f ty
  | `data d -> `data (data f d)

and data f = Tuple2.map1 (_data f)
and _data (f : 'a -> 'b) (Tag (ty, s) : 'a mk__data) : 'b mk__data = 
  Tag (f ty, s)

let rec transpose (ty : Ast.ty) = ty |> Tuple4.map1 @@ function
  | `dag ty -> _transpose (Tuple4.first ty)
  | `id _ | #Ast.unification_variable | `quote _ | `list _ as _ty -> _ty
  | `jux es -> `jux (List.map transpose es)

(* "dag of ... is ..." *)
and _transpose : Ast._ty -> Ast._ty = function
  | `id _ | #Ast.unification_variable | `quote _ | `list _ as _ty -> _ty
  | `dag ty -> Tuple4.first ty
  | `jux tys -> `jux (List.rev tys)

(* todo: abstraction pass? *)
