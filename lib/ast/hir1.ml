open! Batteries

type base_comb = [
  | `swap
  | `dup
  | `zap
  | `cat
  | `cons
  | `unit
  | `i
  | `dip
] [@@deriving show]

type expr = _expr * Span.t * unit [@@deriving show]
and _expr = [
  | `id of string
  | `var of Var.t
  | `stack of Var.t
  | `costack of Var.t
  | `jux of expr list
  | `dag of expr
  | `prime of expr

  | `int of int
  | `float of float
  | `char of char
  | `string of string
  | `quote of expr
  | `list of expr list
  | `block of expr * Mode.t
  | `tag of string

  | `binding of (string * Ast.ty option * expr) list * expr

  | base_comb
] [@@deriving show]

let rec wrap_expr (_e : _expr) : Ast.expr -> expr = 
  Tuple3.map (Fun.const _e) Fun.id (* Add type refresher -> *) Fun.id
and wrap2 (_e : _expr) (e1 : Ast.expr) (e2 : Ast.expr) = 
  Tuple3.map 
    (Fun.const _e) 
    (Fun.const (Span.join (Tuple3.second e1) (Tuple3.second e2))) 
    (* Add type refresher -> *) Fun.id e1
  
and wraps _es e : _expr = `jux (List.map (Fun.flip wrap_expr e) _es)
and wraps2 _es e1 e2 : _expr = `jux (List.map (fun x -> wrap2 x e1 e2) _es)

type def = _def * Span.t [@@deriving show]
and _def = [
  | `valdef of string * Ast.ty option * expr
  | `typedef of Ast.ty
  | `data of Ast.data
] [@@deriving show]

let rec expr ((_e, sp, t) : Ast.expr) : expr = begin match _e with
  | `unop (e, s) -> wraps [`quote (expr e); `id s; `i] e
  | `binop (e1, s, e2) -> 
    wraps2 [`quote (expr e1); `quote (expr e2); `id s; `i] e1 e2
  | `sectLeft (s, e) -> wraps [`unit; `quote (expr e); `id s; `i] e
  | `sectRight (e, s) -> wraps [`unit; `quote (expr e); `swap; `id s; `i] e
  | `sect s -> `id s
  | `arrow (e1, e2) -> `jux [wrap_expr (`dag (expr e1)) e1; expr e2]

  | `jux es -> `jux (List.map expr es)
  | `dag e -> `dag (expr e)
  | `prime e -> `prime (expr e)
  | `quote e -> `quote (expr e)
  | `list es -> `list (List.map expr es)
  | `block em -> `block (Tuple2.map1 expr em)
  | `binding (b, e) -> `binding (List.map (Tuple3.map3 expr) b, expr e)

  | `id _ | `float _| `stack _| `string _| `int _
  | `var _| `tag _| `char _| `costack _ as _e -> _e
end, sp, t

let def (_d, sp) = begin match _d with
  | `valdef vd -> `valdef (Tuple3.map3 expr vd)
  | `typedef _ | `data _ as _d -> _d
end, sp
