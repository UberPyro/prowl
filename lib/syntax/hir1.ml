open! Batteries

open Meta

type expr = _expr * Span.t [@@deriving show]
and _expr = [
  | `swap | `unit | `call | `zap
  | `gen | `fab | `elim

  | `eq | `neq | `lt | `le | `gt | `ge | `not
  | `add | `sub | `mul | `div | `neg
  | `concat
  | `mk

  | `int of int
  | `string of string
  | `id of string

  | `jux of expr list
  | `dag of expr

  | `quote of expr
  | `list of expr list

  | `bind_var of (string * expr) list * expr
] [@@deriving show]

let juxtapose e_s sp : _expr = `jux (List.map (fun e_ -> e_, sp) e_s)

let rec expr ((e_, sp) : Ast.expr) : expr = begin match e_ with
  | `id _ | `int _ | `string _ as e_ -> e_

  | `jux es -> `jux (List.map expr es)
  | `dag e -> `dag (expr e)

  | `quote e -> `quote (expr e)
  | `list es -> List.fold_left begin fun acc ((_, sp') as e') -> 
    juxtapose [`gen; acc; `quote (expr e'); `id "mk"] sp'
  end (juxtapose [`fab; `id "mk"] sp) es

  | `bind_var (bs, e) -> `bind_var (List.map (Tuple2.map2 expr) bs, expr e)

  | `arrow (e1, e2) -> `jux [`dag (expr e1), snd e1;  expr e2]

  | `sect s -> juxtapose [`swap; `unit; `swap; `unit; `id s; `call] sp
  | `binop (e1, s, e2) -> 
    juxtapose [`quote (expr e1); `quote (expr e2); `id s; `call] sp
  | `sectLeft (s, e) -> juxtapose [`unit; `quote (expr e); `id s; `call] sp
  | `sectRight (e, s) -> 
    juxtapose [`unit; `quote (expr e); `swap; `id s; `call] sp
end, sp
