open! Batteries

open Metadata

type 'a core = [
  | `bind_var of (string * 'a) list * 'a
  | `jux of 'a list
  | `dis of 'a * 'a
  | `mark of 'a
  | `plus of 'a
  | `star of 'a
  | `quote of 'a
  | `list of 'a list
] [@@deriving show]

type 'a sugar = [
  | `binop of 'a * string * 'a
  | `sectLeft of string * 'a
  | `sectRight of 'a * string
  | `sect of string
  | `arrow of 'a * 'a
] [@@deriving show]

type expr = _expr * Span.t [@@deriving show]
and _expr = [
  | Prim.word
  | expr core
  | expr sugar
] [@@deriving show]

type desug = [Prim.word | Prim.op | desug Prim.dag | desug core] * Span.t

let rec desugar (#_expr, sp as x) : desug = x |> Tuple2.map1 @@ function
  | #Prim.word as word -> word

  | `bind_var (ls, e) -> 
    `bind_var (List.map (Tuple2.map2 desugar) ls, desugar e)
  | `jux es -> `jux (List.map desugar es)
  | `dis (e1, e2) -> `dis (desugar e1, desugar e2)
  | `mark e -> `mark (desugar e)
  | `plus e -> `plus (desugar e)
  | `star e -> `star (desugar e)
  | `quote e -> `quote (desugar e)
  | `list es -> `list (List.map desugar es)

  | `binop (e1, op, e2) -> 
    let e1, e2 = desugar e1, desugar e2 in
    `jux [`quote e1, snd e1; `quote e2, snd e2; `id op, sp; `call, sp]
  | `sectLeft (op, e) -> 
    let e = desugar e in
    `jux [`unit, sp; `quote e, snd e; `id op, sp; `call, sp]
  | `sectRight (e, op) -> 
    let e = desugar e in
    `jux [`unit, sp; `quote e, snd e; `swap, sp; `id op, sp; `call, sp]
  | `sect op -> 
    `jux [`swap, sp; `unit, sp; `swap, sp; `unit, sp; `id op, sp; `call, sp]
  | `arrow (e1, e2) -> 
    let e1, e2 = desugar e1, desugar e2 in
    `jux [`dag e1, snd e1; e2]
