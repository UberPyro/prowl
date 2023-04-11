open! Batteries
open Uref

open Syntax
open Metadata
open LazySet

exception EmptyStack
exception Polycall
exception Noncallable
exception HigherOrderUnif
exception Disequal

let pp_uref fmt x y = fmt x (uget y)

type fn = costack -> costack LazyList.t [@@deriving show]

and callable = [
  | `closure of Mir.expr * context
  | `thunk of fn * fn
] [@deriving show]

and _value = [
  | Prim.lit
  | `closure of Mir.expr * context
  | `thunk of fn * fn
  | `closedList of callable list
  | `free
  | `empty
] [@@deriving show]

and context = (Mir.expr, _value) Context.t [@@deriving show]

and value = _value uref [@@deriving show]
and stack = value list [@@deriving show]

and costack = 
  | Real of stack
  | Fake of costack
  [@@derivings show]

let comap f = pure % function
  | Real s -> Real (f s)
  | c -> c

let cobind f = function
  | Real s -> f s
  | c -> pure c

let pop = function
  | h :: t -> t, h
  | [] -> raise @@ EmptyStack

let pop2 = function
  | h1 :: h2 :: t -> t, h2, h1
  | _ -> raise @@ EmptyStack

let push t h = h :: t
let push2 t h2 h1 = h1 :: h2 :: t

let rec expr ctx ((e_, _) : Mir.expr) i = match e_ with
  | `gen -> pure begin match i with
    | Real _ -> i
    | Fake _ -> Fake i
  end
  | `fab -> pure @@ Fake i
  | `elim -> pure begin match i with
    | Real _ -> i
    | Fake j -> j
  end
  | `exch -> pure begin match i with
    | Real _ -> Fake i
    | Fake j -> j
  end

  | `pick es -> 
    let rec go c es = match c, es with
      | Real _, e :: _ -> expr ctx e c
      | Fake c', _ :: es' -> go c' es'
      | _, [] -> pure c in
    go i es
  | `ponder es -> 
    let rec go c es = match c, es with
      | Real _, e :: _ -> expr ctx e c
      | Fake c', _ :: es' -> go c' es' |> LazyList.map (fun y -> Fake y)
      | _, [] -> pure c in
    go i es

  | `swap -> comap (pop2 %> fun (s, v2, v1) -> push2 s v1 v2) i
  | `unit -> comap (pop %> fun (s, v) -> plot s (lit_ref v) (colit_ref v)) i
  | `cat -> comap (pop2 %> fun (s, v2, v1) -> 
    plot s (call v2 >=> call v1) (cocall v1 >=> cocall v2)) i
  | `call -> cobind (pop %> fun (s, v) -> call v (Real s)) i
  | `zap -> comap (pop %> fst) i
  | `dup -> comap (pop %> fun (s, v) -> push2 s v v) i
  | `dip -> cobind (pop2 %> fun (s, v2, v1) -> 
    call v1 (Real s) >>= comap (fun s -> push s v2)) i
  
  | `fork es -> cobind (pop %> fun (s, v) -> 
      let rec go es c = match es with
        | e :: es -> go es c >>= comap (fun s -> push s v) >>= expr ctx e
        | [] -> pure c in
      go es (Real s)
    ) i

  | `par es -> cobind (fun s -> 
      let rec go xs_init es_init = match xs_init, es_init with
        | x :: xs, e :: es -> 
          go xs es >>= comap (fun s -> push s x) >>= expr ctx e
        | s', [] -> pure (Real s')
        | [], _ :: _ -> raise EmptyStack in
      go s es
    ) i
  
  | `eq -> query (=) i
  | `cmp -> cobind (pop2 %> fun (s, v2, v1) -> 
    pure @@ match[@warning "-8"] compare v2 v1 with
      | 1 -> Real s
      | 0 -> Fake (Real s)
      | -1 -> Fake (Fake (Real s))
    ) i
  
  | `add n -> iproc (fun x -> x + n) i
  | `subl n -> iproc (fun x -> x - n) i
  | `subr n -> iproc (fun x -> n - x) i
  | `mul n -> iproc (fun x -> x * n) i

  | `concat -> sbop (^) i

  | `mk -> begin match i with
    | Real s -> s |> pop2 %> begin fun (s', v2, v1) -> match uget v2 with
      | `closedList qs -> uref @@ `closedList (begin match uget v1 with
        | #callable as c -> c
        | _ -> `thunk (lit_ref v1, colit_ref v1)
      end :: qs) |> fun x -> Real (push s' x) |> pure
      | _ -> failwith "Type error: not a list"
    end
    | Fake (Real _ as c) -> lit (`closedList []) c
    | Fake (Fake _) as c -> pure c
  end

  | `parse -> parse i
  | `show -> show_int i

  | `int _ | `str _ as x -> lit x i
  | `quote e' -> lit (`closure (e', ctx)) i
  | `list es -> 
    lit (`closedList (List.map (fun e' -> `closure (e', ctx)) es)) i
  
  | `jux es -> List.fold_left (fun a x -> a >>= expr ctx x) (pure i) es
  | `dis (e1, e2) -> expr ctx e1 i <|> expr ctx e2 i
  | `mark e -> pure i <|> expr ctx e i
  | `plus e -> ~+(expr ctx e) i
  | `star e -> ~*(expr ctx e) i

  | `bind_var (bs, e) -> expr (Context.add_many bs ctx) e i
  | `id x -> 
    let e, ctx' = Context.find x ctx in
    expr ctx' e i
  | `ex (ss, e) -> expr (Context.init_many (uref `free) ss ctx) e i
  | `uvar u -> lit_ref (Context.return u ctx) i

  | `dag e -> expr_rev ctx e i

  | `debug -> 
    print_endline (show_costack i); 
    pure i

and value v = comap (fun s -> push s v)

and call v c = match uget v with
  | `closure (e', ctx') -> expr ctx' e' c
  | `thunk (f, _) -> f c
  | _ -> raise Noncallable

and cocall v c = match uget v with
  | `closure (e', ctx') -> expr_rev ctx' e' c
  | `thunk (_, f') -> f' c
  | _ -> raise Noncallable

and binop op = cobind (pop2 %> fun (s, v2, v1) -> 
  lit (op (uget v1) (uget v2)) (Real s))

and query op = cobind (pop2 %> fun (s, v2, v1) -> 
  pure @@ if op v2 v1 then Real s
  else Fake (Real s))

and ibop op = 
  binop @@ fun[@warning "-8"] (`int i1) (`int i2) -> `int (op i1 i2)

and unop op = cobind (pop %> fun (s, v) -> 
  lit (op (uget v)) (Real s))

and iproc op i = 
  i |> comap @@ pop %> fun (s, v) -> match uget v with
    | `int n -> uref @@ `int (op n) |> push s
    | _ -> failwith "Not an int"

and iuop op = unop @@ fun[@warning "-8"] (`int i) -> `int (op i)

and sbop op = 
  binop @@ fun[@warning "-8"] (`str s1) (`str s2) -> `str (op s1 s2)

and parse = comap (pop %> fun (s, st) -> 
  st |> uget |> function[@warning "-8"] `str x -> 
  push s (uref @@ `int (String.to_int x))
  | _ -> failwith "Can't parse - not a string")

and show_int = comap (pop %> fun (s, ix) -> 
  ix |> uget |> function[@warning "-8"] `int i -> 
  push s (uref @@ `str (Int.to_string i))
  | _ -> failwith "Can't show - not an int")

and lit _v = lit_ref (uref _v)
and colit _w = colit_ref (uref _w)

and lit_ref v = comap @@ fun s -> push s v
and colit_ref w = cobind @@ pop %> fun (s, v) -> 
  unify w v;
  match uget v with
  | `empty -> empty
  | _ -> pure @@ Real s

and plot s th coth = push s (uref @@ `thunk (th, coth))

and filter_real = LazyList.filter @@ function
  | Real _ -> true
  | Fake _ -> false

and disjoin = LazyList.map (fun y -> Fake y)

and expr_rev ctx ((e_, sp) : Mir.expr) i = match e_ with
  | `gen -> expr ctx (`elim, sp) i
  | `fab -> begin match i with
    | Real _ -> empty
    | Fake c -> pure c
  end
  | `elim -> expr ctx (`gen, sp) i
  | `exch -> expr ctx (`exch, sp) i

  | `pick es -> i |> cobind @@ fun s -> 
    List.fold_right
      (fun e a -> filter_real @@ expr_rev ctx e (Real s) <|> disjoin a)
      es empty
  | `ponder es -> 
    let rec go c es = match c, es with
      | Real _, e :: _ -> expr_rev ctx e c |> filter_real
      | Fake c', _ :: es' -> go c' es' |> disjoin
      | _, [] -> pure c in
    go i es

  | `swap -> expr ctx (`swap, sp) i
  | `unit -> cobind (pop %> fun (s, v) -> match uget v with
    | `thunk (f, _) -> f (Real s)
    | `closure _ -> empty
    | _ -> raise Noncallable) i
  | `cat | `call -> raise HigherOrderUnif
  | `zap -> lit `free i
  | `dup -> cobind (pop %> fun (s, v) -> lit_ref v (Real s)) i
  | `dip -> cobind (pop2 %> fun (s, v2, v1) -> 
    cocall v1 (Real s) >>= comap (fun s -> push s v2)) i
  
  | `fork es' -> 
    let rec go c v = function
      | e :: es -> 
        expr_rev ctx e c >>= colit_ref v >>= cobind @@ fun s -> 
          go (Real s) v es
      | [] -> pure c in
    go i (uref `free) es'
  | `par es' -> 
    let rec go c = function
      | e :: es -> 
        expr_rev ctx e c >>= cobind @@ pop %> fun (s, v) -> 
          go (Real s) es >>= lit_ref v
      | [] -> pure c in
    go i es'

  | `eq -> begin match i with
    | Real s -> 
      let v = uref `free in
      pure @@ Real (push2 s v v)
    | Fake (Real _) -> raise Disequal
    | Fake (Fake c) -> pure c
  end
  | `cmp -> begin match i with
    | Real _ -> raise Disequal
    | Fake (Real s) -> 
      let v = uref `free in
      pure @@ Real (push2 s v v)
    | Fake (Fake (Real _)) -> raise Disequal
    | Fake (Fake (Fake c)) -> pure c
  end

  | `add n -> iproc (fun x -> x - n) i
  | `subl n -> iproc (fun x -> x + n) i
  | `subr n -> iproc (fun x -> n - x) i
  | `mul n -> i |> cobind begin pop %> fun (s, v) -> match uget v with
    | `int m when m mod n = 0 -> 
      Real (uref @@ `int (m / n) |> push s) |> pure
    | `int _ -> empty
    | _ -> failwith "Not an int"
  end

  | `concat -> cobind (pop %> fun (s, v) -> match uget v with
    | `str x -> 
      LazyList.unfold (Deque.of_enum @@ String.enum x, []) @@ fun (d, l) -> 
        Option.map (fun (s1, r) -> Real (push2 s
          (uref @@ `str (String.of_enum @@ Deque.enum d))
          (uref @@ `str (String.of_list l))
        ), (s1, r :: l)) (Deque.rear d)
    | _ -> failwith "Not a string"
  ) i

  | `mk -> cobind (pop %> fun (s, v) -> pure @@ match uget v with
    | `closedList (#callable as h :: t) -> 
      Real (push2 s (uref (`closedList t)) (uref h))
    | `closedList [] -> Fake (Real s)
    | _ -> failwith "mk - not a list"
  ) i

  | `parse -> expr ctx (`show, sp) i
  | `show -> expr ctx (`parse, sp) i

  | `int _ | `str _ as x -> colit x i
  | `quote e' -> colit (`closure (e', ctx)) i
  | `list es -> 
    colit (`closedList (List.map (fun e' -> `closure (e', ctx)) es)) i

  | `jux es -> List.fold_right (fun e a -> a >=> expr_rev ctx e) es pure i
  | `dis (e1, e2) -> expr_rev ctx e1 i <|> expr_rev ctx e2 i
  | `mark e -> pure i <|> expr_rev ctx e i
  | `plus e -> expr_rev ctx e i >>= ~*(expr_rev ctx e)
  | `star e -> ~*(expr_rev ctx e) i

  | `bind_var (bs, e) -> expr_rev (Context.add_many bs ctx) e i
  | `id x -> 
      let e, ctx' = Context.find x ctx in
      expr_rev ctx' e i
  | `ex (ss, e) -> expr_rev (Context.init_many (uref `free) ss ctx) e i
  | `uvar u -> colit_ref (Context.return u ctx) i

  | `dag e -> expr ctx e i

  | `debug -> 
    print_endline (show_costack i); 
    pure i

and unify v1 v2 = unite ~sel v1 v2

and sel v1_ v2_ = match v1_, v2_ with
  | `empty, _ | _, `empty -> `empty
  | v1, v2 when v1 = v2 -> v1
  | `free, v | v, `free -> v
  | (#callable as q), (#callable as r) -> begin try
    (costack_map2 (List.map2 (fun u1 u2 -> 
      uref @@ sel (uget u1) (uget u2))))
      (call (uref q) (Real []))
      (call (uref r) (Real []))
    |> begin function
      | Real s -> 
        let f c = c |> comap @@ fun s' -> s @ s' in
        `thunk (f, rev_const_callable f)
      | _ -> failwith "internal sel"
    end
    with EmptyStack -> raise HigherOrderUnif
  end
  | _ -> `empty

and costack_map2 f c1 c2 = 
  let rec go c1 c2 = match c1, c2 with
    | Real s1, Real s2 -> Real (f s1 s2)
    | Fake c1', Fake c2' -> Fake (go c1' c2')
    | _ -> raise @@ Invalid_argument "costack_map2" in
  go (unpure c1) (unpure c2)

and unpure s = match LazyList.to_list s with
  | [c] -> c
  | _ -> failwith "Cannot transpose a quoted multifunction"

and rev_const_callable f i = try
  f @@ Real [] >>= cobind @@ fun temp_stack -> 
  List.fold (fun a x -> a >=> colit_ref x) pure temp_stack i
with EmptyStack -> raise HigherOrderUnif

let dm = Span.dummy

let init () = Ouro.insert_many ([
  "gen", `gen;
  "fab", `fab;
  "elim", `elim;
  "exch", `exch;
  "swap", `swap;
  "unit", `unit;
  "cat", `cat;
  "zap", `zap;
  "dup", `dup;
  "eq", `eq;
  "cmp", `cmp;
  "concat", `concat;
  "mk", `mk;
  "parse", `parse;
  "show", `show;
] |> List.map (Tuple2.map2 (fun x -> x, dm)))
  Ouro.empty |> Context.make
