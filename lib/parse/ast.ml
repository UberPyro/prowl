open! Batteries
open Type

type value_type = _value_type * Span.t
and _value_type = 
  | TId of string
  | TVar of string
  | TQuote of relation
  | TList of relation

and stack_head = value_type list
and costack_head = (seq_tail * stack_head) list
and seq_tail = 
  | Has of int
  | Bottom

and relation = _relation * Span.t
and _relation = 
  | ImplStack of stack_head twin
  | ImplCostack of costack_head twin
  | Expl of (seq_tail * costack_head) twin

type expr = _expr * Span.t * (costack * costack)
and _expr = 
  | Id of string
  | Var of string
  | Seq of int
  | Coseq of int
  | Quote of expr
  | List of expr list

  | Int of int
  | Float of float
  | Char of char
  | String of string

  | Tag of string

  | Cat of expr
  | Binop of expr * string * expr
  | Unop of string * expr
  | LeftSect of string * expr
  | RightSect of expr * string
  | Sect of string

  | Let of (string * expr) list * expr
  | Arrow of expr * expr

type parameter = 
  | PVar of string
  | PQuote of int * int

type component = _component * Span.t
and _component = 
  | Def of string * expr
  | Spec of string * relation
  | Alias of string * parameter list * value_type list list
  | Tagspec of string * parameter list * value_type list list * string

type ('a, 'b, 'c) w = 'a -> (Span.t * costack * costack) -> 'b -> 'c
type ('a, 'b) base_expr = {
  id : ('a, string, 'b) w;
  var : ('a, string, 'b) w;
  seq : ('a, int, 'b) w;
  coseq : ('a, int, 'b) w;
  quote : ('a, 'a -> 'b, 'b) w;
  list : ('a, ('a -> 'b) list, 'b) w;

  int : ('a, int, 'b) w;
  float : ('a, float, 'b) w;
  char : ('a, char, 'b) w;
  string : ('a, string, 'b) w;

  tag : ('a, string, 'b) w;

  cat : ('a, 'a -> 'b, 'b) w;
  binop : ('a, ('a -> 'b) * string * ('a -> 'b), 'b) w;
  unop : ('a, string * ('a -> 'b), 'b) w;
  left_sect : ('a, string * ('a -> 'b), 'b) w;
  right_sect : ('a, ('a -> 'b) * string, 'b) w;
  sect : ('a, string, 'b) w;

  let_ : ('a, (string * ('a -> 'b)) list * ('a -> 'b), 'b) w;
  arrow : ('a, ('a -> 'b) * ('a -> 'b), 'b) w;
}

let rec fold_expr a b (e_, sp, (i0, o0)) = 
  let p = sp, i0, o0 in match e_ with
  | Id s -> b.id a p s
  | Var s -> b.var a p s
  | Seq i -> b.seq a p i
  | Coseq i -> b.coseq a p i
  | Quote e -> b.quote a p (fun a -> fold_expr a b e)
  | List es -> b.list a p (List.map (fun e a -> fold_expr a b e) es)

  | Int i -> b.int a p i
  | Float f -> b.float a p f
  | Char c -> b.char a p c
  | String s -> b.string a p s
  
  | Tag t -> b.tag a p t
  | Cat e -> b.cat a p (fun a -> fold_expr a b e)
  | Binop (e1, s, e2) -> 
    b.binop a p ((fun a -> fold_expr a b e1), s, (fun a -> fold_expr a b e2))
  | Unop (s, e) -> b.unop a p (s, fun a -> fold_expr a b e)
  | LeftSect (s, e) -> b.left_sect a p (s, fun a -> fold_expr a b e)
  | RightSect (e, s) -> b.right_sect a p ((fun a -> fold_expr a b e), s)
  | Sect s -> b.sect a p s
  
  | Let (ls, e) -> b.let_ a p (
      List.map (fun (s, e) -> s, fun a -> fold_expr a b e) ls,
      fun a -> fold_expr a b e
    )
  | Arrow (e1, e2) -> 
    b.arrow a p ((fun a -> fold_expr a b e1), fun a -> fold_expr a b e2)
