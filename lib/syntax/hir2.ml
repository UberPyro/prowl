(* Escape analysis and binder insertion *)
open! Batteries

open Meta

type expr = _expr * Span.t [@@deriving show]
and _expr = [
  | Ast.variable
  | Ast.literal
  | Hir1.comb

  | `jux of expr list
  | `dag of expr
  | `prime of expr
  | `quote of expr
  | `block of expr * Ast.monodet * Ast.monodet
  | `bind_uvar of Var.t list * expr

  | `bind_var of int * (string * expr) list * expr
] [@@deriving show]

module Vocab = Map.Make(Var)

(* rewrite for better automatic binder insertion *)
module Ins2 = struct

  module Vocab = Set.Make(Var)

  type t = {
    unbound : Vocab.t; 
    unbound_converse : Vocab.t;
    bound : Vocab.t;
    expr : Hir1.expr;
  }

  let (let+) t f = {t with expr = Tuple2.map1 f t.expr}

  let rec expr_outer (r : t) : t = 
    let r' = expr_inner r in
    let+ _ = r in `bind_uvar (Vocab.elements r'.unbound, r'.expr)
  
  and expr_inner (r : t) : t = match fst r.expr with
    | #Ast.literal | #Hir1.comb | #Ast.lexical_variable -> r
    | `var _ | `stack _ | `costack _ as e_ -> expr_collect r e_ `direct
    | `dag ((`var _ | `stack _ | `costack _ as e_), _) -> 
      expr_collect r e_ `converse
    | `jux es -> 
      let r', es' = List.fold_left begin fun (r', es') e' -> 
        let y = expr_inner (let+ _ = r' in fst e') in
        y, y.expr :: es'
      end (r, []) es in 
      let+ _ = r' in `jux (List.rev es')
    | `prime e' -> 
      let+ x = expr_inner (let+ _ = r in fst e') in `prime (x, snd e')
    

    | _ -> failwith "Todo"

  
  and expr_collect r (`var v | `stack v | `costack v) = 
    let col = 
      if Vocab.mem v r.bound then Fun.id
      else Vocab.add v in function
    | `direct -> {r with unbound = col r.unbound}
    | `converse -> {r with unbound_converse = col r.unbound_converse}


end

module Ins = struct

  type t = Span.t Vocab.t * Span.t Vocab.t * Hir1.expr

  let (let+) t f = Tuple3.map3 f t

  let rec expr_outer (r : t) : t = 
    let unbound, _, ((_, sp) as e) = expr_inner r in
    let+ _ = r in `bind_uvar (List.of_enum (Vocab.keys unbound), e), sp
  
  and expr_inner (r : t) : t = 
    let unbound, bound, (e_, sp) = r in match e_ with
    | #Ast.literal | #Hir1.comb | #Ast.lexical_variable -> r
    | `var v | `stack v | `costack v -> r |> Tuple3.map1 @@
      if Vocab.mem v bound then Fun.id else Vocab.add v sp
    | `jux es -> 
    let+ x = List.fold_left begin fun (u, b, es') e' -> 
      let+ x = expr_inner (u, b, e') in x :: es'
    end (unbound, bound, []) es in `jux (List.rev x), sp
    | `dag e' -> let+ x = expr_inner (unbound, bound, e') in `dag x, sp
    | `prime e' -> let+ x = expr_inner (unbound, bound, e') in `prime x, sp
    | `quote e' -> let+ x = expr_outer (unbound, bound, e') in `quote x, sp
    | `block (e', d1, d2) -> 
      let+ x = expr_outer (unbound, bound, e') in `block (x, d1, d2), sp
    | `bind_var (bs, e_) -> 
      let bs' = List.fold_left begin fun (u, b, bs') (s, e') -> 
        let+ x = expr_outer (u, b, e') in (s, x) :: bs'
      end (unbound, bound, []) bs |> Tuple3.third |> List.rev in
      let+ x = expr_inner (unbound, bound, e_) in `bind_var (bs', x), sp
    | `bind_uvar (vs, e') -> 
      let b = List.fold_left (fun vc v -> Vocab.add v sp vc) bound vs in
      let+ x = expr_inner (unbound, b, e') in `bind_uvar (vs, x), sp
  
  let expr e = expr_outer Vocab.(empty, empty, e)
  
end

module Esc = struct

  type t = Span.t Vocab.t * Span.t Vocab.t * expr
  let (let+) = Ins.(let+)

  let rec expr (unbound, bound, ((e_, sp) : Hir1.expr) as r) : t = match e_ with
    | #Ast.literal | #Hir1.comb | #Ast.lexical_variable as e_' -> 
      let+ _ = r in e_', sp
    | `var v | `stack v | `costack v -> r |> Tuple3.map1 begin
      if Vocab.mem v bound then Fun.id else Vocab.add v sp
    end |> expr
    | `jux es -> 
      let+ x = List.fold_left begin fun (u, b, es') e' -> 
        let+ x = expr (u, b, e') in x :: es'
      end (unbound, bound, []) es in `jux (List.rev x), sp
    | `dag e' -> let+ x = expr (unbound, bound, e') in `dag x, sp
    | `prime e' -> let+ x = expr (unbound, bound, e') in `prime x, sp


    | _ -> failwith "Todo"

  (* let rec expr bound ((_e_, sp as e) : Hir1.expr) : t = 
    begin match _e_ with
    | #Ast.literal | #Hir1.comb | #Ast.lexical_variable as e_ ->
      Vocab.empty, bound, (e_, sp)
    | `var v | `stack v | `costack v as e_ -> 
      begin if Vocab.mem v bound then Vocab.empty
      else Vocab.singleton v sp end, bound, (e_, sp)
    
    | _ -> failwith "Todo"
  end *)

end
