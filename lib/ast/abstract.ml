open Batteries

open Span
(* open CST *)
open Ast

let j (x, _) (y, _) = join x y

let express _w t = ascr (Cat [ascr _w t]) t

let string_content : CST.string_content -> string * Span.t = function
  | `SPACE (t, _) -> " ", t
  | `LF (t, _) -> "\n", t
  | `HT (t, _) -> "\t", t
  | `Str_content1 (t, s) -> s, t
  | `Esc_seq _ -> failwith "Todo escapes"

let bop : CST.bop -> string = function
  | `Op0 (_, s) -> s
  | `Op1 (_, s) -> s
  | `Op2 (_, s) -> s
  | `Op3 (_, s) -> s
  | `Op4 (_, s) -> s
  | `Op5 (_, s) -> s

let rec word : CST.word -> <span: Span.t> Ast.word = function
  | `Int (l, i) -> ascr (Int (Int.of_string i)) l
  | `Char (_, None, _) -> failwith "Bad content"
  | `Char (_, Some (`Char_content1 (l, s)), _) -> ascr (Char s.[0]) l
  | `Char (_, Some _, _) -> failwith "Todo escapes"
  | `LBRACK_expr_RBRACK (l, e, r) -> ascr (Quote (expr e)) (j l r)
  | `L_expr_r (l, e, r) -> ascr (Group (expr e)) (j l r)
  | `LCURL_opt_COMMA_expr_rep_COMMA_expr_opt_COMMA_RCURL
    (l, _, e, xs, _, r) -> 
    ascr (List (List.map expr (e :: List.map snd xs))) (j l r)
  | `LBRACK_RBRACK (l, r) -> 
    ascr (Quote (ascr (Cat []) (j l r))) (j l r)
  | `L_r (l, r) -> 
    ascr (Group (ascr (Cat []) (j l r))) (j l r)
  | `LCURL_RCURL (l, r) -> ascr (List []) (j l r)
  | `Str (l, scs, r) -> 
    let sc = List.map string_content scs in
    let loc = match sc with
      | [] -> j l r
      | [_, t] -> t
      | (_, z) :: xs -> join z @@ snd (List.last xs) in
    let s = 
      sc
      |> List.map fst
      |> String.concat ""
      |> String.to_list
      |> List.map (fun s -> express (Char s) loc) in
    ascr (List s) loc
  | `Id (t, x) -> ascr (Id x) t
  | `L_bop_expr_r (l, b, e, r) -> ascr (SectLeft (bop b, expr e)) (j l r)
  | `L_expr_bop_r (l, e, b, r) -> ascr (SectRight (expr e, bop b)) (j l r)
  | `L_bop_r (l, b, r) -> ascr (Id (bop b)) (j l r)
  | `L_uop_r (l, (_, s), r) -> ascr (Id s) (j l r)
  | `LBRACK_bop_expr_RBRACK (l, b, e, r) -> 
    ascr (Quote (express (SectLeft (bop b, expr e)) (j l r))) (j l r)
  | `LBRACK_expr_bop_RBRACK (l, e, b, r) -> 
    ascr (Quote (express (SectRight (expr e, bop b)) (j l r))) (j l r)
  | `LBRACK_bop_RBRACK (l, b, r) -> 
    ascr (Quote (express (Id (bop b)) (j l r))) (j l r)
  | `LBRACK_uop_RBRACK (l, (_, x), r) -> 
    ascr (Quote (express (Id x) (j l r))) (j l r)

and expr _ = failwith "todo"
