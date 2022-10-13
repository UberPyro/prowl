open Batteries

open Span
(* open CST *)
open Ast

let string_content : CST.string_content -> string * Span.t = function
  | `SPACE (t, _) -> " ", t
  | `LF (t, _) -> "\n", t
  | `HT (t, _) -> "\t", t
  | `Str_content1 (t, s) -> s, t
  | `Esc_seq _ -> failwith "Todo escapes"

let rec word : CST.word -> <span: Span.t> Ast.word = function
  | `Int (l, i) -> ascr (Int (Int.of_string i)) l
  | `Char (_, None, _) -> failwith "Bad content"
  | `Char (_, Some (`Char_content1 (l, s)), _) -> ascr (Char s.[0]) l
  | `Char (_, Some _, _) -> failwith "Todo escapes"
  | `LBRACK_expr_RBRACK ((l, _), e, (r, _)) -> ascr (Quote (expr e)) (join l r)
  | `L_expr_r ((l, _), e, (r, _)) -> ascr (Group (expr e)) (join l r)
  | `LCURL_opt_COMMA_expr_rep_COMMA_expr_opt_COMMA_RCURL
    ((l, _), _, e, xs, _, (r, _)) -> 
    ascr (List (List.map expr (e :: List.map snd xs))) (join l r)
  | `LBRACK_RBRACK ((l, _), (r, _)) -> 
    ascr (Quote (ascr (Cat []) (join l r))) (join l r)
  | `L_r ((l, _), (r, _)) -> 
    ascr (Group (ascr (Cat []) (join l r))) (join l r)
  | `LCURL_RCURL ((l, _), (r, _)) -> ascr (List []) (join l r)
  | `Str ((l, _), scs, (r, _)) -> 
    let sc = List.map string_content scs in
    let loc = match sc with
      | [] -> join l r
      | [_, t] -> t
      | (_, l) :: xs -> join l @@ snd (List.last xs) in
    let s = 
      sc
      |> List.map fst
      |> String.concat ""
      |> String.to_list
      |> List.map (fun s -> ascr (Cat [ascr (Char s) loc]) loc) in
    ascr (List s) loc
  | `Id (t, x) -> ascr (Id x) t


  

  | _ -> failwith "todo"

and expr _ = failwith "todo"
