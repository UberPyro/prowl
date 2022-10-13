open Batteries

(* open CST *)
open Ast

let (* rec *) word = function
  | `Int (l, i) -> ascr (Int (Int.of_string i)) l
  | `Char (_, None, _) -> failwith "Bad content"
  | `Char (_, Some _, _) -> failwith "Todo"
