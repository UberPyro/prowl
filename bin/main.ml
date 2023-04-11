open! Batteries
open Uref

open Prowl
open Cli
open Syntax
open Run

open Eval_mir

let parse ch = 
  let lexbuf = Lexing.from_channel ch in
  try Parse.prog Lex.token lexbuf with
  | _ ->
    let p = lexbuf.lex_curr_p in
    Printf.sprintf
      "Unexpected Token at [%d,%d]"
      p.pos_lnum (p.pos_cnum - p.pos_bol)
    |> failwith

let () = match P.parse_argv op with
  | [] -> P.usage op ()
  | fname :: args -> 
    let ast = parse (File.open_in fname) in
    expr
      (init ())
      (Ast.desugar ast)
      (List.map (fun x -> try 
        uref @@ `int (String.to_int x) 
      with Failure _ -> uref @@ `str x) args
      |> List.rev |> fun s -> Real s)
    |> Print_eval_mir.print
