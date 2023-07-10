open! Batteries

open Syntax
open Semantics
open Util

open System

let parse ch = 
  let lexbuf = Lexing.from_channel ch in
  try Parse.prog Lex.token lexbuf with
  | _ ->
    let p = lexbuf.lex_curr_p in
    Printf.sprintf
      "Unexpected Token at [%d,%d]"
      p.pos_lnum (p.pos_cnum - p.pos_bol)
    |> failwith

let parse_arg a = 
  let len = String.length a in
  if a.[0] = '"' && a.[len - 1] = '"'
  then Ast.String (String.sub a 1 (len - 2))
  else Ast.Int (String.to_int a)

let check debug fname args = 
  let ast = parse (File.open_in fname) in
  let ctx = Infer.prog ast in
  let (_, main_in, main_out), _ = 
    Ouro.find_rec_opt "main" ctx
    |> Option.default_delayed begin fun () -> 
      failwith @@ Printf.sprintf "%s has no main function!" fname
    end in
  let cs_in = List.fold_left begin fun acc -> function[@warning "-8"]
      | Ast.String _ -> Lit String @> acc
      | Ast.Int _ -> Lit Int @> acc
    end (mk_init_costack ()) (List.map parse_arg args) in
  begin try
    main_in =?= cs_in; 
    main_out =?= mk_end_costack ()
  with Ull.UnifError msg -> failwith @@ "Error in main: " ^ msg end;
  if debug then begin
    let out = IO.output_string () in
    Pretty.pretty_ctx out ctx;
    IO.close_out out |> print_endline
  end
