open! Batteries

open Syntax
open Semantics
open Util

open System

let (>|<) pred_lex1 pred_lex2 = 
  let st = ref true in
  fun lexbuf -> 
    let p, l =
      if !st then pred_lex1
      else pred_lex2 in
    let tok = l lexbuf in
    if p tok then st := not !st;
    tok

let lex = 
  ((=) Parse.SPECIFY, Lex.token)
  >|< ((fun x -> 
    x = Parse.ASSIGN
    || x = Parse.IN
    || x = Parse.WITHIN
    || x = Parse.ARROW), Lex_type.token)

let parse ch = 
  let lexbuf = Lexing.from_channel ch in
  let tok = ref None in
  let lex2 buf = 
    let t = lex buf in
    tok := Some t; t in
  try Parse.prog lex2 lexbuf with
  | _ ->
    let p = lexbuf.lex_curr_p in
    Printf.sprintf
      "Unexpected Token %s at [%d,%d]"
      (Option.default "" (Option.map Token.show_token !tok))
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
