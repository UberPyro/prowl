open! Batteries

open Syntax
open Semantics
open Unify
open Ctx

open System
open Infer

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
      (* (Option.default "" (Option.map Token.show_token !tok)) *)
      "<disabled>"
      p.pos_lnum (p.pos_cnum - p.pos_bol)
    |> failwith

let parse_arg a = 
  let len = String.length a in
  if a.[0] = '"' && a.[len - 1] = '"'
  then Ast.String (String.sub a 1 (len - 2))
  else Ast.Int (String.to_int a)

let entry_logic ctx = 
  match find_rec_opt "main" ctx, find_rec_opt "main_nondet" ctx with
  | None, None -> failwith "This program has no main function!"
  | Some _, Some _ -> failwith "This program has multiple main functions!"
  | Some ((_, (main_in, main_out, _, e0)), _), None -> 
    set_det e0 (true, true);
    main_in, main_out
  | None, Some ((_, (main_in, main_out, _, _)), _) -> main_in, main_out

let check debug fname args = 
  let ast = parse (File.open_in fname) in
  let ctx = prog ast in
  let main_in, main_out = entry_logic ctx in
  let cs_in = List.fold_left begin fun acc -> function[@warning "-8"]
      | Ast.String _ -> push_str acc
      | Ast.Int _ -> push_int acc
    end (mk_init_costack ()) (List.map parse_arg args) in
  begin try
    main_in =?= cs_in; 
    main_out =?= mk_end_costack ()
  with Ucommon.UnifError msg -> failwith @@ "Error in main: " ^ msg end;
  if debug then begin
    let out = IO.output_string () in
    pretty out ctx;
    IO.close_out out |> print_endline
  end
