open! Batteries

open Metadata
open Types
open Syntax
open Type

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
  let ctx = Infer.top_stmts Infer.null_ctx ast in
  let (_, main_in, (main_out_low, _)), _ = Ouro.find_rec_opt "main" ctx
    |> Option.default_delayed begin fun () -> 
      failwith @@ Printf.sprintf "%s has no main function!" fname
    end in
  let cs_in = List.fold_left begin fun acc -> function[@warning "-8"]
      | Ast.String _ -> acc <: TLit TString
      | Ast.Int _ -> acc <: TLit TInt
    end (no_dc ()) (List.map parse_arg args) in
  try
    unify_dc main_in cs_in;
    unify_c main_out_low (no_c ());
    if debug then Infer.pretty_ctx ctx |> print_endline;
  with Ull.UnifError msg -> 
    raise @@ Infer.InferError (Span.dummy, ctx, "Error in main: " ^ msg)
