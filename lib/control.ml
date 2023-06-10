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

let check fname args = 
  let ast = parse (File.open_in fname) in
  let ctx = Infer.top_stmts Infer.null_ctx ast in
  let (_, main_in, _main_out), _ = Ouro.find_rec_opt "main" ctx
    |> Option.default_delayed begin fun () -> 
      failwith @@ Printf.sprintf "%s has no main function!" fname
    end in
  let cs_in = List.fold_left begin fun acc -> function[@warning "-8"]
      | Ast.String _ -> acc <: TLit TString
      | Ast.Int _ -> acc <: TLit TInt
    end (mk_dc ()) (List.map parse_arg args) in
  unify_dc main_in cs_in
