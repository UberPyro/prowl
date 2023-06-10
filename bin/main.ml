open! Batteries

open Prowl
open Cli
open Control

open Metadata
open Type

let () = match P.parse_argv op with
  | [] -> P.usage op ()
  | fname :: args -> 
    try check true fname args with
    | Infer.InferError (sp, ctx, msg) -> 
      print_endline "Type Error!";
      print_endline @@ Span.show sp;
      print_endline msg;
      print_endline @@ Infer.show_context ctx;
      exit 2;
