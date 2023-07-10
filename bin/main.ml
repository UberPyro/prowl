open! Batteries

open Prowl
open Cli
open Control

let () = match P.parse_argv op with
  | [] -> P.usage op ()
  | fname :: args -> 
    try check fname args with Semantics.Infer.InferError (sp, _, _, msg) -> 
      Printf.printf "%s\n%s\n" (Metadata.Span.show sp) msg;
      exit 2
