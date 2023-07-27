open! Batteries

open Prowl
open Cli
open Control

open Metadata
open Semantics

let debug = true

let () = match P.parse_argv op with
  | [] -> P.usage op ()
  | fname :: args -> 
    try check debug fname args
    with Infer.InferError (sp, ctx, msg) -> 
      let out = IO.output_string () in
      Span.print_span out sp;
      Unify.Ucommon.pp_err out msg;
      if debug then Ctx.pretty out ctx;
      IO.close_out out |> print_endline;
      exit 2
