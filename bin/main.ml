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
    with Infer.InferError (sp, ctx, uctx, msg) -> 
      let out = IO.output_string () in
      Span.print_span out sp;
      IO.write_line out msg;
      if debug then begin
        Pretty.pretty_ctx out ctx;
        Pretty.pretty_uctx out uctx
      end;
      IO.close_out out |> print_endline;
      exit 2
