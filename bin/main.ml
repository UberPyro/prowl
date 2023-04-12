open! Batteries

open Prowl
open Cli
open Control

let () = match P.parse_argv op with
  | [] -> P.usage op ()
  | fname :: args -> 
    exec fname args
    |> Run.Print_eval_mir.print stdout
