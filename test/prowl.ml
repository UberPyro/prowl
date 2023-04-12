open! Batteries
open! Alcotest
open IO

open Prowl
open Control

let test_dir = Printf.sprintf "../../../test/%s/%s.pr"

let tests = [
  "basic", [
    "hi", "Hello, World!\n"
  ]
]

let test group file output () = 
  let txt = output_string () in
  exec (test_dir group file) []
  |> Run.Print_eval_mir.print txt;
  close_out txt
  |> check string "outputs match" output

let suite = 
  tests |> List.map @@ fun (group, lst) -> 
    group, lst |> List.map @@ fun (fn, out) -> 
      test group fn out
      |> test_case fn `Quick

let () = run "Prowl Integration Tests" suite
