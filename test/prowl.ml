open! Batteries
open! Alcotest
open IO

open Prowl
open Control

let test_dir = Printf.sprintf "../../../test/%s/%s.pr"

let tests = [
  "basic", [
    "hi", "Hello, World!";
    "qmark", "1 1";
  ];
  "ex", [
    "append", "{1, 2, 3, 4, 5, 6}";
    "apdag", "{} {1, 2, 3, 4, 5, 6}
{1} {2, 3, 4, 5, 6}
{1, 2} {3, 4, 5, 6}
{1, 2, 3} {4, 5, 6}
{1, 2, 3, 4} {5, 6}
{1, 2, 3, 4, 5} {6}
{1, 2, 3, 4, 5, 6} {}";
    "append2", "{1, 2, 3, 4, 5, 6}";
    (* "apdag2", "{} {1, 2, 3, 4, 5, 6}
{1} {2, 3, 4, 5, 6}
{1, 2} {3, 4, 5, 6}
{1, 2, 3} {4, 5, 6}
{1, 2, 3, 4} {5, 6}
{1, 2, 3, 4, 5} {6}
{1, 2, 3, 4, 5, 6} {}"; *)
    "is-even", "<Fake> ";
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
      test group fn (out ^ "\n")
      |> test_case fn `Quick

let () = run "Prowl Integration Tests" suite
