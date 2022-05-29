open Batteries
open Alcotest

open Lib
open Util

let tests = [
  "fundamentals", [
    "lit1", "4\n5\nhi";
    "arith1", "7";
    "arith2", "5";
  ]; 
  "bindings", [
    "as1", "11";
    "let1", "6";
    "let-func", "25";
    "let-as", "2";
    "let-sect", "4";
    "compose", "3";
    "sect1", "1";
    "sect-full", "20"; 
    "sect-left", "2"; 
    "sect-right", "1";
  ]; 
  "patterns", [
    "cat", "9";
    "left", "13";
    "right", "0";
    "pair", "4";
    "capture-direct", "7";
    "capture-indirect", "7";
    "capture-fun", "2";
    "nest-capture-pair", "2";
    "nest-either-pair", "1";
    "long-pair", "5";
    "const-int", "1";
    "const-int-reject", "rejected";
    "const-str", "0";
    "pair-func", "2";
    "eith-func", "1";
    "bin-func", "2";
    "stack", "3\n4\n5";
    "stack-rejected-low", "rejected";
    "stack-rejected-high", "rejected";
  ]; 
  "flow", [
    "cat", "5";
    "alt", "0";
    "alt-handle", "1";
    "alt-rejected", "rejected";
    "alt-greedy", "0";
    "case", "2";
    "intersect", "2";
    "n-times", "7\n23";
    "opt", "3";
    "opt-handle", "2";
    "star", "6";
    "star-greedy", "1";
    "plus", "3";
    "plus-reject", "rejected";
    "alt-cut-accepted", "0";
    "alt-cut", "rejected";
    "alt-cut-handle", "1";
    "case-rel", "8";
    "case-rel2", "11";
    "case-cut", "rejected";
    "star-cut", "rejected";
    "star-rel", "5";
    "inversion", "6";
    "inversion-rejected", "rejected";
    "noncap-accept", "6";
    "noncap-reject", "rejected"; 
    "atomic-accept", "1";
    "atomic-reject", "rejected";
  ]; 
  "combinators", [
    "simple", "44";
    "compound", "3";
  ];
  "modules", [
    "access", "3";
    "open", "7";
    (* "recursion", "720"; *)
  ];
  "data", [
    "rev", "1\n3\n5\n7\n9";
    "map", "2\n4\n6";
    (* "filter", "2"; *)
    "cat", "1\n2\n3\n4";
    "cat-rev", "1\n2\n4\n3";
    (* "flatten", "1\n2\n3\n4"; *)  (* head is getting elems *)
  ]
]

open Interpret
module L = Eval.LazySearch
open Run(L)

let run_file fname = 
  File.open_in ("test/" ^ fname ^ ".prw")
  |> Gen.parse |> fun ast -> try
    Interpret.S.init
    |> program (Build.endow "std" ast)
    |> L.unsafe_cut
    |> Interpret.S.s
    |> List.rev_map V.show
    |> String.concat "\n" with
  | L.Rejected -> "rejected"

let check_file group file output () = 
  run_file (Printf.sprintf "%s/%s" group file)
  |> check string "outputs match" output

let () = 
  begin
    let+ group, lst = tests in
    group, let+ fn, out = lst in
    check_file group fn out
    |> test_case fn `Quick
  end
  |> run "Prowl Integration Tests"
