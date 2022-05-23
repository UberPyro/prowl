open Batteries
open OUnit2

let assert_output file output = 
  File.open_in ("test/" ^ file ^ ".prw")
  |> Gen.parse
  |> Interpret.(program init_st)
  |> LazyList.get
  |> begin function
    | Some (v, _) -> 
      List.rev_map Interpret.string_of_v v.stk
      |> String.concat "\n"
    | None -> "rejected"
  end
  |> assert_equal output

let test () = "suite" >::: List.map begin
    fun (n, o, f) -> n >:: fun _ -> assert_output o f
  end [
  "Lit 1", "fundamentals/lit1", "4\n5\nhi"
] |> run_test_tt_main
